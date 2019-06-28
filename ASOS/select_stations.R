# This script is for saving the metadata for the stations to be used 
#   in the wind climatology generation, and for creation of the 
#   visualization map of AK with the selected ASOS stations 

# Script summary
#
# Select Stations & Data
#   Select the stations with enough data for use in 
#   climatology from 1980-2015, filter data to this period
#  Group into tiers - significant periods of missing data and not
#   
# AK Map Selected Stations
#   Indicate by color Tier 1 vs Tier 2
# 
# Output files:
#   /data/AK_ASOS_daily_all_stations_19700101_to_20190528.Rds
#   /data/AK_ASOS_select_stations_19800101_to_20150101/"stid".Rds



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(sf)
library(USAboundaries)

workdir <- getwd()
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures")
asos_qc_dir <- file.path(datadir, "AK_ASOS_stations_qc")
# daily data
asos_daily_path <- file.path(datadir, 
                     "AK_ASOS_daily_all_stations_19700101_to_20190528.Rds")
asos_daily <- readRDS(asos_daily_path)
# station meta data
asos_meta_path <- file.path(datadir, "AK_ASOS_stations_meta.csv")
stations <- read.csv(asos_meta_path, stringsAsFactors = FALSE)

#------------------------------------------------------------------------------

#-- Select Stations -----------------------------------------------------------
# select data from chosen period (determined by examination of heatmap)
start_date <- ymd("1980-01-01")
end_date <- ymd("2015-01-01")
asos_select <- asos_daily %>% filter(date >= start_date & 
                                       date <= end_date)

# filter out sites that don't have observations spanning the period
#   based on proportion of "successful" days, i.e. days with 
#   some proportion of observations, p_succ
n <- 4
p_succ <- n/24
p_tot <- 0.75
# tolerance for missing days
miss_cut <- 2000
# number of potential sampling days
n_days <- as.numeric(end_date - start_date)
asos_select <- asos_select %>% 
  mutate(one = 1, succ = if_else(obs > p_succ, 1, 0))
# determine percentage of successful sampling days and 
#   how many days were missing
stations <- asos_select %>% 
  group_by(stid) %>% 
  summarise(prop_succ = sum(succ)/n_days,
            miss_days = n_days - n()) %>%
  inner_join(stations, by = "stid")
select_stations <- stations %>% 
  filter(prop_succ > p_tot) %>%
  # if more than 1825 missing days (5 years), call tier two
  mutate(tier = factor(if_else(miss_days >= miss_cut, 2, 1)))

# save both meta data frames, 
select_stations_path <- file.path(datadir, "AK_ASOS_select_stations.Rds")
saveRDS(select_stations, select_stations_path)
all_stations_path <- file.path(datadir, "AK_ASOS_stations_meta_succ")
saveRDS(stations, all_stations_path)

#------------------------------------------------------------------------------

#-- AK Map of Stations --------------------------------------------------------
# load select stations (if running separately)
select_stations <- readRDS(select_stations_path)
stids <- select_stations$stid
# alaska sf object
alaska <- us_states(states = "AK", resolution = "high")
ak_sf <- st_transform(alaska, 26935)
# station coordinates as sf object
coords <- as.data.frame(select_stations[, c("stid", "lon", "lat")])
coords <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
coords <- st_transform(coords, 26935)
# tier 2 stations coordinates as sf
coords_2 <- select_stations %>% 
  filter(tier == 2) %>% 
  select(lon, lat) %>%
  as.data.frame()
coords_2 <- st_as_sf(coords_2, coords = c("lon", "lat"), crs = 4326)
coords_2 <- st_transform(coords_2, 26935)
# slightly adjusted coordinates for labels
lab_coords <- select_stations %>% 
  mutate(lat = lat + 0.3) %>%
  select(stid, lon, lat) %>%
  as.data.frame()
lab_coords <- st_as_sf(lab_coords, coords = c("lon", "lat"), crs = 4326)
lab_coords <- st_transform(lab_coords, 26935)
# plot
theme_set(theme_bw())
p <- ggplot(data = ak_sf) + geom_sf(fill = "cornsilk") +
  geom_sf(data = coords, shape = 21, fill = "darkred") + 
  geom_sf(data = coords_2, shape = 21, size = 2, fill = "cyan") +
  ggtitle("AK ASOS Stations Selected For Climatology", 
          subtitle = paste0("Stations having fewer than ", 100 * (1 - p_tot), 
                            "% of days of fewer than ", n, 
                            " hourly recordings, ", year(start_date), " to ",
                            year(end_date), " (", dim(select_stations)[1], 
                            " stations)
  Stations missing more than ", miss_cut, 
                            " days colored blue")) +
  xlab("Lng") + ylab("Lat")
p + geom_sf_label(data = lab_coords, aes(label = stid), 
                  size = 2, 
                  label.padding = unit(0.05, "lines"))

fig_path <- file.path(figdir, "AK_ASOS_avail_locations.png")
ggsave(fig_path, plot = p, device = "png", width = 8, height = 6)




# Wonky stations
#   figure of select stations with gaps/odd behavior in time series
# select stations
select_stations_path <- file.path(datadir, "select_stations.Rds")
select_stations <- readRDS(select_stations_path)

sites <- as.data.frame(select_stations[, c("lon", "lat")])
sites <- st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)
sites <- st_transform(sites, 26935)
# wonky stations
stids_w <- c("PADT", "PADU", "PAEC", "PAFE", "PAGS", 
             "PAIL", "PASD", "PASH", "PASM", "PATK")
sites_w <- select_stations %>% 
  filter(stid %in% stids_w) %>% 
  select(lon, lat) %>%
  as.data.frame()

sites_w <- st_as_sf(sites_w, coords = c("lon", "lat"), crs = 4326)
sites_w <- st_transform(sites_w, 26935)

p <- ggplot(data = ak) + geom_sf(fill = "cornsilk") +
  geom_sf(data = sites, shape = 21, fill = "darkred") + 
  geom_sf(data = sites_w, shape = 21, size = 2, fill = "cyan") +
  #geom_text(aes(label = stids_w)) +
  ggtitle("AK ASOS Stations Selected For Climatology")




# attempting to label stations
#   figure of select stations with gaps/odd behavior in time series
# select stations
select_stations_path <- file.path(datadir, "select_stations.Rds")
select_stations <- readRDS(select_stations_path)

sites <- as.data.frame(select_stations[, c("stid", "lon", "lat")])
sites <- st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)
sites <- st_transform(sites, 26935)
# wonky stations
stids_w <- c("PADT", "PADU", "PAEC", "PAFE", "PAGS", 
             "PAIL", "PASD", "PASH", "PASM", "PATK")
sites_w <- select_stations %>% 
  filter(stid %in% stids_w) %>% 
  select(lon, lat) %>%
  as.data.frame()

sites_w <- st_as_sf(sites_w, coords = c("lon", "lat"), crs = 4326)
sites_w <- st_transform(sites_w, 26935)

p <- ggplot(data = ak) + geom_sf(fill = "cornsilk") +
  geom_sf(data = sites, shape = 21, fill = "darkred") + 
  #geom_sf(data = sites_w, shape = 21, size = 2, fill = "cyan") +
  #geom_text(aes(label = stids_w)) +
  ggtitle("AK ASOS Stations Selected For Climatology")

p + geom_sf_label(data = sites, aes(label = stid), 
                 size = 2, 
                 label.padding = unit(0.1, "lines"))



# Determining which sites belong to which region
# north and west
n_stids <- c("PALU", "PABR", "PASC", "PABA", "PAOT",
             "PASH", "PATC", "PAOM", "PAUN")
# interior
int_stids <- c("PABT", "PAPR", "PFYU", "PAIM", "PATA",
               "PAGA", "PARY", "PAMH", "PAFA", "PAFB",
               "PAEI", "PABI", "PAOR", "PAMC", "PATL",
               "PANN")
# south central
sc_stids <- c("PADT", "PAEC", "PAMX", "PAGK", "PATK", 
              "PASW", "PANC", "PAAQ", "PAEN", "PAWD",
              "PAHO", "PAMR", "PAED", "PAVD", "PACV",
              "PAMD")
# southwest
sw_stids <- c("PADQ", "PAIL", "PASV", "PANI", "PASM",
              "PACZ", "PABE", "PAEH", "PADL", "PAKN",
              "PAPH", "PASD", "PACD", "PADU", "PASN", 
              "PADK", "PASY")
#southeast
se_stids <- c("PAYA", "PAGY", "PAJN", "PAEL", "PAGS",
              "PASI", "PAFE", "PAPG", "PAWG", "PANT", 
              "PAKT")
# make df and save
regions <- c(rep("North/Northwest", length(n_stids)), 
             rep("Interior", length(int_stids)),
             rep("Southwest", length(sw_stids)),
             rep("Southcentral", length(sc_stids)), 
             rep("Southeast", length(se_stids)))
select_stids_regions <- data.frame(stid = c(n_stids, int_stids, sc_stids, sw_stids, se_stids))