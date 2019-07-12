# Script summary
#
# Select Stations & Data
#   Select the stations with enough data for use in 
#   climatology from 1980-2015, filter data to this period
#  Group into tiers - significant periods of missing data and not
#   
# AK Map of Selected Stations
#   Indicate by color Tier 1 vs Tier 2
# 
# Output files:
#   /data/AK_ASOS_select_stations.Rds
#   /data/AK_ASOS_stations_meta_succ.Rds
#   /figures/AK_ASOS_select_locations.png



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
# stids in station meta data
meta_stids <- unique(stations$stid)

#------------------------------------------------------------------------------

#-- Select Stations -----------------------------------------------------------
# select data from chosen period (determined by examination of heatmap)
start_date <- ymd("1980-01-01")
end_date <- ymd("2015-01-01")

# Stids in raw for records
raw_stids <- unique(asos_daily$stid)

asos_daily <- asos_daily %>% filter(date >= start_date & 
                                       date <= end_date)

# stids remaining after filtering to period
period_stids <- unique(asos_daily$stid)

# filter out sites that don't have observations spanning the period
#   based on proportion of "successful" days, i.e. days with 
#   some proportion of observations, p_succ
n <- 4
p_succ <- n/24
p_tot <- 0.75
# tolerance for missing days during target period
miss_cut <- 2000
# number of potential sampling days
n_days <- as.numeric(end_date - start_date)
asos_daily <- asos_daily %>% 
  mutate(succ = if_else(obs > p_succ, 1, 0))
# determine percentage of successful sampling days and 
#   how many days were missing
stations <- asos_daily %>% 
  group_by(stid) %>% 
  summarise(prop_succ = sum(succ)/n_days,
            miss_days = n_days - n()) %>%
  inner_join(stations, by = "stid")
select_stations <- stations %>% 
  filter(prop_succ > p_tot) %>%
  # if more than 1825 missing days (5 years), call tier two
  mutate(tier = factor(if_else(miss_days >= miss_cut, 2, 1)))

# stids remaining after filtering based on available data
select_stids <- unique(select_stations$stid)

# save both meta data frames, 
select_stations_path <- file.path(datadir, "AK_ASOS_select_stations.Rds")
saveRDS(select_stations, select_stations_path)
# Stations df for stations found in Raw data with data from target period
all_stations_path <- file.path(datadir, "AK_ASOS_stations_meta_succ.Rds")
saveRDS(stations, all_stations_path)

# save groups of stids and other criteria for later reporting
stids_list <- list(meta = meta_stids, 
                   raw = raw_stids,
                   period = period_stids, 
                   select = select_stids,
                   min_obs_day_succ = n,
                   p_succ = p_succ,
                   p_tot = p_tot,
                   miss_cut = miss_cut,
                   n_days = n_days)
stids_list_path <- file.path(datadir, "stids_list.Rds")
saveRDS(stids_list, stids_list_path)

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

# labelling: want to "repel" some labels and simply nudge others:
repel_stids <- matrix(c("PAEL", 0.2, 0.2,
                        "PAGS", 0.2, 0.2,
                        "PAJN", 0.2, 0.2,
                        "PASI", 0.2, 0.2,
                        "PAPG", 0.2, 0.2,
                        "PAWG", 0.2, 0.2,
                        "PAKT", 0.2, 0.2,
                        "PANT", 0.2, 0.2,
                        "PADQ", 0.2, 0.2,
                        "PAHO", 0.2, 0.2,
                        "PAHN", 0.2, 0.2,
                        "PAWD", 0.2, 0.2,
                        "PAMD", 0.2, 0.2,
                        "PAVD", 0.2, 0.2,
                        "PACV", 0.2, 0.2,
                        "PANC", 0.2, 0.2,
                        "PAMR", 0.2, 0.2,
                        "PAAQ", 0.2, 0.2,
                        "PABI", 0.2, 0.2,
                        "PANN", 0.2, 0.2,
                        "PAFA", 0.2, 0.2,
                        "PAFB", 0.2, 0.2,
                        "PAEI", 0.2, 0.2,
                        "PAPR", 0.2, 0.2,
                        "PABT", 0.2, 0.2,
                        "PAMX", 0.2, 0.2,
                        "PAOR", 0.2, 0.2,
                        "PABA", 0.2, 0.2, 
                        "PASC", 0.2, 0.2,
                        "PABR", 0.2, 0.2,
                        "PALU", -1000, 50,
                        "PAOT", 0.2, 0.2,
                        "PASH", 0.2, 0.2,
                        "PATC", 0.2, 0.2,
                        "PAOM", 0.2, 0.2, 
                        "PAUN", 0.2, 0.2,
                        "PACZ", 0.2, 0.2,
                        "PAEH", 0.2, 0.2,
                        "PASM", 0.2, 0.2,
                        "PABE", 0.2, 0.2,
                        "PADI", 0.2, 0.2,
                        "PAKN", 0.2, 0.2,
                        "PAPH", 0.2, 0.2, 
                        "PASO", 0.2, 0.2,
                        "PACD", 0.2, 0.2,
                        "PADU", 0.2, 0.2,
                        "PASN", 0.2, 0.2,
                        "PADK", 0.2, 0.2,
                        "PASY", 0.2, 0.2),
                      ncol = 3, byrow = TRUE)
repel_stids <- as.data.frame(repel_stids, 
                             stringsAsFactors = FALSE) %>%
  rename(stid = V1, nudge_x = V2, nudge_y = V3) %>%
  mutate(nudge_x = as.numeric(nudge_x),
         nudge_y = as.numeric(nudge_y))

# coordinates for "repel" labels
repel_coords <- repel_stids %>% 
  inner_join(select_stations, by = "stid") %>% 
  select(stid, lon, lat, nudge_x, nudge_y) 
repel_coords_sf <- st_as_sf(repel_coords[, 1:3], coords = c("lon", "lat"), 
                         remove = FALSE, crs = 4326)
repel_coords_sf <- st_transform(repel_coords_sf, 26935)
repel_coords <- matrix(unlist(repel_coords_sf$geometry), 
                       ncol = 2, byrow = TRUE) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  bind_cols(repel_coords) %>%
  mutate(V1 = as.numeric(V1),
         V2 = as.numeric(V2))

# plot
theme_set(theme_bw())
p <- ggplot(data = ak_sf) + geom_sf(fill = "cornsilk") +
  ggtitle("AK ASOS Stations Selected For Climatology", 
          subtitle = paste0("Stations having fewer than ", 100 * (1 - p_tot), 
                            "% of days of fewer than ", n, 
                            " hourly recordings, ", year(start_date), " to ",
                            year(end_date), " (", dim(select_stations)[1], 
                            " stations)
  Stations missing more than ", miss_cut, 
                            " days colored blue")) +
  xlab("Lng") + ylab("Lat") +
  geom_sf(data = coords, shape = 21, fill = "darkred") + 
  geom_sf(data = coords_2, shape = 21, size = 2, fill = "cyan")

  geom_text_repel(data = repel_coords, 
                  aes(x = V1, y = V2, label = stid), 
                  size = 3, fontface = "bold", 
                  min.segment.length = 0.1,
                  nudge_x = repel_coords$nudge_x,
                  nudge_y = repel_coords$nudge_y) + 
  
fig_path <- file.path(figdir, "AK_ASOS_select_locations.png")
ggsave(fig_path, plot = p, device = "png", width = 8, height = 6)



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