# This script is for saving the metadata for the stations to be used 
#   in the wind climatology generation, and for creation of the 
#   visualization map of AK with the selected ASOS stations 

library(ggplot2)
library(data.table)
library(sf)
library(USAboundaries)
library(lubridate)
library(dplyr)

workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures")
# daily data
asos_daily_path <- file.path(datadir, "AK_ASOS_daily_allsites_wind_19700101_to_20190528.csv")
asos_daily <- fread(asos_daily_path)

asos_station_path <- file.path(datadir, "AK_ASOS_stations.csv")
stations <- read.csv(asos_station_path, stringsAsFactors = FALSE)

# select data from appropriate period
#start_date <- as.Date("1985-01-01", origin = "1970-01-01")
start_date <- ymd("1980-01-01")

#end_date <- start_date + years(30)
end_date <- ymd("2015-01-01")
asos_select_dates <- asos_daily %>% filter(date >= start_date & 
                                     date <= end_date)


# filter out sites that don't have observations spanning the period
#   based on proportion of "successful" days, i.e. days with 
#   some proportion of observations, p_succ
n <- 4
p_succ <- n/24
p_tot <- 0.75
# number of potential sampling days
n_days <- as.numeric(end_date - start_date)
asos_select <- asos_select_dates %>% 
  mutate(one = 1, succ = if_else(obs > p_succ, 1, 0))
asos_select <- asos_select %>% group_by(stid) %>% 
  summarise(prop_succ = sum(succ)/n_days)
select_stations <- inner_join(asos_select, stations, by = "stid") %>%
  filter(prop_succ > p_tot)

# save select stations metadata
select_stations_path <- file.path(datadir, "select_stations.Rds")
saveRDS(select_stations, file = select_stations_path)


# load select stations (if running separately)
select_stations_path <- file.path(datadir, "select_stations.Rds")
select_stations <- readRDS(select_stations_path)
stids <- select_stations$stid

alaska <- us_states(states = "AK", resolution = "high")
ak <- st_transform(alaska, 26935)
theme_set(theme_bw())

sites <- as.data.frame(select_stations[, c("lon", "lat")])
sites <- st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)
sites <- st_transform(sites, 26935)

p <- ggplot(data = ak) + geom_sf(fill = "cornsilk") +
  geom_sf(data = sites, shape = 21, fill = "darkred") + 
  ggtitle("AK ASOS Stations Selected For Climatology", 
          subtitle = paste0("Stations having fewer than ", 100 * (1 - p_tot), 
                            "% of days of fewer than ", n, 
                            " hourly recordings 
     between ", start_date, " and ",
                            end_date, " (", dim(select_stations)[1], 
                            " stations)"))

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