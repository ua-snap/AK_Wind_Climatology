# This script is for visualizing the abundance of successful hourly
#   wind speed and direction recordings at the AK ASOS stations

library(data.table)
library(dplyr)
library(ggplot2)
library(viridis)
library(lubridate)
library(ggExtra)
library(tidyr)

workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures")

# asos_path <- file.path(datadir, "AK_ASOS_allsites_wind_19700101_to_20190528.txt")
# asos <- fread(asos_path)

asos_daily_path <- file.path(datadir, "AK_ASOS_daily_allsites_wind_19700101_to_20190528.csv")
asos_daily <- fread(asos_daily_path)

asos_station_path <- file.path(datadir, "AK_ASOS_stations.csv")
stations <- read.csv(asos_station_path, stringsAsFactors = FALSE)

# break stations in pre/post 1970 begin date
pre_1970 <- which(stations$begints <= as.Date(0, origin = "1970-01-01"))
post_1970 <- setdiff(1:183, pre_1970)
station_temp_1 <- stations[pre_1970, ]
station_temp_1 <- station_temp_1[order(station_temp_1$stid), ]
station_temp_2 <- stations[post_1970, ]
station_temp_2 <- station_temp_2[order(station_temp_2$begints, 
                                       station_temp_2$stid), ]
stations <- rbind(station_temp_1, station_temp_2)
station_levels <- stations$stid


# asos_temp <- asos_daily %>% filter(station %in% c("PADK", "PALP"))

asos_daily$station <- factor(asos_daily$station, levels = station_levels)

p <-ggplot(asos_daily, aes(date, station, fill = obs))+
  geom_tile() + 
  scale_fill_viridis(name="Proportion of Observations (# Obs / 24)")
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= "Observations/Day - AK ASOS, All Stations",
             x="Date", y="Station")
# date breaks
date_breaks <- as.Date(c("1970-01-01", "1980-01-01", "1990-01-01", 
                         "2000-01-01", "2010-01-01", "2020-01-01"))
date_labels <- c("1970", "1980", "1990", "2000", "2010", "2020")
p <- p + scale_x_date(breaks = date_breaks, labels = date_labels)
p <-p + theme(legend.position = "bottom")+
  theme(plot.title = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 5)) +
  theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 10)) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 8)) +
  removeGrid() # ggExtra

# save plot
figure_path <- file.path(figdir, "station_observations.png")
ggsave(figure_path, plot = p, device = "png",
       width = 6, height = 10, units = "in")

