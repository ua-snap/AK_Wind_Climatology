# Script is for generating gam fits and respective plots for two
#   ASOS stations in AK
library(mgcv)
library(lubridate)
library(dplyr)
library(mgcViz)

# directories
workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")

asos_path <- file.path(asos_select_adj_dir, "PAFA_qmap.Rds")
asos_hourly_station <- readRDS(asos_path)

asos_hourly_station$month <- month(asos_hourly_station$date)
asos_hourly_station$day <- day(asos_hourly_station$date)
asos_hourly_station$valid <- ymd_hms(paste0(asos_hourly_station$valid, ":00"))

asos_june <- asos_hourly_station %>% filter(month == 6, date > "2000-01-01" & date <"2011-01-01") %>%
  mutate(hrs = floor(as.numeric(difftime(valid, floor_date(valid, unit = "month"), units = "hours")))) %>%
  group_by(hrs) %>% summarise(avg_sped = mean(sped_adj))



fit <- gam(avg_sped ~ s(hrs), data = asos_june)

fit <- getViz(fit)

o <- plot(sm(fit, 1))
o + l_fitLine(colour = "red") + l_rug(mapping = aes(y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_bw() + 
  ggtitle("PAFA Wind Speeds, 1980 to 2015") + 
  theme(panel.grid = element_blank()) + 
  xlab("Hours Since 6/1 00:00") + ylab("s(Hours Since 6/1 00:00, 5.36)")




asos_path <- file.path(asos_select_adj_dir, "PABA_qmap.Rds")
asos_hourly_station <- readRDS(asos_path)

asos_hourly_station$month <- month(asos_hourly_station$date)
asos_hourly_station$day <- day(asos_hourly_station$date)
asos_hourly_station$valid <- ymd_hms(paste0(asos_hourly_station$valid, ":00"))

asos_june <- asos_hourly_station %>% filter(month == 6, date > "2000-01-01" & date <"2011-01-01") %>%
  mutate(hrs = floor(as.numeric(difftime(valid, floor_date(valid, unit = "month"), units = "hours")))) %>%
  group_by(hrs) %>% summarise(avg_sped = mean(sped_adj))



fit <- gam(avg_sped ~ s(hrs), data = asos_june)

fit <- getViz(fit)

o <- plot(sm(fit, 1))
o + l_fitLine(colour = "red") + l_rug(mapping = aes(y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_bw() + 
  ggtitle("PABA Wind Speeds, 1980 to 2015") + 
  theme(panel.grid = element_blank()) + 
  xlab("Hours Since 6/1 00:00") + ylab("s(Hours Since 6/1 00:00, 8.91)")
