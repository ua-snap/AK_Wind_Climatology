# Script Summary
#
# Summarize select/adj data by day and by month
#
# Output files:
#   /data/AK_ASOS_daily_select_adj_19800101_to_20150101.Rds
#   /data/AK_ASOS_monthly_select_adj_19800101_to_20150101.Rds



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(lubridate)

workdir <- getwd()
datadir <- file.path(workdir, "data")
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
# select station ids
select_stations_path <- file.path(datadir, "AK_ASOS_select_stations.Rds")
select_stations <- readRDS(select_stations_path)
stids <- select_stations$stid

#------------------------------------------------------------------------------

#-- Summarize By Day and Month ------------------------------------------------
{# daily data frame
asos_daily <- data.frame(stid = character(), 
                         date = ymd(),
                         obs_prop = double(), 
                         avg_sped = double(),
                         avg_sped_adj = double(),
                         stringsAsFactors = FALSE)
# monthly data frame
asos_monthly <- data.frame(stid = character(),
                           ym_date = ymd(),
                           obs_prop = double(),
                           avg_sped = double(),
                           avg_sped_adj = double(),
                           stringsAsFactors = FALSE)

# loop through selected/adjusted data and summarize by both day and month
for(i in seq_along(stids)){
  asos_station_path <- file.path(asos_adj_dir, paste0(stids[i], ".Rds"))
  asos_station <- readRDS(asos_station_path)
  # Daily summary
  # count number of hourly observations/24
  asos_summary <- asos_station %>% 
    group_by(stid, date) %>%
    summarise(obs_prop = n()/24, 
              avg_sped = mean(sped),
              avg_sped_adj = mean(sped_adj)) %>%
    select(stid, date, obs_prop, avg_sped, avg_sped_adj)
  # append to daily
  asos_daily <- bind_rows(asos_daily, asos_summary)
  
  # monthly summary
  # count number of hourly observations/24
  asos_summary <- asos_station %>% 
    mutate(ym_date = ymd(paste(year(date), 
                               sprintf("%02d", month(date)), 
                               "01", sep = "-"))) %>%
    group_by(stid, ym_date) %>%
    summarise(obs_prop = n()/(days_in_month(unique(ym_date)) * 24), 
              avg_sped = mean(sped),
              avg_sped_adj = mean(sped_adj)) %>%
    select(stid, ym_date, obs_prop, avg_sped, avg_sped_adj)
  # append to daily
  asos_monthly <- bind_rows(asos_monthly, asos_summary)
}

asos_daily_path <- file.path(datadir, 
                             "AK_ASOS_daily_select_adj_19800101_to_20150101.Rds")
saveRDS(asos_daily, asos_daily_path)
asos_monthly_path <- file.path(datadir, 
                             "AK_ASOS_monthly_select_adj_19800101_to_20150101.Rds")
saveRDS(asos_monthly, asos_monthly_path)}

#------------------------------------------------------------------------------
