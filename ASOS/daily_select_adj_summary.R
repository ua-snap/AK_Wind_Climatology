# Script summary
#
# Summarize Adj Speeds By Day
#   only summarizing adjusted data of selected stations 
#
# Output files:
#   /data/AK_ASOS_daily_select_adj_19700101_to_20190528.Rds



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(lubridate)

workdir <- getwd()
datadir <- file.path(workdir, "data")
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
# select station ids
select_stations_path <- file.path(datadir, "all_stids.Rds")
select_stations <- readRDS(select_stations_path)
stids <- selet_stations$stid

#------------------------------------------------------------------------------

#-- Summarize By Day ----------------------------------------------------------
# daily data frame
asos_daily <- data.frame(stid = character(), 
                         date = ymd(),
                         obs = double(), 
                         avg_sped = double(),
                         stringsAsFactors = FALSE)
# Loop through QC station data
for(i in seq_along(stids)){
  asos_station_path <- file.path(asos_adj_dir, paste0(stids[i], ".Rds"))
  asos_station <- readRDS(asos_station_path)
  # count number of hourly observations/24
  asos_station <- asos_station %>% 
    group_by(stid, date) %>%
    summarise(obs_prop = n()/24, 
              avg_sped = mean(sped_adj)) %>%
    select(stid, date, obs, avg_sped)
  # append to daily
  asos_daily <- bind_rows(asos_daily, asos_station)
}
asos_daily_path <- file.path(datadir, 
                             "AK_ASOS_daily_select_adj_19800101_to_20150101.Rds")
saveRDS(asos_daily, asos_daily_path)

#------------------------------------------------------------------------------
