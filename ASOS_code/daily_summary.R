# Script summary
# 
# Summarize wind speeds by day, using quality-controlled ASOS data
#
# Output files:
#   /data/AK_ASOS_daily_all_stations_19700101_to_20190528.Rds



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(lubridate)

workdir <- getwd()
datadir <- file.path(workdir, "data")
asos_qc_dir <- file.path(datadir, "AK_ASOS_stations_qc")
# station ids
stids_path <- file.path(datadir, "all_stids.Rds")
stids <- readRDS(stids_path)
n_stids <- length(stids)

#------------------------------------------------------------------------------

#-- Summarize By Day ----------------------------------------------------------
# daily data frame
asos_daily <- data.frame(stid = character(), 
                         date = ymd(),
                         obs = double(), 
                         avg_sped = double(),
                         stringsAsFactors = FALSE)
# Loop through QC station data
for(i in 1:n_stids){
  asos_station_path <- file.path(asos_qc_dir, paste0(stids[i], "_qc.Rds"))
  asos_station <- readRDS(asos_station_path)
  # count number of hourly observations/24
  asos_station <- asos_station %>% 
    group_by(stid, date) %>%
    summarise(obs = sum(one)/24, 
              avg_sped = mean(sped)) %>%
    select(stid, date, obs, avg_sped)
  # append to daily
  asos_daily <- bind_rows(asos_daily, asos_station)
}
asos_daily_path <- file.path(datadir, 
                     "AK_ASOS_daily_all_stations_19700101_to_20190528.Rds")
saveRDS(asos_daily, asos_daily_path)

#------------------------------------------------------------------------------
