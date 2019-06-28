# Script summary
#
# Stids
#   The meta data downloaded from IEM does not have every
#   stid found in full downloaded data (missing 2, has 1 extra)
# 
# Raw Station Rds Files
#   Save raw data from stations individually
#
# Quality assessment, filter & Save Rds
#   save df of which observations removed and why
#
# Output files:
#   /data/all_stids.Rds
#   /data/stations_raw/"stid"_raw.Rds
#   /data/stations_qc/"stid"_qc.Rds
#   /data/AK_ASOS_NA.Rds
#   /data/AK_ASOS_excess.Rds
#   /data/AK_ASOS_bad_quality.Rds


#-- Setup ---------------------------------------------------------------------
library(data.table)
library(dplyr)
library(lubridate)

workdir <- getwd()
datadir <- file.path(workdir, "data")
asos_raw_dir <- file.path(datadir, "AK_ASOS_stations_raw")
asos_qc_dir <- file.path(datadir, "AK_ASOS_stations_qc")
# all stations path
asos_path <- file.path(datadir, 
                       "AK_ASOS_all_stations_wind_19700101_to_20190528.txt")
# only concerned with wind speed and drct for now
select_cols <- c("station", "valid", "drct", "sped")
asos <- fread(asos_path, select = select_cols)
# station ids
names(asos)[1] <- "stid"
stids <- unique(asos$stid)
n_stids <- length(stids)

#------------------------------------------------------------------------------

#-- Stids ---------------------------------------------------------------------
# Save stids
stids_path <- file.path(datadir, "all_stids.Rds")
saveRDS(stids, stids_path)

#------------------------------------------------------------------------------

#-- Remove NAs ----------------------------------------------------------------
asos[asos == "M"] <- NA
asos$i <- 1:dim(asos)[1]
good_asos <- na.omit(asos)
asos_NA <- asos[-good_asos$i, ]
rm(asos)
gc()
# count NAs removed by station
asos_NA$i <- 1
asos_NA <- asos_NA %>%
  mutate(year = year(valid)) %>%
  group_by(stid, year) %>%
  summarise(obs_NA = sum(i))
# save 
asos_NA_path <- file.path(datadir, "AK_ASOS_NA.Rds")
saveRDS(asos_NA, asos_NA_path)

#------------------------------------------------------------------------------

#-- Raw Station Rds Files -----------------------------------------------------
# loop through all the stations' raw data, save as RDS files
#   for easier access later
for(i in 1:n_stids){
  stid_i <- stids[i]
  asos_station <- good_asos[stid == stid_i] %>%
    select(stid, valid, drct, sped)
  asos_station$sped <- as.numeric(asos_station$sped)
  asos_station$drct <- as.integer(asos_station$drct)
  asos_station <- asos_station %>% 
    mutate(valid = ymd_hms(paste0(valid, ":00")))
  # save as Rds
  asos_station_path <- file.path(asos_raw_dir, paste0(stids[i], "_raw.Rds"))
  saveRDS(asos_station, asos_station_path)
}
# no longer need full asos df for remainder of script
rm(good_asos)
gc()
#------------------------------------------------------------------------------

#-- Quality Assessment & Save Rds ---------------------------------------------
# loop through all stations and remove missing observations and 
#   poor quality, e.g. super high observations
# record these removed observations and why

# stids if not loading full ASOS dataset
stids_path <- file.path(datadir, "all_stids.Rds")
stids <- readRDS(stids_path)
n_stids <- length(stids)

# observations of poor quality
# reasons for not passing quality check
quality_fails <- c("geq_100", # >= 100 mph reported
                   "delta_30", # a spike of greater than 50 mph
                   "not_hour") # other obs closer to hour
bad_quality <- data.frame(stid = character(),
                          valid = ymd_hms(),
                          drct = double(),
                          sped = double(),
                          fail = character())
for(i in 1:n_stids){
  asos_station_path <- file.path(asos_raw_dir, paste0(stids[i], "_raw.Rds"))
  asos_station <- readRDS(asos_station_path)
  # create variables for quality checks
  asos_station <- asos_station %>% 
    mutate(t_round = round_date(valid, unit = "hour"),
           date = as.Date(t_round),
           H = as.numeric(hour(t_round)),
           M = as.numeric(minute(valid)),
           d_hr = as.numeric(abs(difftime(valid, t_round, units = "hours"))),
           one = 1)
  # Quality check 1: filter out measurements over 100 mph
  #   temp quality check fail df
  temp_qf <- asos_station %>% filter(sped >= 100) %>%
    select(stid, valid, drct, sped) %>%
    mutate(fail = quality_fails[1])
  bad_quality <- rbind(bad_quality, temp_qf)
  asos_station <- asos_station %>% filter(sped < 100)
  # Quality check 2: filter out measurement "spikes"
  # lagged differences of speed and timestamp
  lag_diffs <- diff(asos_station$sped)
  time_diffs <- diff(asos_station$valid)
  # taking abs() simplifies things, 
  #   assumes no consecutive jumps of 30
  oob_i <- which(abs(lag_diffs) > 30)
  spikes <- oob_i[which(diff(oob_i) == 1)] + 1
  # a "true" spike must have occurred within 2 hours of previous/next obs
  true_spikes <- which(time_diffs[spikes] <= 7200 & 
                         time_diffs[spikes - 1] <= 7200)
  spikes <- spikes[true_spikes]
  temp_qf <- asos_station[spikes, ] %>%
    select(stid, valid, drct, sped) %>%
    mutate(fail = quality_fails[2])
  bad_quality <- rbind(bad_quality, temp_qf)
  if(length(spikes) > 0){asos_station <- asos_station[-spikes, ]}
  # Quality check 3: Filter to observations that are closest to "on the hour"
  #   for wind direction; average the speeds
  asos_station <- asos_station %>% 
    group_by(date, H) %>%
    mutate(rank = order(order(d_hr)))
  temp_qf <- asos_station %>%
    filter(rank != 1) %>%
    ungroup() %>%
    select(stid, valid, drct, sped) %>%
    mutate(fail = quality_fails[3])
  bad_quality <- rbind(bad_quality, temp_qf)
  # not averaging drct, keeping nearest to hour, join with avg speeds
  asos_keep <- asos_station %>% 
    filter(rank == 1) %>%
    ungroup() %>%
    select(-sped, -rank, -H, -M)
  # average speeds where multiple of same hour, rejoin with drct data
  asos_station <- asos_station %>%
    group_by(t_round) %>%
    summarise(sped = mean(sped),
              sped_obs = n()) %>%
    full_join(asos_keep, by = "t_round")
    
  # save
  asos_save_path <- file.path(asos_qc_dir, paste0(stids[i], "_qc.Rds"))
  saveRDS(asos_station, asos_save_path)
}
# save bad quality obs and excess obs separately
excess_obs <- bad_quality %>% filter(fail == quality_fails[3])
excess_obs_path <- file.path(datadir, "AK_ASOS_excess.Rds")
saveRDS(excess_obs, excess_obs_path)
rm(excess_obs); gc()
bad_quality <- bad_quality %>% filter(fail %in% quality_fails[1:2])
bad_quality_path <- file.path(datadir, "AK_ASOS_bad_quality.Rds")
saveRDS(bad_quality, bad_quality_path)

#------------------------------------------------------------------------------
