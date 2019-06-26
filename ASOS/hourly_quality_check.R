# Script summary
#
# Quality assessment & Save Rds
#   some observations will be erroneously large. Which ones
#   are removed and why?



#-- Setup ---------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

workdir <- getwd()
datadir <- file.path(workdir, "data")
# all stations path
asos_path <- file.path(datadir, 
                       "AK_ASOS_all_stations_wind_19700101_to_20190528.txt")
# only concerned with wind speed and drct for now
select_cols <- c("station", "valid", "drct", "sped")
asos <- fread(asos_path, select = select_cols)
#------------------------------------------------------------------------------

#-- Raw Station Rds Files -----------------------------------------------------
# loop through all the stations' raw data, save as RDS files
#   (much faster to open)

for (i in 1:length(stids)) {
  asos_path <- file.path(asos_select_adj_dir, paste0(stids[i], ".csv"))
  asos_hourly_station <- read.csv(asos_path, stringsAsFactors = FALSE,
                                  colClasses = classes)
  asos_hourly_station$drct <- as.integer(asos_hourly_station$drct)
  asos_hourly_station$peak_wind_drct <- as.integer(asos_hourly_station$peak_wind_drct)
  asos_path_rds <- file.path(asos_select_adj_dir, paste0(stids[i], ".Rds"))
  saveRDS(asos_hourly_station, asos_path_rds)
}

#------------------------------------------------------------------------------

#-- Quality Assessment & Save Rds ---------------------------------------------
# loop through all stations and remove missing observations and 
#   poor quality, e.g. super high observations
# record these removed observations and why

# station ids
names(asos)[1] <- "stid"
stids <- unique(asos$station)
# observations of poor quality
# reasons for not passing quality check
quality_fails <- c("sped_NA", # missing speed
                   "drct_NA", # missing wind
                   "geq_100", # >= 100 mph reported
                   "delta_50", # a spike of greater than 50 mph
                   "not_hour") # other obs closer to hour
poor_quality_obs <- data.frame(stid = character(),
                               valid = ymd_hms(),
                               drct = double(),
                               sped = double(),
                               fail = factor(levels = quality_fails))
for (i in 1:length(stids)) {
  asos_station <- asos %>% filter(stid == stids[i])
  asos_station[asos_station == "M"] <- NA
  asos_station$sped <- as.numeric(asos_station$sped)
  asos_station$drct <- as.numeric(asos_station$drct)
  # create new variables
  asos_station <- asos_station %>% 
    mutate(valid = ymd_hms(paste0(valid, ":00")),
           t_round = round_date(valid, unit = "hour"),
           date = as.Date(t_round),
           H = as.numeric(hour(t_round)),
           M = as.numeric(minute(valid)),
           d_hr = as.numeric(abs(difftime(valid, t_round, units = "hours"))),
           one = 1)
  # Quality check 1: filter out measurements over 100 mph
  temp_poor <- asos_station %>% filter(sped >= 100) %>%
    select(stid, valid, drct, sped) %>%
    mutate(fail = "geq_100")
  poor_quality_obs <- rbind(poor_quality_obs, temp_poor)
  asos_station <- asos_station %>% filter(sped < 100)
  # Quality check 2: filter out measurement "spikes"
  diff(asos_station$sped)
  # Filter to observations that are closest to "on the hour"
  asos_station <- asos_station %>% 
    group_by(date, H) %>%
    mutate(rank = order(order(d_hr))) %>%
    filter(rank == 1)
  temp_data <- asos_station %>% 
    group_by(date) %>%
    summarise(obs = sum(one)/24, 
              avg_sped = mean(sped))
  
  
  # filter observations over 150
  temp_data$stid <- rep(stids[i], dim(temp_data)[1])
  names(temp_data)[5] <- "stid"
  temp_data <- temp_data[, c("stid", "date", "obs", "avg_sped", "avg_sped_adj")]
  # save and bind
  saveRDS(asos_hourly_station, asos_save_path)
  asos_daily <- rbind(asos_daily, temp_data)
  asos_hourly_station$one <- 1
  susp_values <- asos_hourly_station %>%
    filter(sped > 75) %>% 
    summarise(over_75 = sum(one),
              over_100 = sum())
  
  temp_row <- data.frame(stid = stids[i], 
                        over_75 = susp_values[1, 1],
                        over_100 = susp_values[1, 2])
  susp_df <- rbind(susp_df, temp_row)
}
# save this info somehow



# Loop to quantify extent of values pushed to zero or
#   pushed above zero
stids_cpts <- as.character(cpts_df$stid[cpts_df$cpts > 0])
adj_stations <- data.frame(stid = character(),
                           neg_err_tot = integer(),
                           neg_err_obs_prop = numeric(),
                           neg_err_sum_prop = numeric(),
                           pos_err_tot = integer(),
                           pos_err_obs_prop = numeric(),
                           pos_err_sum_prop = numeric())
for (i in 1:length(stids_cpts)) {
  asos_path <- file.path(asos_select_adj_dir, paste0(stids_cpts[i], ".Rds"))
  asos_hourly_station <- readRDS(asos_path) %>% select(drct, sped, sped_adj)
  
  asos_hourly_station <- asos_hourly_station[complete.cases(asos_hourly_station), ]
  # percent of observations that were positive or 0 and adjusted
  #   below 0
  asos_hourly_station$one <- 1
  n <- dim(asos_hourly_station)[1]
  adj_err_neg <- asos_hourly_station %>% 
    filter(sped_adj < 0) %>% 
    summarise(total_adj_err = sum(one), 
             adj_err_prop = sum(one)/n,
             err_sum = sum(abs(sped_adj)))
  
  adj_err_pos <- asos_hourly_station %>%
    filter(sped == 0 & sped_adj > 0) %>% 
    summarise(total_ajd_err = sum(one),
              adj_err_prop = sum(one)/n,
              err_sum = sum(sped_adj))
  
  tot_sped_sum <- sum(asos_hourly_station$sped)
  adj_df <- data.frame(stid = stids_cpts[i], 
                       neg_err_tot = adj_err_neg[1, 1],
                       neg_err_obs_prop = adj_err_neg[1, 2],
                       neg_err_sum_prop = adj_err_neg[1, 3]/tot_sped_sum,
                       pos_err_tot = adj_err_pos[1, 1],
                       pos_err_obs_prop = adj_err_pos[1, 2],
                       pos_err_sum_prop = adj_err_pos[1, 3]/tot_sped_sum)
  
  adj_stations <- rbind(adj_stations, adj_df)
}

adj_stations$neg_err_obs_prop <- round(adj_stations$neg_err_obs_prop, 2)
adj_stations$neg_err_sum_prop <- round(adj_stations$neg_err_sum_prop, 2)
adj_stations$pos_err_obs_prop <- round(adj_stations$pos_err_obs_prop, 2)
adj_stations$pos_err_sum_prop <- round(adj_stations$pos_err_sum_prop, 2)
