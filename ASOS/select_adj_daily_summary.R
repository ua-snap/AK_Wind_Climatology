# code to create a daily dataset of mean wind speed (+ adjusted)
# Observations are rounded to the nearest hour, closest is taken
#   Also creates and saves new dataset for each station ("_hour")
# Relies on adjusted data via quantile mapping
library(dplyr)
library(data.table)

workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")

# changepoints
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")
cpts_path <- file.path(asos_select_adj_dir, "cpts_df.Rds")
cpts_df <- readRDS(cpts_path)
# stids
stids <- cpts_df$stid

# initialize data frame
# loop through adjusted data objects and summarize by date
asos_daily <- data.frame(stid = character(), date = as.Date(character()),
                         obs = double(), avg_sped = double())
for(i in 1:length(stids)){
  asos_path <- file.path(asos_select_adj_dir, 
                         paste0(stids[i], ".Rds"))
  asos_save_path <- file.path(asos_select_adj_dir,
                              paste0(stids[i], "_hour.Rds"))
  asos_hourly_station <- readRDS(asos_path)
  # create new variables
  asos_hourly_station <- asos_hourly_station %>% 
    mutate(adj_diff = sped - sped_adj,
           valid = ymd_hms(paste0(valid, ":00")),
           t_round = round_date(valid, unit = "hour"),
           date = as.Date(t_round),
           H = as.numeric(hour(t_round)),
           M = as.numeric(minute(valid)),
           d_hr = as.numeric(abs(difftime(valid, t_round, units = "hours"))),
           one = 1)
  # rank within date, hr
  asos_hourly_station <- asos_hourly_station %>% 
    group_by(date, H) %>%
    mutate(rank = order(order(d_hr))) %>%
    filter(rank == 1)
  
  temp_data <- asos_hourly_station %>% 
    group_by(date) %>%
    summarise(obs = sum(one)/24, 
              avg_sped = mean(sped),
              avg_sped_adj = mean(sped_adj))
  
  temp_data$stid <- rep(stids[i], dim(temp_data)[1])
  names(temp_data)[5] <- "stid"
  temp_data <- temp_data[, c("stid", "date", "obs", "avg_sped", "avg_sped_adj")]
  # save and bind
  saveRDS(asos_hourly_station, asos_save_path)
  asos_daily <- rbind(asos_daily, temp_data)
}
# save daily
save_path <- file.path(datadir, "AK_ASOS_daily_select_adjspeed_19800101_to_20150101.csv")
write.csv(asos_daily, save_path, row.names = FALSE)
