# code to create a daily dataset of mean wind speed
#   from mean-adjusted station datasets
library(dplyr)
library(data.table)
library(lubridate)

workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")
asos_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")

# stations selected based on available data
select_stations_path <- file.path(datadir, "select_stations.Rds")
select_stations <- readRDS(select_stations_path)
stids <- select_stations$stid

# initialize data frame
# loop through ASOS data and summarize by date
asos_adj_daily <- data.frame(stid = character(), date = as.Date(character()),
                             obs = double(), avg_sped = double(), 
                             avg_adj_sped = double())
for(i in 1:dim(select_stations)[1]){
  asos_adj_path <- file.path(asos_adj_dir, paste0(stids[i], ".csv"))
  asos_adj <- fread(asos_adj_path, 
                    select = c("station", "valid", "sped", 
                               "drct", "sped_adj"))
  
  asos_adj <- asos_adj %>% filter(is.na(sped) == FALSE & 
                                      is.na(drct) == FALSE) %>%
    mutate(valid = ymd_hms(paste0(valid, ":00")),
           t_round = round_date(valid, unit = "hour"),
           date = as.Date(t_round),
           hr = as.numeric(hour(t_round)),
           m = as.numeric(minute(valid)),
           d_hr = as.numeric(abs(difftime(valid, t_round, units = "hours"))),
           one = 1)
  
  temp_data <- asos_adj %>% group_by(date, hr) %>%
    mutate(rank = order(order(d_hr))) %>%
    filter(rank == 1) %>% group_by(date) %>%
    summarise(obs = sum(one)/24, 
              avg_sped = mean(sped),
              avg_adj_sped = mean(sped_adj))
  
  temp_data$stid <- rep(stids[i], dim(temp_data)[1])
  temp_data <- temp_data[, c("stid", "date", "obs",
                             "avg_sped", "avg_adj_sped")]
  
  asos_adj_daily <- rbind(asos_adj_daily, temp_data)
}

save_path <- file.path(datadir, "AK_ASOS_daily_select_adjspeed_19800101_to_20150101.csv")
write.csv(asos_adj_daily, save_path, row.names = FALSE)
