# code to create a daily dataset of mean wind speed
# ASOS data broekn into pieces for processing
library(dplyr)
library(data.table)

workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")

asos_path <- file.path(datadir, "AK_ASOS_allsites_wind_19700101_to_20190528.txt")
asos <- fread(asos_path, select = c("station", "valid", "drct", "sped"))

station_path <- file.path(datadir, "AK_ASOS_stations.csv")
stations <- read.csv(station_path, stringsAsFactors = FALSE)

# initialize data frame
# loop through ASOS data and summarize by date
asos_daily <- data.frame(stid = character(), date = as.Date(character()),
                         obs = double(), avg_sped = double())
for(i in 1:dim(stations)[1]){
  temp_data <- asos %>% filter(station == stations$stid[i])
  temp_data <- temp_data %>% 
    filter(sped != "M" & drct != "M" & sped < 150) %>%
    mutate(valid = ymd_hms(paste0(valid, ":00")),
           t_round = round_date(valid, unit = "hour"),
           date = as.Date(t_round),
           hr = as.numeric(hour(t_round)),
           m = as.numeric(minute(valid)),
           d_hr = as.numeric(abs(difftime(valid, t_round, units = "hours"))),
           one = 1)
  
  temp_data$sped <- as.numeric(temp_data$sped)
  
  temp_data <- temp_data %>% group_by(date, hr) %>%
    mutate(rank = order(order(d_hr))) %>%
    filter(rank == 1) %>% group_by(date) %>%
    summarise(obs = sum(one)/24, avg_sped = mean(sped))
  
  temp_data$station <- rep(stations$stid[i], dim(temp_data)[1])
  names(temp_data)[4] <- "stid"
  temp_data <- temp_data[, c("stid", "date", "obs", "avg_sped")]
  
  asos_daily <- rbind(asos_daily, temp_data)
}

save_path <- file.path(datadir, "AK_ASOS_daily_allsites_speed_19700101_to_20190528.csv")
write.csv(asos_daily, save_path, row.names = FALSE)
