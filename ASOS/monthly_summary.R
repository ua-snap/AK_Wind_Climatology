# Script for summarizing the data by month for all sites 
#   and saving the results as data and otherwise (e.g. figures)


library(dplyr)
library(lubridate)

workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")
# stations selected based on available data
select_stations_path <- file.path(datadir, "select_stations.Rds")
select_stations <- readRDS(select_stations_path)
# subdirectory for adjusted/filtered (1980-2015) data
adj_dir <- "AK_ASOS_allsites_wind_19800101_to_20150101_adj"
asos_select_adj_dir <- file.path(datadir, adj_dir)

stids <- select_stations$stid
asos_monthly <- data.frame(stid = character(),
                           month = double(),
                           avg_sped = double(),
                           se_sped = double(),
                           n = double())
for(i in 1:length(stids)){
  asos_path <- file.path(asos_select_adj_dir, paste0(stids[i], "_hour.Rds"))
  asos <- readRDS(asos_path)
  asos_monthly_i <- asos %>%
    mutate(mm = month(valid),
           one = 1) %>%
    group_by(station, mm) %>% 
    summarise(avg_sped = mean(sped_adj),
              sped_err = sd(sped_adj)/sqrt(sum(one)),
              n = sum(one)) 
  names(asos_monthly_i)[1] <- "stid"
  
  df <- as.data.frame(asos_monthly_i)
  asos_monthly <- rbind(asos_monthly, df)
  if(i == length(stids)){
    save_path <- file.path(datadir, "monthly_speeds.Rds")
    saveRDS(asos_monthly, save_path)
  }
}
