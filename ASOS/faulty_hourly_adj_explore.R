# adjustment assessment
#   after adjusting raw data for mean differences in time 
#   series, some data will have been adjusted below zero. 
# Where are these observations, and what should be done?

library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")

# changepoints
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")
cpts_path <- file.path(asos_select_adj_dir, "cpts_df.Rds")
cpts_df <- readRDS(cpts_path)

# loop through all the stations, save as RDS files
#   (much faster to open)
stids <- as.character(cpts_df$stid)
# column classes for csv read
classes <- c(rep("character", 3), rep("numeric", 3),
             rep("character", 2), "Date", rep("numeric", 3))
for (i in 1:length(stids)) {
  asos_path <- file.path(asos_select_adj_dir, paste0(stids[i], ".csv"))
  asos_hourly_station <- read.csv(asos_path, stringsAsFactors = FALSE,
                                  colClasses = classes)
  asos_hourly_station$drct <- as.integer(asos_hourly_station$drct)
  asos_hourly_station$peak_wind_drct <- as.integer(asos_hourly_station$peak_wind_drct)
  asos_path_rds <- file.path(asos_select_adj_dir, paste0(stids[i], ".Rds"))
  saveRDS(asos_hourly_station, asos_path_rds)
}

# loop through all stations to quantify suspisciously large speeds
susp_df <- data.frame(stid = character(),
                      obs_gr_150 = integer())
for (i in 1:length(stids)) {
  asos_path <- file.path(asos_select_adj_dir, paste0(stids[i], ".Rds"))
  asos_hourly_station <- readRDS(asos_path)
  
  asos_hourly_station$one <- 1
  susp_values <- asos_hourly_station %>%
    filter(sped > 150) %>% summarise(over_150 = sum(one))
  
  temp_row <- data.frame(stid = stids[i], 
                        over_150 = susp_values[1, 1])
  susp_df <- rbind(susp_df, temp_row)
}


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




# looking at the highest wind speeds, some ridiculous numbers
asos_hourly_station[order(asos_hourly_station$sped, decreasing = TRUE)[1:50], ]
n1 <- dim(asos_hourly_station)[1]
# remove if observed speed over 2000 mph
asos_hourly_station <- asos_hourly_station %>% filter(sped < 200)
# rows removed
n1 - dim(asos_hourly_station)[1]
