# Script summary
#
# Quantile Mapping
#   Loop through daily summary and determine 0-2 changepoints
#   Record dates, changepoints, means and sd's before and after
#   Break data into segments, perform quantile mapping, and save
#
# Clean CSV files
#   loop through Rds files, remove unnecessary data and
#   fill rows with blank data, save csv's
#
# Output files:
#   /data/AK_ASOS_stations_adj/"stids".Rds
#   /data/AK_ASOS_stations_adj/cpts_df.Rds
#   /data/AK_ASOS_stations_adj_csv/"stids".csv



#-- Setup ---------------------------------------------------------------------

# Custom quantile mapping function
#   Values adjusted below zero set to zero
qMapWind <- function(obs, sim){
  require(ggplot2)
  qn <- min(length(obs), length(sim))
  q_obs <- quantile(obs, seq(0, 1, length.out = qn), type = 8)
  q_sim <- quantile(sim, seq(0, 1, length.out = qn), type = 8)
  q_diff <- q_sim - q_obs
  # assign quantiles to observations
  # round() used to alleviate troubles with fp comparison
  qs <- unique(round(q_sim, 10))
  q_t <- table(round(q_sim, 10))
  q_ids <- c()
  
  # loop through unique quantiles
  for(i in 1:length(qs)){
    if(i == 1){
      i_s <- which(sim == qs[i])
      q_mem <- 0
    }else{
      i_s <- which(round(sim, 10) <= qs[i] & 
                     round(sim, 10) > qs[i - 1])
    }
    # occasionally, quantiles are determined where no data will fall
    #   just skip these
    n_i <- length(i_s)
    if(n_i == 0){next}
    dup_q <- q_t[i]

    # randomly apply quantile id's to the indices of duplicated quantiles
    if(n_i > 1){
      names(i_s) <- (as.numeric(cut_number(i_s, dup_q)) + q_mem)[sample(n_i)]
    } else {
      names(i_s) <- 1 + q_mem
    }
    q_ids <- c(q_ids, i_s)
    q_mem <- dup_q + q_mem
  }
  q_ids <- q_ids[order(q_ids)]
  q_adj <- sim - as.numeric(q_diff[as.numeric(names(q_ids))])
  # q_adj <- if_else(q_adj < 0, 0, q_adj)
  return(q_adj)
}

# Setup workspace
library(data.table)
library(dplyr)
library(lubridate)
library(changepoint)

workdir <- getwd()
datadir <- file.path(workdir, "data")
# daily avg speed data
asos_daily_path <- file.path(datadir, 
                             "AK_ASOS_daily_all_stations_19700101_to_20190528.Rds")
asos_daily <- readRDS(asos_daily_path)
# stations selected based on available data
select_stations_path <- file.path(datadir, "AK_ASOS_select_stations.Rds")
select_stations <- readRDS(select_stations_path)
# QC/filtered data dir, and adjusted/adjusted csv directories for saving
asos_qc_dir <- file.path(datadir, "AK_ASOS_stations_qc")
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
asos_adj_dir_csv <- file.path(datadir, "AK_ASOS_stations_adj_csv")

# target period limits, for whether we do anything about changepoints
start_date <- ymd("1980-01-01")
end_date <- start_date + years(35)
# select station ids
stids <- select_stations$stid
# cutting daily data frame down
asos_daily <- asos_daily %>% filter(stid %in% stids)

#------------------------------------------------------------------------------

#-- Changepoints and Quantile Mapping -----------------------------------------
# loop through and create df of sites and changepoints
#   based on monthly average of daily means
# to_calm and from_calm
cpts_df <- data.frame(stid = stids, 
                      cp1 = ymd("1970-01-01"), 
                      cp2 = ymd("1970-01-01"),
                      m1 = 0, m2 = 0, m3 = 0,
                      sd1 = 0, sd2 = 0, sd3 = 0,
                      to_calm = 0, from_calm = 0,
                      stringsAsFactors = FALSE)

# function to fill "adjusted" columns in hourly 
#   station dataframe with unadjusted data
noAdjust <- function(df) {
  df$sped_adj <- df$sped

  return(df)
}
# changepoint tolerance
alpha <- 0.01
conf <- 1 - alpha
for(i in seq_along(stids)) {
  # monthly data for finding changepoints
  asos_station <- asos_daily %>% filter(stid == stids[i]) %>%
    mutate(month = format(date, format = "%Y-%m"), one = 1) %>% 
    group_by(month) %>% summarise(avg_sped_mo = mean(avg_sped)) %>%
    mutate(date = as.Date(paste0(month, "-01"),
                          origin = "1970-01-01"))
  # hourly data to adjust
  asos_hourly_path <- file.path(asos_qc_dir, paste0(stids[i], "_qc.Rds"))
  asos_hourly_station <- readRDS(asos_hourly_path)
  # save paths
  asos_adj_rds_path <- file.path(asos_adj_dir, paste0(stids[i], ".Rds"))
  
  # initial changepoint
  m_amoc <- cpt.mean(asos_station$avg_sped_mo, method = "AMOC", 
                     Q = 1, pen.value = alpha, 
                     penalty = "Asymptotic", class = FALSE)
  # if significant, run again on both segments of data
  if (m_amoc[2] >= conf) {
    cp1_date <- asos_station$date[m_amoc[1]]
    seg1 <- asos_station %>% filter(date <= cp1_date)
    seg2 <- asos_station %>% filter(date > cp1_date)
    m2_amoc <- cpt.mean(seg1$avg_sped_mo, method = "AMOC",
                        class = FALSE)
    m3_amoc <- cpt.mean(seg2$avg_sped_mo, method = "AMOC",
                        class = FALSE)
    m2_conf <- m2_amoc[2]
    m3_conf <- m3_amoc[2]
    # if either cp is significant, take the more significant
    cond1 <- m2_conf > conf | m3_conf > conf
    if (cond1) {
      if (m2_conf >= m3_conf){
        cp2_date <- cp1_date
        cp1_date <- seg1$date[m2_amoc[1]]
      } else {cp2_date <- seg2$date[m3_amoc[1]]}
    } else {cp2_date <- NA}
  # if no changepoint found, next station
  } else {
    cpts_df[cpts_df$stid == stids[i], 2:11] <- rep(NA, 10)
    asos_adj <- noAdjust(asos_hourly_station)
    # save un-adjusted data filtered to target dates
    asos_adj <- asos_adj %>% filter(date >= start_date & 
                                      date <= end_date)
    saveRDS(asos_adj, asos_adj_rds_path)
    next
  }
  # if dates are not far apart, discard date 2
  if (is.na(cp2_date) == FALSE) {
    cp_dist <- as.numeric(abs(difftime(cp1_date, cp2_date, units = "weeks")))
    if (cp_dist <= 26) {
      cp2_date <- NA
    }
  }

  # ensure cp1 date is older than cp2 date (should be)
  if (is.na(cp2_date) == FALSE) {
    if (cp1_date > cp2_date) {
      a <- cp1_date
      cp1_date <- cp2_date
      cp2_date <- a
    }
  }

  # if cp1 date is before 1980, or within 6 months of 2015-01-01, ignore
  # no cp2 found
  if (is.na(cp2_date) == TRUE) {
    if (cp1_date < ymd("1980-01-01") | cp1_date > ymd("2014-06-01")) {
      cpts_df[cpts_df$stid == stids[i], 2:11] <- rep(NA, 10)
      asos_adj <- noAdjust(asos_hourly_station)
      asos_adj <- asos_adj %>% filter(date >= start_date & 
                                        date <= end_date)
      saveRDS(asos_adj, asos_adj_rds_path)
      next
    }
  } else {
    # both dates oob
    if ((cp1_date < ymd("1980-01-01") | cp1_date > ymd("2014-06-01")) & 
        (cp2_date < ymd("1980-01-01") | cp2_date > ymd("2014-06-01"))) {
      cpts_df[cpts_df$stid == stids[i], 2:11] <- rep(NA, 10)
      asos_adj <- noAdjust(asos_hourly_station)
      asos_adj <- asos_adj %>% filter(date >= start_date & 
                                        date <= end_date)
      saveRDS(asos_adj, asos_adj_rds_path)
      next
    }
    # cp1 oob
    if ((cp1_date < ymd("1980-01-01") | cp1_date > ymd("2014-06-01")) & 
        (cp2_date >= ymd("1980-01-01") & cp2_date <= ymd("2014-06-01"))) {
      cp1_date <- cp2_date
      cp2_date <- NA
    }
    # cp2 oob
    if(!is.na(cp2_date)){
      if ((cp1_date >= ymd("1980-01-01") & cp1_date <= ymd("2014-06-01")) & 
          (cp2_date < ymd("1980-01-01") | cp2_date > ymd("2014-06-01"))) {
        cp2_date <- NA
      }
    }
  }

  # record changepoints
  cpts_df$cp1[cpts_df$stid == stids[i]] <- cp1_date
  cpts_df$cp2[cpts_df$stid == stids[i]] <- cp2_date
  # summarise hourly data by periods determined by changepoints
  # set hourly and daily periods
  # hourly, for adjusting the observations
  asos_hourly_station <- asos_hourly_station %>% 
    mutate(period = if_else(date < cp1_date, 1, 2))
  asos_hourly_station$period[asos_hourly_station$date > cp2_date] <- 3
  # monthly, for summarizing means
  asos_station <- asos_station %>% 
    mutate(period = if_else(date < cp1_date, 1, 2))
  asos_station$period[asos_station$date > cp2_date] <- 3
  # calculate mean period speeds
  means_df <- asos_station %>% 
    group_by(period) %>% 
    summarise(m_per = mean(avg_sped_mo),
              sd_per = sd(avg_sped_mo))
  
  ## adjust via quantile mapping
  no_perd <- max(asos_station$period)
  if (no_perd == 2) {
    p1_mean <- means_df$m_per[means_df$period == 1]
    p2_mean <- means_df$m_per[means_df$period == 2]
    p1_sd <- means_df$sd_per[means_df$period == 1]
    p2_sd <- means_df$sd_per[means_df$period == 2]
    
    biased <- asos_hourly_station$sped[asos_hourly_station$period == 1]
    current <- asos_hourly_station$sped[asos_hourly_station$period == 2]
    
    sped_qmap_adj <- qMapWind(current, biased)
    
    asos_hourly_station$sped_adj <- c(sped_qmap_adj, current)
    
    # record stats
    cpts_df$m1[cpts_df$stid == stids[i]] <- p1_mean
    cpts_df$m2[cpts_df$stid == stids[i]] <- p2_mean
    cpts_df$m3[cpts_df$stid == stids[i]] <- NA
    cpts_df$sd1[cpts_df$stid == stids[i]] <- p1_sd
    cpts_df$sd2[cpts_df$stid == stids[i]] <- p2_sd
    cpts_df$sd3[cpts_df$stid == stids[i]] <- NA
    
  # otherwise, three periods (two changepoints)  
  } else {
    p1_mean <- means_df$m_per[means_df$period == 1]
    p2_mean <- means_df$m_per[means_df$period == 2]
    p3_mean <- means_df$m_per[means_df$period == 3]
    p1_sd <- means_df$sd_per[means_df$period == 1]
    p2_sd <- means_df$sd_per[means_df$period == 2]
    p3_sd <- means_df$sd_per[means_df$period == 3]
    
    biased1 <- asos_hourly_station$sped[asos_hourly_station$period == 1]
    biased2 <- asos_hourly_station$sped[asos_hourly_station$period == 2]
    current <- asos_hourly_station$sped[asos_hourly_station$period == 3]
    
    sped_qmap_adj1 <- qMapWind(current, biased1)
    sped_qmap_adj2 <- qMapWind(current, biased2)
    
    asos_hourly_station$sped_adj <- c(sped_qmap_adj1, 
                                      sped_qmap_adj2, 
                                      current)
    
    # record meansi
    cpts_df$m1[cpts_df$stid == stids[i]] <- p1_mean
    cpts_df$m2[cpts_df$stid == stids[i]] <- p2_mean
    cpts_df$m3[cpts_df$stid == stids[i]] <- p3_mean
    cpts_df$sd1[cpts_df$stid == stids[i]] <- p1_sd
    cpts_df$sd2[cpts_df$stid == stids[i]] <- p2_sd
    cpts_df$sd3[cpts_df$stid == stids[i]] <- p3_sd
  }
  
  # record other info
  # speeds adjusted from calm and to calm
  from_calm <- asos_hourly_station %>% 
    filter(sped == 0 & sped_adj > 0) %>%
    summarise(count = n())
  to_calm <- asos_hourly_station %>% 
    filter(sped > 0 & sped_adj <= 0) %>%
    summarise(count = n())

  cpts_df$from_calm[cpts_df$stid == stids[i]] <- from_calm$count
  cpts_df$to_calm[cpts_df$stid == stids[i]] <- to_calm$count
  
  # original adjusted speed
  asos_hourly_station$orig_adj <- asos_hourly_station$sped_adj
  asos_hourly_station$sped_adj[asos_hourly_station$sped_adj < 0] <- 0
  # save
  asos_hourly_station <- asos_hourly_station %>% 
    filter(date >= start_date & 
             date <= end_date)
  saveRDS(asos_hourly_station, asos_adj_rds_path)
}

cpts_df$cpts <- (!is.na(cpts_df$cp1)) + (!is.na(cpts_df$cp2))
cpts_path <- file.path(asos_adj_dir, "cpts_df.Rds")
saveRDS(cpts_df, cpts_path)

#------------------------------------------------------------------------------

#-- Save CSVs -----------------------------------------------------------------
# construct data frame for all potential hours of observation between 1980
#   and 2015
{start_date <- ymd_hms("1980-01-01 00:00:00")
end_date <- ymd_hms("2015-01-01 23:00:00")
sampling_df <- data.frame(t_round = seq(start_date, end_date, "hour"))
# Loop through saved Rds station data and save as csv
for(i in seq_along(stids)){
  station_rds_path <- file.path(asos_adj_dir, paste0(stids[i], ".Rds"))
  station_csv_path <- file.path(asos_adj_dir_csv, paste0(stids[i], ".csv"))
  # interpolate missing times by joining with sampling_df, save csv
  asos_df <- readRDS(station_rds_path) %>% 
    right_join(sampling_df, by = "t_round") %>%
    select(stid, t_round, drct, sped, sped_adj, valid) %>%
    rename(t_actual = valid) %>% 
    mutate(stid = stids[i]) %>%
    as.data.frame()
  write.csv(asos_df, station_csv_path, row.names = FALSE)
}}

# save CSV of all changepoints info
cpts_df_save <- cpts_df %>% 
  filter(is.na(cp1) != TRUE) %>%
  mutate(m1 = round(m1, 2),
         m2 = round(m2, 2),
         m3 = round(m3, 2),
         sd1 = round(sd1, 2),
         sd2 = round(sd2, 2),
         sd3 = round(sd3, 2))
cpts_save_path <- file.path(datadir, "AK_ASOS_changepoint_info.csv")
write.csv(cpts_df_save, cpts_save_path, row.names = FALSE)
#------------------------------------------------------------------------------
