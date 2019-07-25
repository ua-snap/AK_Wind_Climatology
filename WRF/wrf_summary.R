# Script Summary
#
# Summarize all adjusted WRF ouput data by day and month
#
# Output files:
#   /data/ERA_daily.Rds
#   /data/ERA_monthly.Rds
#   /data/CM3h_daily.Rds
#   /data/CM3h_monthly.Rds
#   /data/CM3f_daily.Rds
#   /data/CM3f_monthly.Rds
#   /data/CCSM4h_daily.Rds
#   /data/CCSM4h_monthly.Rds
#   /data/CCSM4f_daily.Rds
#   /data/CCSM4f_monthly.Rds



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(lubridate)

workdir <- getwd()
datadir <- file.path(workdir, "data")
era_adj_dir <- file.path(datadir, "ERA_stations_adj")
era_adj_paths <- list.files(era_adj_dir, full.names = TRUE)
cm3_adj_dir <- file.path(datadir, "CM3_stations_adj")
cm3h_adj_paths <- list.files(cm3_adj_dir, pattern = "cm3h", full.names = TRUE)
cm3f_adj_paths <- list.files(cm3_adj_dir, pattern = "cm3f", full.names = TRUE)
ccsm4_adj_dir <- file.path(datadir, "CCSM4_stations_adj")
ccsm4h_adj_paths <- list.files(ccsm4_adj_dir, full.names = TRUE, 
                               pattern = "ccsm4h")
ccsm4f_adj_paths <- list.files(ccsm4_adj_dir, full.names = TRUE,
                               pattern = "ccsm4f")

#------------------------------------------------------------------------------

{#-- Summarize ERA By Day and Month -------------------------------------------
  # loop through selected/adjusted data and summarize by both day and month
  read_n_summarize <- function(path, by.day = TRUE){
    if(by.day == TRUE){
      readRDS(path) %>% 
        mutate(date = ymd(format(ts, "%Y-%m-%d"))) %>%
        group_by(stid, date) %>%
        summarise(avg_sped = mean(sped),
                  avg_sped_adj = mean(sped_adj)) %>%
        select(stid, date, avg_sped, avg_sped_adj)
    } else {
      readRDS(path) %>% 
        mutate(ym_date = ymd(paste0(format(ts, "%Y-%m"), "-01"))) %>%
        group_by(stid, ym_date) %>%
        summarise(avg_sped = mean(sped),
                  avg_sped_adj = mean(sped_adj)) %>%
        select(stid, ym_date, avg_sped, avg_sped_adj)
    }
  }
  
  # ERA-interim
  era_daily <- bind_rows(lapply(era_adj_paths, read_n_summarize))
  era_monthly <- bind_rows(lapply(era_adj_paths, read_n_summarize, 
                                  by.day = FALSE))
  era_daily_path <- file.path(datadir, 
                               "ERA_daily.Rds")
  saveRDS(era_daily, era_daily_path)
  era_monthly_path <- file.path(datadir, 
                                 "ERA_monthly.Rds")
  saveRDS(era_monthly, era_monthly_path)
  
  # CM3 historical
  cm3h_daily <- bind_rows(lapply(cm3h_adj_paths, read_n_summarize))
  cm3h_monthly <- bind_rows(lapply(cm3h_adj_paths, read_n_summarize, 
                                  by.day = FALSE))
  cm3h_daily_path <- file.path(datadir, 
                              "CM3h_daily.Rds")
  saveRDS(cm3h_daily, cm3h_daily_path)
  cm3h_monthly_path <- file.path(datadir, 
                                "CM3h_monthly.Rds")
  saveRDS(cm3h_monthly, cm3h_monthly_path)
  
  # CM3 future
  cm3f_daily <- bind_rows(lapply(cm3f_adj_paths, read_n_summarize))
  cm3f_monthly <- bind_rows(lapply(cm3f_adj_paths, read_n_summarize, 
                                   by.day = FALSE))
  cm3f_daily_path <- file.path(datadir, 
                               "CM3f_daily.Rds")
  saveRDS(cm3f_daily, cm3f_daily_path)
  cm3f_monthly_path <- file.path(datadir, 
                                 "CM3f_monthly.Rds")
  saveRDS(cm3f_monthly, cm3f_monthly_path)
  
  # CCSM4 historical
  ccsm4h_daily <- bind_rows(lapply(ccsm4h_adj_paths, read_n_summarize))
  ccsm4h_monthly <- bind_rows(lapply(ccsm4h_adj_paths, read_n_summarize, 
                                   by.day = FALSE))
  ccsm4h_daily_path <- file.path(datadir, 
                               "CCSM4h_daily.Rds")
  saveRDS(ccsm4h_daily, ccsm4h_daily_path)
  ccsm4h_monthly_path <- file.path(datadir, 
                                 "CCSM4h_monthly.Rds")
  saveRDS(ccsm4h_monthly, ccsm4h_monthly_path)
  
  # CCSM4 future
  ccsm4f_daily <- bind_rows(lapply(ccsm4f_adj_paths, read_n_summarize))
  ccsm4f_monthly <- bind_rows(lapply(ccsm4f_adj_paths, read_n_summarize, 
                                   by.day = FALSE))
  ccsm4f_daily_path <- file.path(datadir, 
                               "CCSM4f_daily.Rds")
  saveRDS(ccsm4f_daily, ccsm4f_daily_path)
  ccsm4f_monthly_path <- file.path(datadir, 
                                 "CCSM4f_monthly.Rds")
  saveRDS(ccsm4f_monthly, ccsm4f_monthly_path)
}

#------------------------------------------------------------------------------
