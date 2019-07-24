# Script summary
#
# Determine station cells
#   find the indices from the WRF output that correspond to 
#   selected stations
#
# Query the ERA-interim WRF ouput
#   extract and save by station
#
# Query the CM3 WRF ouput
#   extract and save by station
#
# Query the CCSM4 WRF ouput
#   extract and save by station
#
# Output files:
#   /data/stid_wrf_ind.Rds
#   /data/ERA_stations_raw/"stid"_era_raw.Rds
#   /data/CM3_stations_raw/"stid"_cm3h_raw.Rds
#   /data/CM3_stations_raw/"stid"_cm3f_raw.Rds
#   /data/CCSM4_stations_raw/"stid"_ccsm4h_raw.Rds
#   /data/CCSM4_stations_raw/"stid"_ccsm4f_raw.Rds



#-- Setup ---------------------------------------------------------------------
library(ncdf4) 
library(raster) 
library(rgdal)
library(dplyr)
library(lubridate)
library(ggplot2)

workdir <- getwd()
datadir <- file.path(workdir, "data")
# local WRF data for station-based WRF output
era_raw_dir <- file.path(datadir, "ERA_stations_raw")
cm3_raw_dir <- file.path(datadir, "CM3_stations_raw")
ccsm4_raw_dir <- file.path(datadir, "CCSM4_stations_raw")
# WRF output dir (external drive)
wrf_output_dir <- "F:/Wind_Climatology/data/WRF_output"

# select stations meta data
asos_meta_path <- file.path(datadir, "AK_ASOS_select_stations.Rds")
select_stations <- readRDS(asos_meta_path)
# select station stids
stids <- unique(select_stations$stid)

#------------------------------------------------------------------------------

#-- Station Cells -------------------------------------------------------------
# Use Ancilliary data to determine grid cells for stations
anc <- nc_open(file.path(datadir, "geo_em.d01.nc"))
xlat_m <- ncvar_get(anc, "XLAT_M")
xlon_m <- ncvar_get(anc, "XLONG_M")
nc_close(anc)
coords <- cbind(select_stations$lat, select_stations$lon)
wrf_coords <- array(c(xlat_m, xlon_m), dim = c(262, 262, 2))

euc.dist <- function(x1, x2){sqrt(sum((x1 - x2) ^ 2))}
stid_wrf_ind <- data.frame(stid = select_stations$stid,
                           wrf_i = 0, wrf_j = 0)
pb <- progress_bar$new(total = dim(coords)[1]),
                       format = " Calculating WRF indices [:bar] :percent")
for(i in seq_along(coords[, 1])){
  temp <- apply(wrf_coords, c(1, 2), euc.dist, coords[i, ])
  ji <- which(temp == min(temp), arr.ind = TRUE)
  stid_wrf_ind[i, 2:3] <- c(ji[2], ji[1])
  pb$tick()
}
saveRDS(stid_wrf_ind, file.path(datadir, "stid_wrf_ind.Rds"))

#------------------------------------------------------------------------------

#-- Extract ERA-Interim WRF Output --------------------------------------------
library(ncdf4)
library(dplyr)
library(lubridate)
{
  # setup years for iteration (ERA only)
  start_date <- ymd("1980-01-01")
  end_date <- ymd("2015-01-01")
  target_years <- year(start_date):year(end_date)
  # u10 and v10 historical directories (from external drive)
  u10_dir <- file.path(wrf_output_dir, "ERA_u10")
  v10_dir <- file.path(wrf_output_dir, "ERA_v10")
  # initialize data frames for saving components
  for(i in seq_along(stids)){
    save_path <- file.path(era_raw_dir, 
                           paste0(stids[i], "_era_raw.Rds"))
    df <- data.frame(stid = character(),
                     ts = ymd_hms(),
                     u10 = numeric(),
                     v10 = numeric(),
                     stringsAsFactors = FALSE)
    saveRDS(df, save_path)
  }
  pb <- progress_bar$new(total = length(target_years),
                         format = " Extracting ERA output [:bar] :percent")
  # loop through .nc files and save 
  for(i in seq_along(target_years)){
    # open connections
    u10_fname <- "u10_hourly_wrf_ERA-Interim_historical_"
    u10_path <- file.path(u10_dir, paste0(u10_fname, target_years[i], ".nc"))
    v10_fname <- "v10_hourly_wrf_ERA-Interim_historical_"
    v10_path <- file.path(v10_dir, paste0(v10_fname, target_years[i], ".nc"))
    u10 <- nc_open(u10_path)
    v10 <- nc_open(v10_path)
    # u10 time vectors
    u10_start <- ymd_hms(paste0(substr(u10$dim$time$units, 13, 22), " 00:00:00"))
    u10_ts <- ncvar_get(u10, varid = "time")
    u10_ts <- u10_start + hours(u10_ts)
    
    for(j in seq_along(stids)){
      station_path <- file.path(era_raw_dir, 
                                paste0(stids[j], "_era_raw.Rds"))
      station <- readRDS(station_path)
      # extract data
      wrf_i <- stid_wrf_ind$wrf_i[stid_wrf_ind$stid == stids[j]]
      wrf_j <- stid_wrf_ind$wrf_j[stid_wrf_ind$stid == stids[j]]
      # u10 data
      u10_stid <- ncvar_get(u10, "u10", 
                            start = c(wrf_i, wrf_j, 1),
                            count = c(1, 1, -1))
      # v10 data
      v10_stid <- ncvar_get(v10, "v10", 
                            start = c(wrf_i, wrf_j, 1),
                            count = c(1, 1, -1))
      # bind and save
      stid <- rep(stids[j], length(u10_ts))
      df <- data.frame(stid = stids[j],
                       ts = u10_ts,
                       u10 = u10_stid, 
                       v10 = v10_stid, 
                       stringsAsFactors = FALSE)
      station <- bind_rows(station, df)
      saveRDS(station, station_path)
    }
    # close connections - probably should? overwritten anyway..
    nc_close(u10)
    nc_close(v10)
    pb$tick()
  }
}
  
#------------------------------------------------------------------------------

#-- Extract CM3 WRF Output ----------------------------------------------------
library(ncdf4)
library(dplyr)
library(lubridate)
{
  # setup years for iteration 
  # "historical" period
  h_start <- ymd("1980-01-01")
  h_end <- ymd("2005-12-31")
  # "rcp" period
  rcp_start <- ymd("2006-01-01")
  rcp_end <- ymd("2100-01-01")
  h_years <- year(h_start):year(h_end)
  rcp_years <- year(rcp_start):year(rcp_end)
  
  # WRF matrix indices
  stid_wrf_ind <- readRDS(file.path(datadir, "stid_wrf_ind.Rds"))
  
  # u10 and v10 directories (from external drive)
  u10_dir <- file.path(wrf_output_dir, "CM3_u10")
  v10_dir <- file.path(wrf_output_dir, "CM3_v10")
  
  # initialize data frames for saving historical components
  for(i in seq_along(stids)){
    save_path <- file.path(cm3_raw_dir, 
                           paste0(stids[i], "_cm3h_raw.Rds"))
    df <- data.frame(stid = character(),
                     ts = ymd_hms(),
                     u10 = numeric(),
                     v10 = numeric(),
                     stringsAsFactors = FALSE)
    saveRDS(df, save_path)
  }
  pb <- progress_bar$new(total = length(h_years),
                         format = " Extracting historical CM3 output [:bar] :percent")
  # loop through "historical" .nc files and save 
  for(i in seq_along(h_years)){
    # open connections
    u10_fname <- "u10_hourly_wrf_GFDL-CM3_historical_"
    u10_path <- file.path(u10_dir, paste0(u10_fname, h_years[i], ".nc"))
    v10_fname <- "v10_hourly_wrf_GFDL-CM3_historical_"
    v10_path <- file.path(v10_dir, paste0(v10_fname, h_years[i], ".nc"))
    u10 <- nc_open(u10_path)
    v10 <- nc_open(v10_path)
    # u10 time vectors
    u10_start <- ymd_hms(paste0(substr(u10$dim$time$units, 13, 22), " 00:00:00"))
    u10_ts <- ncvar_get(u10, varid = "time")
    u10_ts <- u10_start + hours(u10_ts)
    
    for(j in seq_along(stids)){
      station_path <- file.path(cm3_raw_dir, 
                                paste0(stids[j], "_cm3h_raw.Rds"))
      station <- readRDS(station_path)
      # extract data
      wrf_i <- stid_wrf_ind$wrf_i[stid_wrf_ind$stid == stids[j]]
      wrf_j <- stid_wrf_ind$wrf_j[stid_wrf_ind$stid == stids[j]]
      # u10 data
      u10_stid <- ncvar_get(u10, "u10", 
                            start = c(wrf_i, wrf_j, 1),
                            count = c(1, 1, -1))
      # v10 data
      v10_stid <- ncvar_get(v10, "v10", 
                            start = c(wrf_i, wrf_j, 1),
                            count = c(1, 1, -1))
      # bind and save
      stid <- rep(stids[j], length(u10_ts))
      df <- data.frame(stid = stids[j],
                       ts = u10_ts,
                       u10 = u10_stid, 
                       v10 = v10_stid, 
                       stringsAsFactors = FALSE)
      station <- bind_rows(station, df)
      saveRDS(station, station_path)
    }
    # close connections - probably should? overwritten anyway..
    nc_close(u10)
    nc_close(v10)
    pb$tick()
  }
  
  # initialize data frames for saving future components
  for(i in seq_along(stids)){
    save_path <- file.path(cm3_raw_dir, 
                           paste0(stids[i], "_cm3f_raw.Rds"))
    df <- data.frame(stid = character(),
                     ts = ymd_hms(),
                     u10 = numeric(),
                     v10 = numeric(),
                     stringsAsFactors = FALSE)
    saveRDS(df, save_path)
  }
  pb <- progress_bar$new(total = length(rcp_years),
                         format = " Extracting future CM3 output [:bar] :percent")
  # loop through "rcp" .nc files and save 
  for(i in seq_along(rcp_years)){
    # open connections
    u10_fname <- "u10_hourly_wrf_GFDL-CM3_rcp85_"
    u10_path <- file.path(u10_dir, paste0(u10_fname, rcp_years[i], ".nc"))
    v10_fname <- "v10_hourly_wrf_GFDL-CM3_rcp85_"
    v10_path <- file.path(v10_dir, paste0(v10_fname, rcp_years[i], ".nc"))
    u10 <- nc_open(u10_path)
    v10 <- nc_open(v10_path)
    # u10 time vectors
    u10_start <- ymd_hms(paste0(substr(u10$dim$time$units, 13, 22), " 00:00:00"))
    u10_ts <- ncvar_get(u10, varid = "time")
    u10_ts <- u10_start + hours(u10_ts)
    
    for(j in seq_along(stids)){
      station_path <- file.path(cm3_raw_dir, 
                                paste0(stids[j], "_cm3f_raw.Rds"))
      station <- readRDS(station_path)
      # extract data
      wrf_i <- stid_wrf_ind$wrf_i[stid_wrf_ind$stid == stids[j]]
      wrf_j <- stid_wrf_ind$wrf_j[stid_wrf_ind$stid == stids[j]]
      # u10 data
      u10_stid <- ncvar_get(u10, "u10", 
                            start = c(wrf_i, wrf_j, 1),
                            count = c(1, 1, -1))
      # v10 data
      v10_stid <- ncvar_get(v10, "v10", 
                            start = c(wrf_i, wrf_j, 1),
                            count = c(1, 1, -1))
      # bind and save
      stid <- rep(stids[j], length(u10_ts))
      df <- data.frame(stid = stids[j],
                       ts = u10_ts,
                       u10 = u10_stid, 
                       v10 = v10_stid, 
                       stringsAsFactors = FALSE)
      station <- bind_rows(station, df)
      saveRDS(station, station_path)
    }
    # close connections - probably should? overwritten anyway..
    nc_close(u10)
    nc_close(v10)
    pb$tick()
  }
}

#------------------------------------------------------------------------------

#-- Extract CCSM4 WRF Output --------------------------------------------------
{
  # setup years for iteration 
  # "historical" period
  h_start <- ymd("1980-01-01")
  h_end <- ymd("2005-12-31")
  # "rcp" period
  rcp_start <- ymd("2006-01-01")
  rcp_end <- ymd("2100-01-01")
  h_years <- year(h_start):year(h_end)
  rcp_years <- year(rcp_start):year(rcp_end)
  
  # WRF matrix indices
  stid_wrf_ind <- readRDS(file.path(datadir, "stid_wrf_ind.Rds"))
  
  # u10 and v10 directories (from external drive)
  u10_dir <- file.path(wrf_output_dir, "CCSM4_u10")
  v10_dir <- file.path(wrf_output_dir, "CCSM4_v10")
  
  # initialize data frames for saving components
  for(i in seq_along(stids)){
    save_path <- file.path(ccsm4_raw_dir, 
                           paste0(stids[i], "_ccsm4h_raw.Rds"))
    df <- data.frame(stid = character(),
                     ts = ymd_hms(),
                     u10 = numeric(),
                     v10 = numeric(),
                     stringsAsFactors = FALSE)
    saveRDS(df, save_path)
  }
  pb <- progress_bar$new(total = length(h_years),
                         format = " Extracting historical CCSM4 output [:bar] :percent")
  # loop through "historical" .nc files and save 
  for(i in seq_along(h_years)){
    # open connections
    u10_fname <- "u10_hourly_wrf_NCAR-CCSM4_historical_"
    u10_path <- file.path(u10_dir, paste0(u10_fname, h_years[i], ".nc"))
    v10_fname <- "v10_hourly_wrf_NCAR-CCSM4_historical_"
    v10_path <- file.path(v10_dir, paste0(v10_fname, h_years[i], ".nc"))
    u10 <- nc_open(u10_path)
    v10 <- nc_open(v10_path)
    # u10 time vectors
    u10_start <- ymd_hms(paste0(substr(u10$dim$time$units, 13, 22), " 00:00:00"))
    u10_ts <- ncvar_get(u10, varid = "time")
    u10_ts <- u10_start + hours(u10_ts)
    
    for(j in seq_along(stids)){
      station_path <- file.path(ccsm4_raw_dir, 
                                paste0(stids[j], "_ccsm4h_raw.Rds"))
      station <- readRDS(station_path)
      # extract data
      wrf_i <- stid_wrf_ind$wrf_i[stid_wrf_ind$stid == stids[j]]
      wrf_j <- stid_wrf_ind$wrf_j[stid_wrf_ind$stid == stids[j]]
      # u10 data
      u10_stid <- ncvar_get(u10, "u10", 
                            start = c(wrf_i, wrf_j, 1),
                            count = c(1, 1, -1))
      # v10 data
      v10_stid <- ncvar_get(v10, "v10", 
                            start = c(wrf_i, wrf_j, 1),
                            count = c(1, 1, -1))
      # bind and save
      stid <- rep(stids[j], length(u10_ts))
      df <- data.frame(stid = stids[j],
                       ts = u10_ts,
                       u10 = u10_stid, 
                       v10 = v10_stid, 
                       stringsAsFactors = FALSE)
      station <- bind_rows(station, df)
      saveRDS(station, station_path)
    }
    # close connections - probably should? overwritten anyway..
    nc_close(u10)
    nc_close(v10)
    pb$tick()
  }
  
  # initialize data frames for saving future components
  for(i in seq_along(stids)){
    save_path <- file.path(ccsm4_raw_dir, 
                           paste0(stids[i], "_ccsm4f_raw.Rds"))
    df <- data.frame(stid = character(),
                     ts = ymd_hms(),
                     u10 = numeric(),
                     v10 = numeric(),
                     stringsAsFactors = FALSE)
    saveRDS(df, save_path)
  }
  pb <- progress_bar$new(total = length(rcp_years),
                         format = " Extracting future CCSM4 output [:bar] :percent")
  # loop through "rcp" .nc files and save 
  for(i in seq_along(rcp_years)){
    # open connections
    u10_fname <- "u10_hourly_wrf_NCAR-CCSM4_rcp85_"
    u10_path <- file.path(u10_dir, paste0(u10_fname, rcp_years[i], ".nc"))
    v10_fname <- "v10_hourly_wrf_NCAR-CCSM4_rcp85_"
    v10_path <- file.path(v10_dir, paste0(v10_fname, rcp_years[i], ".nc"))
    u10 <- nc_open(u10_path)
    v10 <- nc_open(v10_path)
    # u10 time vectors
    u10_start <- ymd_hms(paste0(substr(u10$dim$time$units, 13, 22), " 00:00:00"))
    u10_ts <- ncvar_get(u10, varid = "time")
    u10_ts <- u10_start + hours(u10_ts)
    
    for(j in seq_along(stids)){
      station_path <- file.path(ccsm4_raw_dir, 
                                paste0(stids[j], "_ccsm4f_raw.Rds"))
      station <- readRDS(station_path)
      # extract data
      wrf_i <- stid_wrf_ind$wrf_i[stid_wrf_ind$stid == stids[j]]
      wrf_j <- stid_wrf_ind$wrf_j[stid_wrf_ind$stid == stids[j]]
      # u10 data
      u10_stid <- ncvar_get(u10, "u10", 
                            start = c(wrf_i, wrf_j, 1),
                            count = c(1, 1, -1))
      # v10 data
      v10_stid <- ncvar_get(v10, "v10", 
                            start = c(wrf_i, wrf_j, 1),
                            count = c(1, 1, -1))
      # bind and save
      stid <- rep(stids[j], length(u10_ts))
      df <- data.frame(stid = stids[j],
                       ts = u10_ts,
                       u10 = u10_stid, 
                       v10 = v10_stid, 
                       stringsAsFactors = FALSE)
      station <- bind_rows(station, df)
      saveRDS(station, station_path)
    }
    # close connections - probably should? overwritten anyway..
    nc_close(u10)
    nc_close(v10)
    pb$tick()
  }
}

#------------------------------------------------------------------------------
