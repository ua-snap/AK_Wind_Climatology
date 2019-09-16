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
library(dplyr)
library(lubridate)

workdir <- getwd()
datadir <- file.path(workdir, "data")
# local WRF data for station-based WRF output
era_raw_dir <- file.path(datadir, "ERA_stations_raw")
# WRF output dir (external drive)
wrf_output_dir <- "F:/AK_Wind_Climatology/data/WRF_output"

# select stations meta data
asos_meta_path <- file.path(datadir, "AK_ASOS_select_stations.Rds")
select_stations <- readRDS(asos_meta_path)
# select station stids
stids <- unique(select_stations$stid)

#------------------------------------------------------------------------------

#-- Station Cells -------------------------------------------------------------
library(raster) 
library(rgdal)

# u10 and v10 historical directories (from external drive)
u10_dir <- file.path(wrf_output_dir, "ERA_u10")
v10_dir <- file.path(wrf_output_dir, "ERA_v10")
# determine where on the WRF grid each station lies via intersecting station
#   locations with raster
# open connection to one of the files 
u10_fname <- "u10_hourly_wrf_ERA-Interim_historical_"
u10_path <- file.path(u10_dir, paste0(u10_fname, 1980, ".nc"))
u10 <- nc_open(u10_path)
# grid is based on centroids, so need to determine correct extent for building
#   corresponding raster
# extract x and y from u10
xc <- ncvar_get(u10, varid = "xc")
yc <- ncvar_get(u10, varid = "yc")
nx <- length(xc)
ny <- length(yc)
# extract time 0 layer of u component
t0 <- ncvar_get(u10, "u10", count = c(nx, ny, 1))
# close connection
nc_close(u10)
# resolution of grid would be the difference between two centroids
res <- xc[2] - xc[1]
# define extent bounds - pad with half of unit grid size 
bounds <- c(min(xc), max(xc), min(yc), max(yc)) + c(-res, res)/2
# Projection from WRF
wrf_crs <- CRS("+units=m +proj=stere +lat_ts=64.0 +lon_0=-152.0 +lat_0=90.0 +x_0=0 +y_0=0 +a=6370000 +b=6370000")
r <- raster(t0, xmn = bounds[1], xmx = bounds[2],
            ymn = bounds[3], ymx = bounds[4],
            crs = wrf_crs)
# CRS for station coordinates
crs1 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
# select coords and transform
stations_sp <- select_stations %>% 
  dplyr::select(lon, lat) %>% 
  SpatialPoints(proj4string = crs1) %>%
  spTransform(wrf_crs)
# interset points with raster
u10_select <- raster::extract(r, stations_sp, method = "simple")
# can try to match extracted wind components
#   not useful if there are duplicate values
anyDuplicated(t0)
# cool, we can match stations based on model output
f1 <- function(x, set){which(set == x, arr.ind = TRUE)}
cell_ind <- sapply(u10_select, f1, t0)
# save these cell indices with stids
stid_wrf_ind <- data.frame(cbind(stids, t(cell_ind))) %>%
  rename(stid = stids, wrf_i = V2, wrf_j = V3) %>%
  mutate(stid = as.character(stids),
         wrf_i = as.numeric(as.character(wrf_i)),
         wrf_j = as.numeric(as.character(wrf_j)))

saveRDS(stid_wrf_ind, file.path(datadir, "stid_wrf_ind.Rds"))
write.csv(stid_wrf_ind, file.path(datadir, "stid_wrf_ind.csv"), 
          row.names = FALSE)

#------------------------------------------------------------------------------

#-- Extract ERA-Interim WRF Output --------------------------------------------
library(ncdf4)
library(dplyr)
library(lubridate)
library(progress)
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
      yc <- stid_wrf_ind$wrf_i[stid_wrf_ind$stid == stids[j]]
      xc <- stid_wrf_ind$wrf_j[stid_wrf_ind$stid == stids[j]]
      # u10 data
      u10_stid <- ncvar_get(u10, "u10", 
                            start = c(xc, yc, 1),
                            count = c(1, 1, -1))
      # v10 data
      v10_stid <- ncvar_get(v10, "v10", 
                            start = c(xc, yc, 1),
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
library(progress)
{
  cm3_raw_dir <- file.path(datadir, "CM3_stations_raw")
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
      yc <- stid_wrf_ind$wrf_i[stid_wrf_ind$stid == stids[j]]
      xc <- stid_wrf_ind$wrf_j[stid_wrf_ind$stid == stids[j]]
      # u10 data
      u10_stid <- ncvar_get(u10, "u10", 
                            start = c(xc, yc, 1),
                            count = c(1, 1, -1))
      # v10 data
      v10_stid <- ncvar_get(v10, "v10", 
                            start = c(xc, yc, 1),
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
      yc <- stid_wrf_ind$wrf_i[stid_wrf_ind$stid == stids[j]]
      xc <- stid_wrf_ind$wrf_j[stid_wrf_ind$stid == stids[j]]
      # u10 data
      u10_stid <- ncvar_get(u10, "u10", 
                            start = c(xc, yc, 1),
                            count = c(1, 1, -1))
      # v10 data
      v10_stid <- ncvar_get(v10, "v10", 
                            start = c(xc, yc, 1),
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
library(ncdf4)
library(dplyr)
library(lubridate)
library(progress)
{
  ccsm4_raw_dir <- file.path(datadir, "CCSM4_stations_raw")
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
      yc <- stid_wrf_ind$wrf_i[stid_wrf_ind$stid == stids[j]]
      xc <- stid_wrf_ind$wrf_j[stid_wrf_ind$stid == stids[j]]
      # u10 data
      u10_stid <- ncvar_get(u10, "u10", 
                            start = c(xc, yc, 1),
                            count = c(1, 1, -1))
      # v10 data
      v10_stid <- ncvar_get(v10, "v10", 
                            start = c(xc, yc, 1),
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
      yc <- stid_wrf_ind$wrf_i[stid_wrf_ind$stid == stids[j]]
      xc <- stid_wrf_ind$wrf_j[stid_wrf_ind$stid == stids[j]]
      # u10 data
      u10_stid <- ncvar_get(u10, "u10", 
                            start = c(xc, yc, 1),
                            count = c(1, 1, -1))
      # v10 data
      v10_stid <- ncvar_get(v10, "v10", 
                            start = c(xc, yc, 1),
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
