# Script summary
#
# Determine station cells
#   find the indices from the WRF output that correspond to 
#   selected stations
#
# Query the WRF ouput
#   extract and save by station
#
# Output files:
#   /data/stid_wrf_ind.Rds
#   /data/WRF/stations_wrf_raw/"stid"_raw.Rds

#-- Setup ---------------------------------------------------------------------
library(ncdf4) 
library(raster) 
library(rgdal)
library(dplyr)
library(lubridate)
library(ggplot2)

workdir <- getwd()
datadir <- file.path(workdir, "data")
stations_wrf_dir_raw <- file.path(datadir, "WRF", "stations_wrf_raw")
stations_wrf_dir <- file.path(datadir, "WRF", "stations_wrf")
stations_wrf_dir_adj <- file.path(datadir, "WRF", "stations_wrf_adj")
# select stations meta data
asos_meta_path <- file.path(datadir, "AK_ASOS_select_stations.Rds")
select_stations <- readRDS(asos_meta_path)
# select station stids
stids <- unique(select_stations$stid)
# u10 and v10 historical directories (from external drive)
wrf_data_dir <- "F:/Wind_Climatology/data/WRF"
u10_dir <- file.path(wrf_data_dir, "u10")
v10_dir <- file.path(wrf_data_dir, "v10")
# setup years for iteration
start_date <- ymd("1979-01-01")
end_date <- ymd("2015-01-01")
target_years <- year(start_date):year(end_date)

#------------------------------------------------------------------------------

#-- Station Cells -------------------------------------------------------------
# determine where on the WRF grid each station lies via intersecting station
#   locations with raster
# open connection to one of the files 
u10_fname <- "u10_hourly_wrf_ERA-Interim_historical_"
u10_path <- file.path(u10_dir, paste0(u10_fname, target_years[1], ".nc"))
u10 <- nc_open(u10_path)
# grid is based on centroids, so need to determine correct extent for building
#   corresponding raster
# extract x and y from u10
xc <- ncvar_get(u10, varid = "xc")
yc <- ncvar_get(u10, varid = "yc")
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
  select(lon, lat) %>% 
  SpatialPoints(proj4string = crs1) %>%
  spTransform(wrf_crs)
# interset points with raster
u10_select <- extract(r, stations_sp, method = "simple")
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

#------------------------------------------------------------------------------

#-- Extract WRF Output --------------------------------------------------------
# initialize data frames for saving components
for(i in seq_along(stids)){
  save_path <- file.path(stations_wrf_dir_raw, paste0(stids[i], "_raw.Rds"))
  df <- data.frame(stid = character(),
                   ts = ymd_hms(),
                   u10 = numeric(),
                   v10 = numeric(),
                   stringsAsFactors = FALSE)
  saveRDS(df, save_path)
}

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
    station_path <- file.path(stations_wrf_dir_raw, 
                              paste0(stids[j], "_raw.Rds"))
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
    stid <- rep(stids[j], length(v10_ts))
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
}

#------------------------------------------------------------------------------
