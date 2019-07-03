# Script summary
#
# Determine station cells
#   find the cells from the WRF output that correspond to 
#   selected stations
#
# Output files:
#   

#-- Setup ---------------------------------------------------------------------
library(ncdf4) 
library(raster) 
library(rgdal)
library(dplyr)
library(lubridate)
library(ggplot2)

workdir <- getwd()
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures")
# select stations meta data
asos_meta_path <- file.path(datadir, "AK_ASOS_select_stations.csv")
select_stations <- readRDS(asos_meta_path)
# stids in station meta data
meta_stids <- unique(stations$stid)
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
# determine where on the WRF grid each station lies
# open connection to one of the files 
u10_fname <- "u10_hourly_wrf_ERA-Interim_historical_1998.nc"
u10_path <- file.path(u10_dir, 
                      "u10_hourly_wrf_ERA-Interim_historical_1998.nc")
# open connection for pulling data
u10 <- nc_open(u10_path)