# Script summary
#
# Convert velocity components
#   To wind speeds in MPH and direction in degrees
#
# Output files:
#   /data/WRF/stations_wrf/"stids"_wrf.Rds



#-- Setup ---------------------------------------------------------------------
# Setup workspace
workdir <- getwd()
datadir <- file.path(workdir, "data")
# adjusted ASOS data
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
# WRF data
wrf_data_dir <- file.path(datadir, "WRF")
wrf_raw_dir <- file.path(wrf_data_dir, "stations_wrf_raw")
wrf_dir <- file.path(wrf_data_dir, "stations_wrf")

#------------------------------------------------------------------------------

#-- Convert components --------------------------------------------------------
# function to apply to convert m/s components to mph and directions
# borrowed from github/environmentalinformatics-marburg/Rsenal
# modified to return mph
uv2wdws <- function(u,v) {
  
  degrees <- function(radians) 180 * radians / pi
  
  mathdegs <- degrees(atan2(v, u))
  wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
  wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
  ws <- sqrt(u^2 + v^2) * 2.23694
  
  return(cbind(wd, ws))
  
}

# loop through raw output, convert, save
wrf_raw_paths <- file.path(wrf_raw_dir, list.files(wrf_raw_dir))
for(i in seq_along(wrf_raw_paths)){
  wrf_raw <- readRDS(wrf_raw_paths[i])
  wdws <- uv2wdws(wrf_raw$u10, wrf_raw$v10)
  wrf <- cbind(wrf_raw[, 1:2], wdws)
  names(wrf)[3:4] <- c("drct", "sped")
  stid <- wrf_raw[1, 1]
  save_path <- file.path(wrf_dir, paste0(stid, "_wrf.Rds"))
  saveRDS(wrf, save_path)
}