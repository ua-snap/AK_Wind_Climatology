# Script summary
#
# Convert velocity components to wind speeds/directions in MPH/degrees for:
# ERA-Interim
#
# CM3
#
# CCSM4
#
# Output files:
#   /data/ERA_stations/"stids"_era.Rds
#   /data/CM3_stations/"stids"_cm3.Rds
#   /data/CCSM4_stations/"stids"_ccsm4.Rds




#-- Setup ---------------------------------------------------------------------
# Setup workspace
workdir <- getwd()
datadir <- file.path(workdir, "data")
# adjusted ASOS data
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
# WRF data
era_raw_dir <- file.path(datadir, "ERA_stations_raw")
era_dir <- file.path(datadir, "ERA_stations")

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
era_raw_paths <- file.path(era_raw_dir, list.files(era_raw_dir))
for(i in seq_along(era_raw_paths)){
  era_raw <- readRDS(era_raw_paths[i])
  wdws <- uv2wdws(era_raw$u10, era_raw$v10)
  era <- cbind(era_raw[, 1:2], wdws)
  names(wrf)[3:4] <- c("drct", "sped")
  stid <- era_raw[1, 1]
  save_path <- file.path(era_dir, paste0(stid, "_era.Rds"))
  saveRDS(era, save_path)
}