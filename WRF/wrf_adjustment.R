# Script summary
#
# Quantile Mapping
#   Loop through WRF output for stations, quantile map
#   to bias correct
#
# Clean CSV files
#   save CSVs of the adjusted WRF output 
#
# Output files:
#   /data/WRF/stations_wrf_adj/"stids"_wrf_adj.Rds
#   /data/WRF/stations_wrf_adj_csv/"stids"_wrf_adj.csv



#-- Setup ---------------------------------------------------------------------
# Setup workspace
library(dplyr)
library(lubridate)

workdir <- getwd()
datadir <- file.path(workdir, "data")
# adjusted ASOS data
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
# WRF data
wrf_data_dir <- file.path(datadir, "WRF")
wrf_dir <- file.path(wrf_data_dir, "stations_wrf")
# helper functions for qmapping
helpers <- file.path(workdir, "code/helpers.R")
source(helpers)

#------------------------------------------------------------------------------

#-- Quantile Mapping ----------------------------------------------------------
# loop through WRF output data files and adjust
wrf_files <- list.files(wrf_dir)
for(i in seq_along(wrf_files)){
  
}
asos_station_path <- file.path(asos_adj_dir, "PAFA.Rds")
asos_station <- readRDS(asos_station_path)
wrf_station_path <- file.path(wrf_dir, "PAFA_wrf.Rds")
asos_wrf <- readRDS(wrf_station_path)

sim <- asos_wrf$sped
obs <- asos_station$sped_adj
sim_adj <- qMapWind(obs, sim)

qm1 <- fitQmapQUANT(obs, sim, wet.day = FALSE)
sim_adj <- doQmapQUANT(sim, qm1)


ggECDF_compare(obs, sim, sim_adj)

# super slow, try rounding values to see if makes differnce
sim2 <- round(sim, 1)
obs2 <- round(obs, 1)
sim_adj2 <- round(sim_adj, 1)
# way better
ggECDF_compare(obs2, sim2, sim_adj2)
