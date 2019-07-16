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
figdir <- file.path(workdir, "figures")
# adjusted ASOS data
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
# WRF data
wrf_dir <- file.path(datadir, "WRF_stations")
wrf_adj_dir <- file.path(datadir, "WRF_stations_adj")
wrf_adj_csv_dir <- file.path(datadir, "WRF_stations_adj_csv")
# helper functions for qmapping
helpers <- file.path(workdir, "code/helpers.R")
source(helpers)

#------------------------------------------------------------------------------

#-- Quantile Mapping ----------------------------------------------------------
# loop through WRF output data files and adjust
wrf_files <- list.files(wrf_dir)
#for(i in seq_along(wrf_files)){
for(i in 18:length(wrf_files)){
  wrf_path <- file.path(wrf_dir, wrf_files[i])
  wrf <- readRDS(wrf_path)
  stid <- wrf$stid[1]
  asos_path <- file.path(asos_adj_dir, paste0(stid, ".Rds"))
  asos <- readRDS(asos_path)
  sim <- wrf$sped 
  obs <- asos$sped_adj
  
  # quantile mapping
  sim_adj <- qMapWind(obs, sim)
  sim_adj[sim_adj < 1] <- 0
  wrf$sped_adj <- sim_adj
  # save data
  wrf_adj_path <- file.path(wrf_adj_dir, 
                            paste0(stid, "_wrf_adj.Rds"))
  wrf_adj_csv_path <- file.path(wrf_adj_csv_dir, 
                                paste0(stid, "_wrf_adj.csv"))
  saveRDS(wrf, wrf_adj_path)
  write.csv(wrf, wrf_adj_csv_path, row.names = FALSE)
  
  # plot and save ECDF comparisons
  ecdf_path <- file.path(figdir, "wrf_adj_ecdfs", paste0(stid, ".png"))
  sim_samp <- sample(length(sim), 100000)
  n <- length(obs)
  if(n > 100000){
    obs_samp <- sample(n, 100000)
  } else {obs_samp <- 1:n}
  p1 <- ggECDF_compare(obs[obs_samp], 
                       sim[sim_samp], 
                       sim_adj[sim_samp], p_title = stid)
  ggsave(ecdf_path, p1, width = 6.82, height = 4.58)
}

#------------------------------------------------------------------------------
