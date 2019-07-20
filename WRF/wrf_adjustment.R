# Script summary
#
# Quantile Mapping
#   Loop through WRF output for stations, quantile map to bias correct
#   save CSVs of the adjusted WRF output
#   Save figures of ECDF plots
#   Do this for:
#
# ERA-Interim
#
# CSM3
#
# CCSM4
#
# Output files:
#   /data/ERA_stations_adj/"stids"_era_adj.Rds
#   /data/ERA_stations_adj_csv/"stids"_era_adj.csv
#   /data/CM3_stations_adj/"stids"_cm3h_adj.Rds
#   /data/CM3_stations_adj/"stids"_cm3f_adj.Rds
#   /data/CM3_stations_adj_csv/"stids"_cm3_adj.csv
#   /data/CCSM4_stations_adj/"stids"_ccsm4h_adj.Rds
#   /data/CCSM4_stations_adj/"stids"_ccsm4f_adj.Rds
#   /data/CCSM4_stations_adj_csv/"stids"_ccsm4_adj.csv



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(lubridate)

workdir <- getwd()
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures")
# adjusted ASOS data
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
era_dir <- file.path(datadir, "ERA_stations")
era_adj_dir <- file.path(datadir, "ERA_stations_adj")
era_adj_csv_dir <- file.path(datadir, "ERA_stations_adj_csv")

# helper functions for qmapping
helpers <- file.path(workdir, "code/helpers.R")
source(helpers)

#------------------------------------------------------------------------------

#-- Quantile Map ERA-Interim --------------------------------------------------
# loop through ERA output data files and adjust
era_files <- list.files(era_dir)
for(i in seq_along(era_files)){
  era_path <- file.path(era_dir, era_files[i])
  era <- readRDS(era_path)
  stid <- era$stid[1]
  asos_path <- file.path(asos_adj_dir, paste0(stid, ".Rds"))
  asos <- readRDS(asos_path)
  sim <- era$sped 
  obs <- asos$sped_adj
  
  # quantile mapping
  sim_adj <- qMapWind(obs, sim)
  sim_adj[sim_adj < 1] <- 0
  era$sped_adj <- sim_adj
  # save data
  era_adj_path <- file.path(era_adj_dir, 
                            paste0(stid, "_era_adj.Rds"))
  era_adj_csv_path <- file.path(era_adj_csv_dir, 
                                paste0(stid, "_era_adj.csv"))
  saveRDS(era, era_adj_path)
  write.csv(era, era_adj_csv_path, row.names = FALSE)
  
  # plot and save ECDF comparisons
  ecdf_path <- file.path(figdir, "era_adj_ecdfs", paste0(stid, "_era.png"))
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

#-- Quantile Map CM3 ----------------------------------------------------------
cm3_dir <- file.path(datadir, "CM3_stations")
cm3_adj_dir <- file.path(datadir, "CM3_stations_adj")
cm3_adj_csv_dir <- file.path(datadir, "CM3_stations_adj_csv")

cm3h_paths <- list.files(cm3_dir, pattern = "cm3h", full.names = TRUE)
cm3f_paths <- list.files(cm3_dir, pattern = "cm3f", full.names = TRUE)
pb <- progress_bar$new(total = length(cm3h_paths),
                       format = " Quantile Mapping CM3 data [:bar] :percent")
for(i in seq_along(cm3h_paths)){
  cm3 <- readRDS(cm3h_paths[i])
  # remove 1979 for quantile mapping
  cm3 <- cm3 %>% filter(ts >= ymd_hms("1980-01-01 00:00:00"))
  stid <- cm3$stid[1]
  era_path <- file.path(era_adj_dir, paste0(stid, "_era_adj.Rds"))
  era <- readRDS(era_path)
  sim <- cm3$sped 
  obs <- era$sped_adj
  
  # historical quantile mapping
  qmap_obj <- qMapWind(obs, sim, ret.deltas = TRUE)
  sim_adj <- qmap_obj$sim_adj
  sim_adj[sim_adj < 1] <- 0
  cm3$sped_adj <- sim_adj
  # save data
  cm3_adj_path <- file.path(cm3_adj_dir, 
                            paste0(stid, "_cm3h_adj.Rds"))
  cm3_adj_csv_path <- file.path(cm3_adj_csv_dir, 
                                paste0(stid, "_cm3h_adj.csv"))
  saveRDS(cm3, cm3_adj_path)
  write.csv(cm3, cm3_adj_csv_path, row.names = FALSE)
  
  # plot and save ECDF comparisons
  ecdf_path <- file.path(figdir, "cm3_adj_ecdfs", paste0(stid, "_cm3h.png"))
  sim_samp <- sample(length(sim), 100000)
  n <- length(obs)
  if(n > 100000){
    obs_samp <- sample(n, 100000)
  } else {obs_samp <- 1:n}
  p1 <- ggECDF_compare(obs[obs_samp], 
                       sim[sim_samp], 
                       sim_adj[sim_samp], 
                       p_title = paste0(stid, " Historical"))
  ggsave(ecdf_path, p1, width = 6.82, height = 4.58)
  
  cm3 <- readRDS(cm3f_paths[i])
  # remove 1979 for quantile mapping
  cm3 <- cm3 %>% filter(ts >= ymd_hms("1980-01-01 00:00:00"))
  # just check to make sure same station
  stid2 <- cm3$stid[1]
  if(stid2 != stid){print("shit stations don't match");break}
  
  sim <- cm3$sped 
  # future quantile mapping
  sim_adj <- qMapWind(sim = sim, use.deltas = qmap_obj$deltas)
  sim_adj[sim_adj < 1] <- 0
  cm3$sped_adj <- sim_adj
  # save data
  cm3_adj_path <- file.path(cm3_adj_dir, 
                            paste0(stid, "_cm3f_adj.Rds"))
  cm3_adj_csv_path <- file.path(cm3_adj_csv_dir, 
                                paste0(stid, "_cm3f_adj.csv"))
  saveRDS(cm3, cm3_adj_path)
  write.csv(cm3, cm3_adj_csv_path, row.names = FALSE)
  
  # plot and save ECDF comparisons
  ecdf_path <- file.path(figdir, "cm3_adj_ecdfs", paste0(stid, "_cm3f.png"))
  sim_samp <- sample(length(sim), 100000)
  n <- length(obs)
  if(n > 100000){
    obs_samp <- sample(n, 100000)
  } else {obs_samp <- 1:n}
  p1 <- ggECDF_compare(obs[obs_samp], 
                       sim[sim_samp], 
                       sim_adj[sim_samp], 
                       p_title = paste0(stid, " Future"))
  ggsave(ecdf_path, p1, width = 6.82, height = 4.58)
  pb$tick()
}

#------------------------------------------------------------------------------

#-- Quantile Map CCSM4 --------------------------------------------------------
ccsm4_dir <- file.path(datadir, "CCSM4_stations")
ccsm4_adj_dir <- file.path(datadir, "CCSM4_stations_adj")
ccsm4_adj_csv_dir <- file.path(datadir, "CCSM4_stations_adj_csv")

ccsm4h_paths <- list.files(ccsm4_dir, pattern = "ccsm4h", full.names = TRUE)
ccsm4f_paths <- list.files(ccsm4_dir, pattern = "ccsm4f", full.names = TRUE)
pb <- progress_bar$new(total = length(ccsm4h_paths),
                       format = " Quantile Mapping CCSM4 data [:bar] :percent")
for(i in seq_along(ccsm4h_paths)){
  ccsm4 <- readRDS(ccsm4h_paths[i])
  # remove 1979 for quantile mapping
  ccsm4 <- ccsm4 %>% filter(ts >= ymd_hms("1980-01-01 00:00:00"))
  stid <- ccsm4$stid[1]
  era_path <- file.path(era_adj_dir, paste0(stid, "_era_adj.Rds"))
  era <- readRDS(era_path)
  sim <- ccsm4$sped 
  obs <- era$sped_adj
  
  # historical quantile mapping
  qmap_obj <- qMapWind(obs, sim, ret.deltas = TRUE)
  sim_adj <- qmap_obj$sim_adj
  sim_adj[sim_adj < 1] <- 0
  ccsm4$sped_adj <- sim_adj
  # save data
  ccsm4_adj_path <- file.path(ccsm4_adj_dir, 
                            paste0(stid, "_ccsm4h_adj.Rds"))
  ccsm4_adj_csv_path <- file.path(ccsm4_adj_csv_dir, 
                                paste0(stid, "_ccsm4h_adj.csv"))
  saveRDS(ccsm4, ccsm4_adj_path)
  write.csv(ccsm4, ccsm4_adj_csv_path, row.names = FALSE)
  
  # plot and save ECDF comparisons
  ecdf_path <- file.path(figdir, "ccsm4_adj_ecdfs", paste0(stid, "_ccsm4h.png"))
  sim_samp <- sample(length(sim), 100000)
  n <- length(obs)
  if(n > 100000){
    obs_samp <- sample(n, 100000)
  } else {obs_samp <- 1:n}
  p1 <- ggECDF_compare(obs[obs_samp], 
                       sim[sim_samp], 
                       sim_adj[sim_samp], 
                       p_title = paste0(stid, " Historical"))
  ggsave(ecdf_path, p1, width = 6.82, height = 4.58)
  
  ccsm4 <- readRDS(ccsm4f_paths[i])
  # remove 1979 for quantile mapping
  ccsm4 <- ccsm4 %>% filter(ts >= ymd_hms("1980-01-01 00:00:00"))
  # just check to make sure same station
  stid2 <- ccsm4$stid[1]
  if(stid2 != stid){print("shit stations don't match");break}
  
  sim <- ccsm4$sped 
  # future quantile mapping
  sim_adj <- qMapWind(sim = sim, use.deltas = qmap_obj$deltas)
  sim_adj[sim_adj < 1] <- 0
  ccsm4$sped_adj <- sim_adj
  # save data
  ccsm4_adj_path <- file.path(ccsm4_adj_dir, 
                            paste0(stid, "_ccsm4f_adj.Rds"))
  ccsm4_adj_csv_path <- file.path(ccsm4_adj_csv_dir, 
                                paste0(stid, "_ccsm4f_adj.csv"))
  saveRDS(ccsm4, ccsm4_adj_path)
  write.csv(ccsm4, ccsm4_adj_csv_path, row.names = FALSE)
  
  # plot and save ECDF comparisons
  ecdf_path <- file.path(figdir, "ccsm4_adj_ecdfs", paste0(stid, "_ccsm4f.png"))
  sim_samp <- sample(length(sim), 100000)
  n <- length(obs)
  if(n > 100000){
    obs_samp <- sample(n, 100000)
  } else {obs_samp <- 1:n}
  p1 <- ggECDF_compare(obs[obs_samp], 
                       sim[sim_samp], 
                       sim_adj[sim_samp], 
                       p_title = paste0(stid, " Future"))
  ggsave(ecdf_path, p1, width = 6.82, height = 4.58)
  pb$tick()
}

#------------------------------------------------------------------------------
