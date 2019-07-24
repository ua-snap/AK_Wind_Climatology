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
# CSM3 (historical and future)
#
# CCSM4 (historical and future)
#
# Convert CSV
#   save csv files of "historical" and "future" output (not the same 
#   as in model runs)
#
# Output files:
#   /data/ERA_stations_adj/"stids"_era_adj.Rds
#   /data/ERA_stations_adj_csv/"stids"_era_adj.csv
#   /data/CM3_stations_adj/"stids"_cm3h_adj.Rds
#   /data/CM3_stations_adj/"stids"_cm3f_adj.Rds
#   /data/CM3_stations_adj_csv/"stids"_cm3h_adj.csv
#   /data/CM3_stations_adj_csv/"stids"_cm3f_adj.csv
#   /data/CCSM4_stations_adj/"stids"_ccsm4h_adj.Rds
#   /data/CCSM4_stations_adj/"stids"_ccsm4f_adj.Rds
#   /data/CCSM4_stations_adj_csv/"stids"_ccsm4h_adj.csv
#   /data/CCSM4_stations_adj_csv/"stids"_ccsm4f_adj.csv



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(progress)

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
era_paths <- list.files(era_dir, full.names = TRUE)
pb <- progress_bar$new(total = length(era_raw_paths),
                       format = " Quantile Mapping ERA Speeds [:bar] :percent")
for(i in seq_along(era_paths)){
  era <- readRDS(era_paths[i])
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
  saveRDS(era, era_adj_path)
  pb$tick()
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
   saveRDS(cm3, cm3_adj_path)

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
  saveRDS(cm3, cm3_adj_path)

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
  saveRDS(ccsm4, ccsm4_adj_path)

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
  saveRDS(ccsm4, ccsm4_adj_path)

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

#-- Save CSVs -----------------------------------------------------------------
# CM3 dirs
era_adj_dir <- file.path(datadir, "era_stations_adj")
era_adj_csv_dir <- file.path(datadir, "era_stations_adj_csv")
# era paths
era_paths <- list.files(era_adj_dir, full.names = TRUE)

pb <- progress_bar$new(total = length(era_paths),
                       format = " Creating ERA CSVs [:bar] :percent")
for(i in seq_along(era_paths)){
  # read, filter to target dates, save CSVs
  era <- readRDS(era_paths[i]) %>%
    filter(ts < ymd("2015-01-02"))
  stid <- era$stid[1]
  era_path <- file.path(era_adj_csv_dir, paste0(stid, "_era_adj.csv"))
  write.csv(era, era_path, row.names = FALSE)
  pb$tick()
}

# CM3 dirs
cm3_adj_dir <- file.path(datadir, "CM3_stations_adj")
cm3_adj_csv_dir <- file.path(datadir, "CM3_stations_adj_csv")
# CM3 paths
cm3h_paths <- list.files(cm3_adj_dir, pattern = "cm3h", full.names = TRUE)
cm3f_paths <- list.files(cm3_adj_dir, pattern = "cm3f", full.names = TRUE)
# CCSM4 dirs
ccsm4_adj_dir <- file.path(datadir, "CCSM4_stations_adj")
ccsm4_adj_csv_dir <- file.path(datadir, "CCSM4_stations_adj_csv")
# CCSM4 paths
ccsm4h_paths <- list.files(ccsm4_adj_dir, pattern = "ccsm4h", full.names = TRUE)
ccsm4f_paths <- list.files(ccsm4_adj_dir, pattern = "ccsm4f", full.names = TRUE)

pb <- progress_bar$new(total = length(cm3h_paths),
                       format = " Creating CSVs [:bar] :percent")
# Loop through CM3 paths and save future/hist CSVs
h_start <- ymd_hms("1980-01-01 00:00:00")
h_end <- ymd_hms("2015-01-01 23:59:59")
f_start <- ymd_hms("2066-01-01 00:00:00")
f_end <- ymd_hms("2100-01-01 23:59:59")
for(i in seq_along(cm3h_paths)){
  # read, filter to target dates, save CSVs
  cm3h <- readRDS(cm3h_paths[i])
  cm3f <- readRDS(cm3f_paths[i])
  cm3 <- bind_rows(cm3h, cm3f)
  cm3h <- cm3 %>% filter(ts >= h_start & ts <= h_end)
  cm3f <- cm3 %>% filter(ts >= f_start & ts <= f_end)
  stid <- cm3f$stid[1]
  cm3h_path <- file.path(cm3_adj_csv_dir, paste0(stid, "_cm3h_adj.csv"))
  cm3f_path <- file.path(cm3_adj_csv_dir, paste0(stid, "_cm3f_adj.csv"))
  write.csv(cm3h, cm3h_path, row.names = FALSE)
  write.csv(cm3f, cm3f_path, row.names = FALSE)
  pb$tick()
}

#------------------------------------------------------------------------------

#-- Generate ECDFs ------------------------------------------------------------
# plot and save ECDF comparisons
# ERA-Interim
era_adj_paths <- list.files(era_adj_dir, full.names = TRUE)
pb <- progress_bar$new(total = length(era_raw_paths),
                       format = " Plotting ECDFs from ERA Adjustment [:bar] :percent")
for(i in seq_along(era_adj_paths)){
  era <- readRDS(era_adj_paths[i])
  stid <- era$stid[1]
  asos <- readRDS(file.path(asos_adj_dir, paste0(stid, ".Rds")))
  obs <- asos$sped_adj
  sim <- era$sped
  sim_adj <- era$sped_adj
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
  pb$tick()
}

#------------------------------------------------------------------------------
