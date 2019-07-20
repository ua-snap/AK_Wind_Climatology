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
library(progress)

# Setup workspace
workdir <- getwd()
datadir <- file.path(workdir, "data")
# adjusted ASOS data
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
# WRF data
era_raw_dir <- file.path(datadir, "ERA_stations_raw")
era_dir <- file.path(datadir, "ERA_stations")
cm3_raw_dir <- file.path(datadir, "CM3_stations_raw")
cm3_dir <- file.path(datadir, "CM3_stations")
ccsm4_raw_dir <- file.path(datadir, "CCSM4_stations_raw")
ccsm4_dir <- file.path(datadir, "CCSM4_stations")

# helper functions for qmapping
helpers <- file.path(workdir, "code/helpers.R")
source(helpers)

#------------------------------------------------------------------------------

#-- Convert ERA-Interim components --------------------------------------------
# loop through raw output, convert, save
era_raw_paths <- file.path(era_raw_dir, list.files(era_raw_dir))
pb <- progress_bar$new(total = length(era_raw_paths),
                       format = " Converting ERA components [:bar] :percent")
for(i in seq_along(era_raw_paths)){
  era_raw <- readRDS(era_raw_paths[i])
  wdws <- uv2wdws(era_raw$u10, era_raw$v10)
  era <- cbind(era_raw[, 1:2], wdws)
  names(era)[3:4] <- c("drct", "sped")
  stid <- era_raw[1, 1]
  save_path <- file.path(era_dir, paste0(stid, "_era.Rds"))
  saveRDS(era, save_path)
  pb$tick()
}

#------------------------------------------------------------------------------

#-- Convert CM3 components ----------------------------------------------------
# loop through raw output, convert, save
# historic CM3 data
cm3h_raw_paths <- list.files(cm3_raw_dir, pattern = "cm3h", full.names = TRUE)
pb <- progress_bar$new(total = length(cm3h_raw_paths),
                       format = " Converting CM3 historic components [:bar] :percent")
for(i in seq_along(cm3h_raw_paths)){
  cm3_raw <- readRDS(cm3h_raw_paths[i])
  wdws <- uv2wdws(cm3_raw$u10, cm3_raw$v10)
  cm3 <- cbind(cm3_raw[, 1:2], wdws)
  names(cm3)[3:4] <- c("drct", "sped")
  stid <- cm3_raw[1, 1]
  save_path <- file.path(cm3_dir, paste0(stid, "_cm3h.Rds"))
  saveRDS(cm3, save_path)
  pb$tick()
}

# future CM3 data
cm3f_raw_paths <- list.files(cm3_raw_dir, pattern = "cm3f", full.names = TRUE)
pb <- progress_bar$new(total = length(cm3f_raw_paths),
                       format = " Converting CM3 future components [:bar] :percent")
for(i in seq_along(cm3f_raw_paths)){
  cm3_raw <- readRDS(cm3f_raw_paths[i])
  wdws <- uv2wdws(cm3_raw$u10, cm3_raw$v10)
  cm3 <- cbind(cm3_raw[, 1:2], wdws)
  names(cm3)[3:4] <- c("drct", "sped")
  stid <- cm3_raw[1, 1]
  save_path <- file.path(cm3_dir, paste0(stid, "_cm3f.Rds"))
  saveRDS(cm3, save_path)
  pb$tick()
}

#------------------------------------------------------------------------------

#-- Convert CCSM4 components --------------------------------------------------
# loop through raw output, convert, save
# historic ccsm4 data
ccsm4h_raw_paths <- list.files(ccsm4_raw_dir, pattern = "ccsm4h", 
                               full.names = TRUE)
pb <- progress_bar$new(total = length(ccsm4h_raw_paths),
                       format = " Converting ccsm4 historic components [:bar] :percent")
for(i in seq_along(ccsm4h_raw_paths)){
  ccsm4_raw <- readRDS(ccsm4h_raw_paths[i])
  wdws <- uv2wdws(ccsm4_raw$u10, ccsm4_raw$v10)
  ccsm4 <- cbind(ccsm4_raw[, 1:2], wdws)
  names(ccsm4)[3:4] <- c("drct", "sped")
  stid <- ccsm4_raw[1, 1]
  save_path <- file.path(ccsm4_dir, paste0(stid, "_ccsm4h.Rds"))
  saveRDS(ccsm4, save_path)
  pb$tick()
}

# future ccsm4 data
ccsm4f_raw_paths <- list.files(ccsm4_raw_dir, pattern = "ccsm4f", 
                               full.names = TRUE)
pb <- progress_bar$new(total = length(ccsm4f_raw_paths),
                       format = " Converting ccsm4 future components [:bar] :percent")
for(i in seq_along(ccsm4f_raw_paths)){
  ccsm4_raw <- readRDS(ccsm4f_raw_paths[i])
  wdws <- uv2wdws(ccsm4_raw$u10, ccsm4_raw$v10)
  ccsm4 <- cbind(ccsm4_raw[, 1:2], wdws)
  names(ccsm4)[3:4] <- c("drct", "sped")
  stid <- ccsm4_raw[1, 1]
  save_path <- file.path(ccsm4_dir, paste0(stid, "_ccsm4f.Rds"))
  saveRDS(ccsm4, save_path)
  pb$tick()
}

#------------------------------------------------------------------------------
