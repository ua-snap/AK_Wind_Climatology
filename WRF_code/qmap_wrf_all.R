# quantile map all years of WRF output, future and historical
#-- Setup ---------------------------------------------------------------------
# qMap function to be mclapply'd to historical output
wrap_qMap_his <- function(i, gcm = "CM3") {
  obs <- era_adj[[i]] %>%
    filter(ts < hp[2])
  sim <- wrf_wdws[[paste0(gcm, "h")]][[i]] %>%
    filter(ts >= hp[1])
  qMap(obs$sped_adj, sim$ws, ret.deltas = TRUE, zero_trunc = TRUE)
}

# qMap function to be mclapply'd to future output
wrap_qMap_fut <- function(i, gcm = "CM3") {
  qMap(
    sim = wrf_wdws[[paste0(gcm, "f")]][[i]]$ws,
    use.deltas = wrf_adj[[paste0(gcm, "h")]][[i]]$deltas,
    zero_trunc = TRUE
  )
}

# create ecdfs for each station
mk_ecdfs <- function(i, gcm, pt) {
  if(grepl("h", gcm)) {
    obs_df <- era_adj[[i]]
    sim_df <- wrf_adj[[gcm]][[i]]$df
  } else {
    obs_df <- wrf_adj[[gsub("f", "h", gcm)]][[i]]$df
    sim_df <- wrf_adj[[gcm]][[i]]
  }
  s1 <- sample(1:dim(obs_df)[1], 5e4)
  s2 <- sample(1:dim(sim_df)[1], 5e4)
  stid <- era_adj[[i]]$stid[1]

  p <- ggECDF_compare(
    list(
      obs = obs_df[s1, ncol(obs_df)],
      sim = sim_df$sim[s2],
      sim_adj = sim_df$sim_adj[s2]
    ), 
    p_title = paste0(pt, ", ", stid),
    xmin = 0, var = "ws (mph)"
  )
  fn <- paste0(
    "../AK_Wind_Climatology_aux/figures/qmap/", gcm, "/", stid, ".png"
  )
  ggsave(fn, p, height = 4)
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(gridExtra)
})

source("../Nome_Mets/helpers.R")

cat("Reading in WRF GCM and adjusted ERA-Interim output\n")
te <- system.time({
  wrf_wdws <- readRDS("../AK_Wind_Climatology_aux/data/wrf_wdws.Rds")
  era_adj <- readRDS("../AK_Wind_Climatology_aux/data/era_adj.Rds")
})
cat(paste0("WRF output loaded, ", round(te[3], 1), " seconds\n"))

# lists must be in same order
wrf_stids <- names(wrf_wdws$CM3h)
stopifnot(wrf_stids == names(era_adj))

# start and end dates for historical period
hp <- c(ymd("1980-01-01"), ymd("2006-01-01"))

wrf_adj <- list()
cat("quantile-mapping CM3 historical...\n")
te <- system.time({
  wrf_adj$CM3h <- parallel::mclapply(
    seq_along(wrf_wdws$CM3h), wrap_qMap_his, mc.cores = 32
  )
  attr(wrf_adj$CM3h, "ts") <- wrf_wdws$CM3h$PAAQ %>% 
    filter(ts >= hp[1]) %>%
    pull(ts)
})
cat(paste0("CM3 historical output adjusted, ", round(te[3], 1), " seconds\n"))

cat("quantile-mapping CCSM4 historical...\n")
te <- system.time({
  wrf_adj$CCSM4h <- parallel::mclapply(
    seq_along(wrf_wdws$CCSM4h), wrap_qMap_his, gcm = "CCSM4", mc.cores = 32
  )
  attr(wrf_adj$CCSM4h, "ts") <- wrf_wdws$CCSM4h$PAAQ %>% 
    filter(ts >= hp[1]) %>%
    pull(ts)
})
cat(paste0("CCSM4 historical output adjusted, ", round(te[3], 1), " seconds\n"))

# quantile map projected output
cat("quantile-mapping CM3 future...\n")
te <- system.time({
  wrf_adj$CM3f <- parallel::mclapply(
    seq_along(wrf_wdws$CM3f), wrap_qMap_fut, mc.cores = 32
  )
  attr(wrf_adj$CM3f, "ts") <- wrf_wdws$CM3f$PAAQ %>% 
    pull(ts)
})
cat(paste0("CM3 future output adjusted, ", round(te[3], 1), " seconds\n"))

cat("quantile-mapping CCSM4 future...\n")
te <- system.time({
  wrf_adj$CCSM4f <- parallel::mclapply(
    seq_along(wrf_wdws$CCSM4f), wrap_qMap_fut, gcm = "CCSM4", mc.cores = 32
  )
  attr(wrf_adj$CCSM4f, "ts") <- wrf_wdws$CCSM4f$PAAQ %>% 
    pull(ts)
})
cat(paste0("CCSM4 future output adjusted, ", round(te[3], 1), " seconds\n"))

# save WRF otput
wrf_adj <- lapply(wrf_adj, function(gcm) {
  names(gcm) <- wrf_stids
  gcm
})
cat("Saving adjusted WRF GCM data\n")
te <- system.time({
  wrf_fn <- "../AK_Wind_Climatology_aux/data/wrf_adj.Rds"
  saveRDS(wrf_adj, wrf_fn, compress = FALSE)
})
cat(paste0(
  "WRF GCM data saved as wrf_adj.Rds, ", round(te[3], 1), " seconds\n"
))

# generate and save ECDFs
cat("Saving CM3 historical ECDFs\n")
te <- system.time(
  parallel::mclapply(
    seq_along(wrf_wdws$CM3h), mk_ecdfs, 
    gcm = "CM3h", pt = "CM3h to ERA adj", mc.cores = 32
  )
)
cat(paste0("CM3 historical ECDFs saved, ", round(te[3], 1), " seconds\n"))

cat("Saving CM3 future ECDFs\n")
te <- system.time(
  parallel::mclapply(
    seq_along(wrf_wdws$CM3f), mk_ecdfs, 
    gcm = "CM3f", pt = "CM3f to CM3h adj", mc.cores = 32
  )
)
cat(paste0("CM3 Future ECDFs saved, ", round(te[3], 1), " seconds\n"))

cat("Saving CCSM4 historical ECDFs\n")
te <- system.time(
  parallel::mclapply(
    seq_along(wrf_wdws$CCSM4h), mk_ecdfs, 
    gcm = "CCSM4h", pt = "CCSM4h to ERA adj", mc.cores = 32
  )
)
cat(paste0("CCSM4 historical ECDFs saved, ", round(te[3], 1), " seconds\n"))

cat("Saving CCSM4 future ECDFs\n")
te <- system.time(
  parallel::mclapply(
    seq_along(wrf_wdws$CCSM4f), mk_ecdfs, 
    gcm = "CCSM4f", pt = "CCSM4f to CCSM4h adj", mc.cores = 32
  )
)
cat(paste0("CCSM4 future ECDFs saved, ", round(te[3], 1), " seconds\n"))

#------------------------------------------------------------------------------
