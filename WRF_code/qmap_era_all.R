# quantile map ERA-Interim output, 
#-- Setup ---------------------------------------------------------------------
# remove spikes from adjusted ASOS data
rm_spikes <- function(df, max_thr = 80) {
  df <- filter(df, df$sped_adj < max_thr)
  wsl <- list(ws = df$sped_adj, ts = df$t_round)
  
  # calculate speed and time differences 
  calc_diffs <- function(olst) {
    lapply(olst, diff)
  }
  # find indices of obs to remove
  rm_obs <- function(dlst) {
    rm_inc <- which(dlst$ws >= 35 & dlst$ts <= 2)
    rm_dec <- which(dlst$ws <= -35 & dlst$ts <= 2)
    union(rm_inc + 1, rm_dec)
  }
  
  wsd <- calc_diffs(wsl)
  rmv <- rm_obs(wsd)
  if(length(rmv)) df[-rmv, ] else df
}

# qMap function to be mclapply'd to historical output
wrap_qMap <- function(i) {
  obs <- asos_adj[[i]] %>%
    filter(t_round < hp)
  sim <- era_adj[[i]] %>%
    filter(ts < hp)
  qmap_df <- qMap(obs$sped_adj, sim$sped, zero_trunc = TRUE)
  sim$sped_adj <- qmap_df$sim_adj
  sim
}

# create ecdfs for each station
mk_ecdfs <- function(i) {
  s1 <- sample(1:dim(asos_adj[[i]])[1], 5e4)
  s2 <- sample(1:314064, 5e4)
  stid <- era_adj[[i]]$stid[1]
  p <- ggECDF_compare(
    list(
      obs = asos_adj[[i]]$sped_adj[s1],
      sim = era_adj[[i]]$sped[s2],
      sim_adj = era_adj[[i]]$sped_adj[s2]
    ), 
    p_title = paste0("ERA to ASOS Adj, ", stid),
    xmin = 0
  )
  fn <- paste0(
    "../AK_Wind_Climatology_aux/figures/qmap/ERA-Interim/", 
    stid, ".png"
  )
  ggsave(fn, p, height = 4)
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
library(dplyr)
library(lubridate)

# end date for historical period
hp <- ymd("2015-01-01")

# use helper functions from Nome_Mets project
source("../Nome_Mets/helpers.R")

# gather ASOS data and adjust
fns <- list.files(
  "../AK_Wind_Climatology_aux/data/AK_ASOS_stations_adj", 
  full.names = TRUE
)

cat("Reading and checking ASOS data files\n")
te <- system.time({
  asos_adj <- parallel::mclapply(fns, readRDS, mc.cores = 32)
  # add station id's
  names(asos_adj) <- unlist(
    lapply(strsplit(fns, split = "/"), function(fv) {
      gsub(".Rds", "", fv[length(fv)])
    })
  )
})
cat(paste0("ASOS data read, ", round(te[3], 1), " seconds\n"))
te <- system.time(
  asos_adj <- lapply(asos_adj, rm_spikes)
)
cat(paste0("ASOS data screened, ", round(te[3], 1), " seconds\n"))

# gather ERA-Interim data that was already adjusted
fns <- list.files(
  "../AK_Wind_Climatology_aux/data/ERA_stations_adj", 
  full.names = TRUE
)

cat("Reading ERA-Interim output 1980-2014\n")
te <- system.time(
  era_adj <- parallel::mclapply(fns, readRDS, mc.cores = 32)
)
cat(paste0("REA-Interim output read, ", round(te[3], 1), " seconds\n"))

# stop if station order in lists doesn't match
stopifnot(names(asos_adj) == unlist(lapply(era_adj, function(df) df$stid[1])))

cat("quantile-mapping ERA-Interim 1980-2014\n")
te <- system.time(
  era_adj <- parallel::mclapply(
    seq_along(era_adj), wrap_qMap, mc.cores = 32
  )
)
cat(paste0("ERA-Interim output adjusted, ", round(te[3], 1), " seconds\n"))

# add station id's
names(era_adj) <- unlist(lapply(era_adj, function(df) {df$stid[1]}))

cat("Saving adjusted ERA-Interim data\n")
te <- system.time({
  era_fn <- "../AK_Wind_Climatology_aux/data/era_adj.Rds"
  saveRDS(era_adj, era_fn, compress = FALSE)
})
cat(paste0("ERA data saved, ", round(te[3], 1), " seconds\n"))

cat("Saving ECDFs\n")
te <- system.time(
  parallel::mclapply(seq_along(era_adj), mk_ecdfs, mc.cores = 32)
)
cat(paste0("ECDFs saved, ", round(te[3], 1), " seconds\n"))

#------------------------------------------------------------------------------
