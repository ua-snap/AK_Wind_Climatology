# save wrf_adj as CSV with adjusted ERA5 as historical and WRF GCMs as future
# option to either save as individual CSVs (by STID) or save as one large CSV

#-- Single CSV ----------------------------------------------------------------
# parse Args
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("One (and only one) argument must be supplied ('stid' or 'single')", call.=FALSE)
}
if (!(args[1] %in% c("stid", "single"))) {
  stop("Argument must be either 'stid' or 'single'", call.=FALSE)
}

# make single df
mk_wrf_df <- function(era_adj, wrf_wdws, wrf_adj) {
  wrf_stids <- names(wrf_adj$CM3h)
  # combine ERA-Interim data
  era_df <- bind_rows(era_adj) %>%
    mutate(gcm = "ERA") %>%
    rename(ws = sped_adj, wd = drct) %>%
    select(gcm, stid, ts, ws, wd)
  
  # combine all mods of each location
  wrf_df <- parallel::mclapply(seq_along(wrf_adj$CM3h), function(i) {
    # gather vectors of wind directions
    cm3f_wd <- wrf_wdws$CM3f[[i]] %>%
      pull(wd)
    ccsm4f_wd <- wrf_wdws$CCSM4f[[i]] %>%
      pull(wd)
    
    # gather data frames of adjusted speed, combine with directions
    cm3f <- wrf_adj$CM3f[[i]] %>%
      mutate(ts = attr(wrf_adj$CM3f, "ts"),
             gcm = "CM3f",
             wd = cm3f_wd)
    ccsm4f <- wrf_adj$CCSM4f[[i]] %>%
      mutate(ts = attr(wrf_adj$CCSM4f, "ts"),
             gcm = "CCSM4f",
             wd = ccsm4f_wd)
    
    df <- rbind(cm3f, ccsm4f) %>%
      filter(ts >= lb)
    df$stid <- wrf_stids[i]
    df
  }, mc.cores = 32) %>%
    bind_rows %>%
    rename(ws = sim_adj) %>%
    select(gcm, stid, ts, ws, wd)
  
  bind_rows(era_df, wrf_df) %>% 
    filter(ts < ub)
}

# save by station
save_by_stid <- function(era_adj, wrf_wdws, wrf_adj) {
  stids <- names(wrf_adj$CM3h)
  # combine all mods of each location
  save_df <- parallel::mclapply(seq_along(stids), function(i) {
    stid <- stids[i]
    # prep ERA data
    era_df <- era_adj[[stid]] %>%
      mutate(gcm = "ERA") %>%
      rename(ws = sped_adj, wd = drct) %>%
      select(gcm, stid, ts, ws, wd)
    # gather vectors of wind directions
    cm3f_wd <- wrf_wdws$CM3f[[stid]] %>%
      pull(wd)
    ccsm4f_wd <- wrf_wdws$CCSM4f[[stid]] %>%
      pull(wd)
    
    # gather data frames of adjusted speed, combine with directions
    cm3 <- wrf_adj$CM3f[[stid]] %>%
      mutate(ts = attr(wrf_adj$CM3f, "ts"),
             gcm = "CM3",
             wd = cm3f_wd,
             stid = stid) %>%
      filter(ts >= lb & ts < ub) %>%
      rename(ws = sim_adj) %>%
      select(gcm, stid, ts, ws, wd)
    ccsm4 <- wrf_adj$CCSM4f[[stid]] %>%
      mutate(ts = attr(wrf_adj$CCSM4f, "ts"),
             gcm = "CCSM4",
             wd = ccsm4f_wd,
             stid = stid) %>%
      filter(ts >= lb & ts < ub) %>%
      rename(ws = sim_adj) %>%
      select(gcm, stid, ts, ws, wd)
    
    # combine with ERA and lose unnecessary precision
    cm3 <- rbind(era_df, cm3) %>%
      mutate(ws = round(ws, 2),
             wd = round(wd, 2))
    ccsm4 <- rbind(era_df, ccsm4) %>%
      mutate(ws = round(ws, 2),
             wd = round(wd, 2))
    
    data.table::fwrite(cm3, paste0("../data/cw/wrf_adj/CM3_", stid, ".csv"))
    data.table::fwrite(ccsm4, paste0("../data/cw/wrf_adj/CCSM4_", stid, ".csv"))
  }, mc.cores = 32)
}

# load data used in both
load_data <- function() {
  cat("Loading adjusted and unadjusted WRF GCM output\n")
  te <- system.time({
    wrf_adj <- readRDS("../AK_Wind_Climatology_aux/data/wrf_adj.Rds")
    wrf_wdws <- readRDS("../AK_Wind_Climatology_aux/data/wrf_wdws.Rds")
  })
  cat(paste0("WRF output loaded, ", round(te[3], 1), " seconds\n"))
  
  cat("Loading adjusted ERA-Interim \n")
  te <- system.time({
    era_adj <- readRDS("../AK_Wind_Climatology_aux/data/era_adj.Rds")
  })
  cat(paste0("Adjusted ERA-Interim output loaded, ", round(te[3], 1), " seconds\n"))
  list(wrf_adj, wrf_wdws, era_adj)
}

#------------------------------------------------------------------------------

#-- Single CSV ----------------------------------------------------------------
save_single <- function() {
  cat("Unpacking list of WRF output\n")
  te <- system.time({
    wrf_df <- mk_wrf_df(era_adj, wrf_wdws, wrf_adj)
  })
  cat(paste0("WRF output unpacked, ", round(te[3], 1), " seconds\n"))
  
  cat("Saving adjusted WRF output as a wrf_adj.csv\n")
  te <- system.time({
    data.table::fwrite(wrf_df, "../AK_Wind_Climatology_aux/data/wrf_adj.csv")
  })
  cat(paste0("WRF output saved, ", round(te[3], 1), " seconds\n"))
}

#------------------------------------------------------------------------------

#-- STID CSVs -----------------------------------------------------------------
save_stid <- function() {
  cat("Saving WRF output by station id\n")
  te <- system.time({
    save_wrf <- save_by_stid(era_adj, wrf_wdws, wrf_adj)
  })
  cat(paste0("WRF output saved, ", round(te[3], 1), " seconds\n"))
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(lubridate)
})

# lower bound for future period of GCM runs
#   ERA-Interim ends after 2014-12-31
lb <- ymd("2015-01-01")
ub <- ymd("2100-01-01")

loaded <- load_data()
wrf_adj <- loaded[[1]]
wrf_wdws <- loaded[[2]]
era_adj <- loaded[[3]]

if(args[1] == "single") save_single() else save_stid()

#------------------------------------------------------------------------------
