# Script summary
#   Final quality check of post-correction AK ASOS data
#
# Extreme adjustments
#
# "Discreteness" between periods
#
# Are high speeds legit?



#-- Setup ---------------------------------------------------------------------
# directories
workdir <- getwd()
datadir <- file.path(workdir, "data")
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
cpts_path <- file.path(asos_adj_dir, "cpts_df.Rds")
cpts_df <- readRDS(cpts_path)
stids <- cpts_df$stid[cpts_df$cpts > 0]

#------------------------------------------------------------------------------

#-- Extreme Adjustments -------------------------------------------------------
# loop through files and determine how many times speeds were
#   adjusted by over 5 mph
{result_df <- data.frame(stid = stids,
                         adj_5 = 0,
                         adj_6 = 0,
                         adj_7 = 0,
                         adj_8 = 0, 
                         adj_9 = 0, 
                         adj_10 = 0,
                         adj_15 = 0)
cases <- c(5, 6, 7, 8, 9, 10, 15)
n_geq <- function(geq, x){length(which(x >= geq))}
for(i in seq_along(stids)){
  asos_path <- file.path(asos_adj_dir, paste0(stids[i], ".Rds"))
  asos_df <- readRDS(asos_path) %>%
    mutate(sped_diff = abs(sped - sped_adj))
  counts <- sapply(cases, n_geq, x = asos_df$sped_diff)
  result_df[result_df$stid == stids[i], 2:8] <- counts
}
names(result_df) <- c("STID", "Over 5", "Over 6", "Over 7",
                      "Over 8", "Over 9", "Over 10", "Over 15")}

# Same as above but check for inflation instead of just abs adjustment
{result_df <- data.frame(stid = stids,
                         adj_5 = 0,
                         adj_6 = 0,
                         adj_7 = 0,
                         adj_8 = 0, 
                         adj_9 = 0, 
                         adj_10 = 0,
                         adj_15 = 0)
cases <- c(5, 6, 7, 8, 9, 10, 15)
n_geq <- function(geq, x){length(which(x >= geq))}
for(i in seq_along(stids)){
  asos_path <- file.path(asos_adj_dir, paste0(stids[i], ".Rds"))
  asos_df <- readRDS(asos_path) %>%
    mutate(sped_diff = sped_adj - sped)
  counts <- sapply(cases, n_geq, x = asos_df$sped_diff)
  result_df[result_df$stid == stids[i], 2:8] <- counts
}
names(result_df) <- c("STID", "Over 5", "Over 6", "Over 7",
                      "Over 8", "Over 9", "Over 10", "Over 15")}

#------------------------------------------------------------------------------




### Data "discreteness"
Plotting ECDFs reveals many different step sizes. Just want to verify that this is legit. 
# Example station where "obs" and "sim" data appear 
#   different in discreteness: PADK
asos_path <- file.path(asos_adj_dir, "PADK.Rds")
asos_station <- readRDS(asos_path)
# unadjusted data
sim <- asos_station$sped[asos_station$period == 1]
obs <- asos_station$sped[asos_station$period == 2]
length(unique(sim))
length(unique(obs))