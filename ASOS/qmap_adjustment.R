library(qmap)
library(dplyr)
library(ggplot2)
library(lubridate)

workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")

# changepoints
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")
cpts_path <- file.path(asos_select_adj_dir, "cpts_df.Rds")
cpts_df <- readRDS(cpts_path)
# stations with 1 change point
adj1_stids <- cpts_df$stid[cpts_df$cpts == 1]
# two cp's
adj2_stids <- cpts_df$stid[cpts_df$cpts == 2]

# TESTING
# -----------------------------------------------------------------------------
# have an issue in qmap adjustment with the default implementation of 
#   fitQmapDIST. 
# need to test the various methods on "PACZ" to determine which is best
i <- 2
asos_path <- file.path(asos_select_adj_dir, 
                       paste0(adj1_stids[i], ".Rds"))
asos_hourly_station <- readRDS(asos_path)
cpts_temp <- cpts_df[cpts_df$stid == adj1_stids[i],]
asos_hourly_station <- asos_hourly_station %>% 
  mutate(Set = if_else(date < cpts_temp[1, 2], 1, 2)) %>%
  filter(is.na(sped) == FALSE & is.na(drct) == FALSE &
           sped < 100)

asos_hourly_station$Set <- as.factor(asos_hourly_station$Set)

current <- asos_hourly_station %>% 
  filter(Set == 2) %>% select(sped)
current <- current$sped

biased <- asos_hourly_station %>% 
  filter(Set == 1) %>% select(sped)
biased <- biased$sped

fit1 <- fitQmapQUANT(current, biased, wet.day = FALSE, qstep = 0.01)
asos_hourly_station$sped_adj[asos_hourly_station$Set == 1] <- doQmapQUANT(biased, fit1)

xmax <- 5 * summary(asos_hourly_station$sped_adj)[3]
ggplot(asos_hourly_station, aes(sped_adj, color = Set)) + stat_ecdf(size = 1) + 
  xlab("Wind Speed (MPH)") + ylab(element_blank()) + 
  xlim(c(0, xmax))  + ggtitle(" ")

# need custom function for quantile mapping wind data
qMapWind <- function(obs, sim){
  require(ggplot2)
  q_obs <- quantile(obs, seq(0, 1, length.out = 101), type = 8)
  q_sim <- quantile(sim, seq(0, 1, length.out = 101), type = 8)
  q_diff <- q_sim - q_obs
  # assign quantiles to observations
  qs <- unique(q_sim)
  q_t <- table(q_sim)
  q_ids <- c()
  for(i in 1:length(qs)){
    if(i == 1){
      i_s <- which(sim == qs[i])
      q_mem <- 0
    }else{
      i_s <- which(sim <= qs[i] & sim > qs[i - 1])
    }
    dup_q <- q_t[i]
    n_i <- length(i_s)
    
    # randomly apply quantile id's to the indices of duplicated quantiles
    names(i_s) <- (as.numeric(cut_number(i_s, dup_q)) + q_mem)[sample(1:n_i, n_i)]
    q_ids <- c(q_ids, i_s)
    q_mem <- dup_q + q_mem
  }
  q_ids <- q_ids[order(q_ids)]
  q_adj <- sim - as.numeric(q_diff[paste0((as.numeric(names(q_ids)) - 1), "%")])
  q_adj <- if_else(q_adj < 0, 0, q_adj)
  return(q_adj)
}


test <- qMapWind(current, biased)
temp <- data.frame(sped = c(test, current), 
                   Set = factor(c(rep(1, length(test)), rep(2, length(current)))))
xmax <- 5 * summary(current)[3]
ggplot(temp, aes(sped, color = Set)) + stat_ecdf(size = 1) + 
  xlab("Wind Speed (MPH)") + ylab(element_blank()) + 
  xlim(c(0, xmax))  + ggtitle(" ")

# Qmap Adjustments
# -----------------------------------------------------------------------------
# adjusting sites with one change point
for (i in 1:length(adj1_stids)) {
  asos_path <- file.path(asos_select_adj_dir, 
                         paste0(adj1_stids[i], ".Rds"))
  asos_hourly_station <- readRDS(asos_path)
  asos_hourly_station <- asos_hourly_station[order(ymd_hms(paste0(asos_hourly_station$valid, ":00"))), ]
  
  # change point data
  # filter out observations missing speed or direction
  cpts_temp <- cpts_df[cpts_df$stid == adj1_stids[i], ]
  asos_hourly_station <- asos_hourly_station %>% 
    mutate(Set = if_else(date < cpts_temp[1, 2], 1, 2)) %>%
    filter(is.na(sped) == FALSE & is.na(drct) == FALSE &
           sped < 150)
  # current and biased ts
  recent <- asos_hourly_station$sped[asos_hourly_station$Set == 2]
  set1 <- asos_hourly_station$sped[asos_hourly_station$Set == 1]
  # fit the qmap
  sped_qmap_adj <- qMapWind(recent, set1)
  # add adjusted data to df
  asos_hourly_station$sped_adj <- c(sped_qmap_adj, recent)
  # save to new file
  asos_qmap_adj_path <- file.path(asos_select_adj_dir, 
                                  paste0(adj1_stids[i], "_qmap.Rds"))
  saveRDS(asos_hourly_station, asos_qmap_adj_path)
}


# adjusting sites with two change points
for (i in 1:length(adj2_stids)) {
  asos_path <- file.path(asos_select_adj_dir, 
                         paste0(adj2_stids[i], ".Rds"))
  asos_hourly_station <- readRDS(asos_path)
  # make sure df ordered according to date
  asos_hourly_station <- asos_hourly_station[order(ymd_hms(paste0(asos_hourly_station$valid, ":00"))), ]
  # change point data
  # filter out observations missing speed or direction
  cpts_temp <- cpts_df[cpts_df$stid == adj2_stids[i], ]
  asos_hourly_station <- asos_hourly_station %>% 
    mutate(Set = if_else(date < cpts_temp[1, 3], 2, 3)) %>%
    mutate(Set = if_else(date < cpts_temp[1, 2], 1, Set)) %>%
    filter(is.na(sped) == FALSE & is.na(drct) == FALSE &
           sped < 150)
  # current and biased ts
  recent <- asos_hourly_station$sped[asos_hourly_station$Set == 3]
  set2 <- asos_hourly_station$sped[asos_hourly_station$Set == 2]
  set1 <- asos_hourly_station$sped[asos_hourly_station$Set == 1]
  # fit the qmap
  sped_qmap_adj1 <- qMapWind(recent, set1)
  sped_qmap_adj2 <- qmapWind(recent, set2)

  # add adjusted data to df
  asos_hourly_station$sped_adj <- c(sped_qmap_adj1, sped_qmap_adj2, recent)
  # save to new file
  asos_qmap_adj_path <- file.path(asos_select_adj_dir, 
                                  paste0(adj2_stids[i], "_qmap.Rds"))
  saveRDS(asos_hourly_station, asos_qmap_adj_path)
}


