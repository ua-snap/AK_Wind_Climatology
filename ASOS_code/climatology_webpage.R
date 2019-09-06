# Script Summary
#   example figures for climatology website
#
# Example wind roses
#
# Example monthly average barplots
#
# Station names for climatology website
#
# Output files:
#   /figures/website_examples/PAFA_windrose.png
#   /figures/website_examples/PABR_windrose.png
#   /figures/website_examples/PAFA_mo_speeds.png
#   /figures/website_examples/PABR_mo_speeds.png
#   /data/AK_comm_winds_sta_names.csv



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)

# Work directory
workdir <- getwd()
datadir <- file.path(workdir, "data")
fig_dir <- file.path(workdir, "figures")
asos_dir <- file.path(datadir, "AK_ASOS_stations_adj")
# website examples directory
ex_dir <- file.path(fig_dir, "website_examples")

# select stations meta
select_stations <- readRDS(file.path(datadir, "AK_ASOS_select_stations.Rds"))
stids <- select_stations$stid

#------------------------------------------------------------------------------

#-- Wind Roses ----------------------------------------------------------------
library(openair)
# id 20 is Fairbanks
saveWindRoses <- function(stid, asos_dir, ex_dir){
  library(openair)
  
  asos <- readRDS(file.path(asos_dir, paste0(stid, ".Rds")))
  save_path <- file.path(ex_dir, paste0(stid, "_windrose.png"))
  png(save_path, width = 2500, height = 2500, res = 300)
  #b <- round((max(asos$sped_adj)*0.57)/5)
  #b <- round(quantile(asos$sped_adj, seq(0, 1, by = 0.01))[100]/4)
  #sv <- summary(asos$sped_adj)
  sort(table())
  gl <- 2 + round((summary(asos$sped_adj)[3] - 4)/2)
  windRose(asos, "sped_adj", "drct", 
           paddle = FALSE, breaks = c(0, 4, 8, 12, 16, 20),
           angle = 10, type = "month", #grid.line = gl,
           key.header = paste0(stid),
           key.footer = "mph")
  dev.off()
}
lapply(stids1, saveWindRoses, asos_dir, ex_dir)

#------------------------------------------------------------------------------

#-- Bar plots -----------------------------------------------------------------
# Monthly speeds
monthly_path <- file.path(datadir, 
                          "AK_ASOS_monthly_select_adj_19800101_to_20150101.Rds")
asos_monthly <- readRDS(monthly_path)

saveMonthlyBarplots <- function(stid1, asos_monthly, ex_dir){
  
  asos_station <- asos_monthly %>%
    filter(stid == stid1) %>%
    mutate(month = as.factor(month(ym_date))) %>%
    group_by(month) %>%
    summarise(avg_sped = mean(avg_sped))
  levels(asos_station$month) <- month.abb[as.numeric(levels(asos_station$month))]
  
  barfill <- "gold1"
  barlines <- "goldenrod2"
  p1 <- ggplot(asos_station, aes(x = month, y = avg_sped)) +
    #geom_errorbar(aes(ymin = avg_sped, ymax = avg_sped + sd_sped, width = 0.2)) +
    geom_bar(stat = "identity", colour = barlines, fill = barfill) +
    ggtitle(stid1) + 
    xlab("Month") + ylab("Average Wind Speed (mph)") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))
  save_path <- file.path(ex_dir, paste0(stid1, "_mo_speeds.png"))
  ggsave(save_path, p1)
}
lapply(stids, saveMonthlyBarplots, asos_monthly, ex_dir)

#------------------------------------------------------------------------------

#-- Station Names -------------------------------------------------------------
library(dyplr)
library(data.table)
select_meta <- fread(file.path(datadir, "AK_ASOS_select_stations.csv"))
# names used in manuscript figs (currently)
pub_names <- fread(file.path(datadir, "AK_ASOS_names_key.csv"))

temp <- select_meta[pub_names, on = "stid"]
setnames(temp, old = c("station_name", "pub_name"), 
         new = c("ASOS Name", "New Name"))

temp[`New Name` == "Merrill Field", `New Name` := "Anchorage Merrill Field"]
temp[`New Name` == "Barter Island", `New Name` := "Kaktovik/Barter Island"]

# for sharing proposed names only
#fwrite(temp[, `ASOS Name`, `New Name`], 
#       file.path(datadir, "new_ASOS_station_names.csv"))
new_select_stations <- temp[, c("stid", "New Name", "lat", "lon")]
setnames(new_select_stations, old = "New Name", new = "station_name")
fwrite(new_select_stations, 
       file.path(datadir, "AK_ASOS_select_stations_new_names.csv"))

#------------------------------------------------------------------------------

#-- Website Validation --------------------------------------------------------
# section for validating the calculations made by Bruce
library(data.table)
library(parallel)
library(dplyr)

accap_dir <- file.path(datadir, "accap_website")
calms <- fread(file.path(accap_dir, "calms.csv"))[, -c("V1")]
means <- fread(file.path(accap_dir, "means.csv"))[, -c("V1")]
monthly_averages <- fread(file.path(accap_dir, "monthly_averages.csv"))[, -c("V1")]
roses <- fread(file.path(accap_dir, "roses.csv"))[, -c("V1")]

setnames(calms, "sid", "stid")
setnames(means, "station", "stid")
setnames(monthly_averages, "sid", "stid")
setnames(roses, "sid", "stid")

# station ids
select_stations <- readRDS(file.path(datadir, "AK_ASOS_select_stations.Rds"))
stids <- select_stations$stid

# compare calms
# calculate calms and means function
getCalm <- function(stid, asos_dir){
  readRDS(file.path(asos_dir, paste0(stid, ".Rds"))) %>%
    mutate(month = month(date)) %>%
    group_by(stid, month) %>% 
    summarise(total = n(),
              calm = as.integer(sum(if_else(sped_adj == 0, 1, 0))),
              percent = calm/total,
              percent = as.numeric(format(round(percent * 100, 1), 
                                          nsmall = 1)),
              mean = round(mean(sped_adj), 1),
              sd = round(sd(sped_adj), 1))
}

# calcualte in parallel
system.time({
  cl <- makeCluster(4)
  clusterEvalQ(cl, {
    library(data.table)
    library(parallel)
    library(dplyr)
  })
  clusterExport(cl, c("stids", "asos_dir", "getCalm"))
  calc <- bind_rows(parLapply(cl, stids, getCalm, asos_dir))
  stopCluster(cl)
  calc <- arrange(calc, stid)
})

# compare
calms <- calms[order(stid)]
calms <- calms[, percent = round(percent, 1)]
# my calculations
calms_calc <- calc %>% select(stid, month, total, calm, percent)
all.equal(unlist(calms_calc), unlist(calms))

# compare means
# Bruce calcs
means <- means[order(stid)]
# my calculations
means_calc <- calc %>% select(stid, mean, month, sd)
all.equal(unlist(means_calc), unlist(means))

# compare monthly averages
# calculate calms and means function
getMoAvgs <- function(stid, asos_dir){
  readRDS(file.path(asos_dir, paste0(stid, ".Rds"))) %>%
    mutate(year = year(t_round),
           month = month(t_round)) %>%
    group_by(stid, year, month) %>% 
    summarise(speed = round(mean(sped_adj), 1))
}

# calcualte in parallel
system.time({
  cl <- makeCluster(4)
  clusterEvalQ(cl, {
    library(data.table)
    library(parallel)
    library(dplyr)
  })
  clusterExport(cl, c("stids", "asos_dir", "getMoAvgs"))
  mo_avg_calc <- bind_rows(parLapply(cl, stids, getMoAvgs, asos_dir))
  stopCluster(cl)
  mo_avg_calc <- arrange(mo_avg_calc, stid)
})

# compare
monthly_averages <- monthly_averages[order(stid)]
# my calculations
all.equal(unlist(mo_avg_calc), unlist(monthly_averages))
diffs <- which(mo_avg_calc$speed != monthly_averages$speed)
max(mo_avg_calc$speed[diffs] - monthly_averages$speed[diffs])

# compare wind roses
# calculate wind rose counts
getRoses <- function(stid, asos_dir){
  breaks <- c(0, 6, 10, 14, 18, 22)
  labels <- c("0-6", "6-10", "14-18", "18-22", "22+")
  
  asos <- readRDS(file.path(asos_dir, paste0(stid, ".Rds"))) %>%
    mutate(month = month(t_round) - 1,
           direction_class = as.integer(drct/10))
  
  temp <- cut(asos$sped_adj, breaks, labels, include.lowest = TRUE)
  temp[is.na(temp)] <- "22+"
  
  asos$speed_range <- temp
  poss_df <- expand.grid(stid, 0:35, 0:12, as.factor(labels), 
                         stringsAsFactors = FALSE)
  names(poss_df) <- c("stid", "direction_class", "month", "speed_range")
  levels(poss_df$speed_range) <- labels
  
  test <- asos %>%
    group_by(month, stid, speed_range, direction_class) %>%
    summarise(count = n(),
              frequency = n()/nrow(asos)) %>%
    full_join(poss_df, by = c("direction_class", "month", "stid", "speed_range")) %>%
    select(count, direction_class, frequency, month, stid, speed_range) %>%
    
    arrange(month, direction_class, speed_range)
}

# calcualte in parallel
system.time({
  cl <- makeCluster(4)
  clusterEvalQ(cl, {
    library(data.table)
    library(parallel)
    library(dplyr)
  })
  clusterExport(cl, c("stids", "asos_dir", "getMoAvgs"))
  mo_avg_calc <- bind_rows(parLapply(cl, stids, getMoAvgs, asos_dir))
  stopCluster(cl)
  mo_avg_calc <- arrange(mo_avg_calc, stid)
})

# compare
monthly_averages <- monthly_averages[order(stid, month, direction_class, speed_range)]
# my calculations
all.equal(unlist(mo_avg_calc), unlist(monthly_averages))
diffs <- which(mo_avg_calc$speed != monthly_averages$speed)
max(mo_avg_calc$speed[diffs] - monthly_averages$speed[diffs])
#------------------------------------------------------------------------------
