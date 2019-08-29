# Script Summary
#   example figures for climatology website
#
# Example wind roses
#
# Example monthly average barplots
#
# Output files:
#   /figures/website_examples/PAFA_windrose.png
#   /figures/website_examples/PABR_windrose.png
#   /figures/website_examples/PAFA_mo_speeds.png
#   /figures/website_examples/PABR_mo_speeds.png



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
  require(openair)
  
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





pafb_jan <- pafb %>%
  filter(format(t_round, "%m") == "01")
  
pafb_jan %>%
  filter(drct == 0) %>%
  ggplot(aes(sped_adj)) + 
  geom_histogram()

pafb_jan %>%
  filter(sped == 0) %>%
  group_by(drct) %>%
  summarise(count = n())

pafb_jan$drct[pafb_jan$sped_adj == 0] <- NA
