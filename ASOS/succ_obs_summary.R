# Script Summary
#
# Visualizing the frequency of successful wind measurements across sites
# 
# Output files:
#   /figures/pre_process/succ_obs_heatmap.png



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(viridis)
library(lubridate)
library(ggExtra)
library(tidyr)

workdir <- getwd()
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures")

asos_daily_path <- file.path(datadir, "/AK_ASOS_daily_all_stations_19700101_t",
                             "o_20190528.Rds", fsep = "")
asos_daily <- readRDS(asos_daily_path)

stid_ord <- asos_daily %>% 
  group_by(stid) %>%
  summarize(d1 = min(date),
            obs_sum = sum(obs)) %>%
  arrange(d1, obs_sum) %>%
  select(stid) %>% 
  unlist() %>% unname()

asos_daily <- asos_daily %>%
  mutate(stid = factor(stid, levels = stid_ord))

p <-ggplot(asos_daily, aes(date, stid, fill = obs))+
  geom_tile() + 
  scale_fill_viridis(name="Proportion of Observations (# Obs / 24)")
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= "Observations/Day - AK ASOS, All Stations",
             x="Date", y="Station")
# date breaks
date_breaks <- as.Date(c("1970-01-01", "1980-01-01", "1990-01-01", 
                         "2000-01-01", "2010-01-01", "2020-01-01"))
date_labels <- c("1970", "1980", "1990", "2000", "2010", "2020")
p <- p + scale_x_date(breaks = date_breaks, labels = date_labels)
p <-p + theme(legend.position = "bottom")+
  theme(plot.title = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 5)) +
  theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 10)) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 8)) +
  removeGrid() # ggExtra

# save plot
figure_path <- file.path(figdir, "pre_process", "succ_obs_heatmap.png")
ggsave(figure_path, plot = p, device = "png",
       width = 6, height = 10, units = "in")

