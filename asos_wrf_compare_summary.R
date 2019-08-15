# Script Summary
#   Compare number of winds above 30 for Barrow and Nome ASOS vs WRF,
#   compare average speeds as well
#
# Output Files
# 



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(gridExtra)

workdir <- getwd()
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures", "ASOS_WRF_compare")
asos_dir <- file.path(datadir, "AK_ASOS_stations_adj")
era_dir <- file.path(datadir, "ERA_stations_adj")
cm3_dir <- file.path(datadir, "CM3_stations_adj")
ccsm4_dir <- file.path(datadir, "CCSM4_stations_adj")

#------------------------------------------------------------------------------

#-- Compare Means and Counts --------------------------------------------------
h_cut <- ymd_hms("2014-12-31 23:00:00")
# Barrow
asos <- readRDS(file.path(asos_dir, "PABR.Rds")) %>%
  filter(t_round <= h_cut) %>%
  rename(ts = t_round)
# era
era <- readRDS(file.path(era_dir, "PABR_era_adj.Rds")) %>%
  filter(ts <= h_cut)
# cm3h
cm3h <- readRDS(file.path(cm3_dir, "PABR_cm3h_adj.Rds")) %>%
  bind_rows(readRDS(file.path(cm3_dir, "PABR_cm3f_adj.Rds"))) %>%
  filter(ts <= h_cut)
# ccsm4h
ccsm4h <- readRDS(file.path(ccsm4_dir, "PABR_ccsm4h_adj.Rds")) %>%
  bind_rows(readRDS(file.path(ccsm4_dir, "PABR_ccsm4f_adj.Rds"))) %>%
  filter(ts <= h_cut)

df <- era %>%
  select(ts, sped_adj) %>%
  rename(ERA = sped_adj) %>%
  left_join(asos, by = "ts") %>%
  select(ts, ERA, sped_adj) %>%
  rename(ASOS = sped_adj) %>%
  left_join(cm3h, by = "ts") %>%
  rename(CM3 = sped_adj) %>%
  select(ts, ERA, ASOS, CM3) %>%
  left_join(ccsm4h, by = "ts") %>%
  rename(CCSM4 = sped_adj) %>%
  select(ts, ERA, ASOS, CM3, CCSM4) %>%
  gather("Source", "Speed", -ts)

means <- df %>% group_by(Source) %>%
  summarise(src.mean = jitter(mean(Speed, na.rm = TRUE)))

asosWrfHist <- function(df, means, name){
  p <- ggplot(df, aes(x = Speed, color = Source)) +
    geom_histogram(fill = "white", position = "identity",
                   alpha = 0.1, size = 1) +
    scale_x_continuous(breaks = seq(0, 60, by = 5)) +
    geom_vline(data = means, aes(xintercept = src.mean , color = Source),
               linetype = "dashed") +
    theme_bw() + xlab("Speed (MPH)") + ylab("Count") +
    theme(legend.position = "top", 
          panel.grid = element_blank()) + 
    ggtitle(name)
  
  u_lim <- ceiling(max(df$Speed, na.rm = TRUE))
  p_in <- df %>% filter(Speed >= 30) %>%
    ggplot(aes(x = Speed, color = Source)) + 
    geom_histogram(fill = "white", size = 1, 
                   breaks = c(30, 32, 34, 36, 38, u_lim)) + 
    stat_bin(geom = "text", size = 3.5, color = "black",
             aes(label = ..count.., group = Source),
             position = position_stack(vjust = 0.5),
             breaks = c(30, 32, 34, 36, 38, u_lim)) +
    scale_x_continuous(limits = c(30, u_lim), 
                       breaks = c(30, 32, 34, 36, 38, u_lim)) +
    theme_bw() + 
    theme(panel.grid = element_blank()) + 
    ggtitle(name) + ylab("Count")
  
  return(list(p, p_in))
}

plst <- asosWrfHist(df, means, "Barrow")
  
fig_path <- file.path(figdir, "speed_hist_all_sources_PABR.png")
ggsave(fig_path, plst[[1]], dev = "png", width = 7, height = 5.5)
fig_path <- file.path(figdir, "speed_hist_all_sources_PABR_over30.png")
ggsave(fig_path, plst[[2]], dev = "png", width = 7, height = 5)

# count_data
PABR_count_df <- df %>%
  filter(Speed >= 30) %>%
  group_by(Source) %>%
  summarise(Count = n()) %>%
  rename(`Barrow Source` = Source)

# Nome
asos <- readRDS(file.path(asos_dir, "PAOM.Rds")) %>%
  filter(t_round <= h_cut) %>%
  rename(ts = t_round)
# era
era <- readRDS(file.path(era_dir, "PAOM_era_adj.Rds")) %>%
  filter(ts <= h_cut)
# cm3h
cm3h <- readRDS(file.path(cm3_dir, "PAOM_cm3h_adj.Rds")) %>%
  bind_rows(readRDS(file.path(cm3_dir, "PAOM_cm3f_adj.Rds"))) %>%
  filter(ts <= h_cut)
# ccsm4h
ccsm4h <- readRDS(file.path(ccsm4_dir, "PAOM_ccsm4h_adj.Rds")) %>%
  bind_rows(readRDS(file.path(ccsm4_dir, "PAOM_ccsm4f_adj.Rds"))) %>%
  filter(ts <= h_cut)

df <- era %>%
  select(ts, sped_adj) %>%
  rename(ERA = sped_adj) %>%
  left_join(asos, by = "ts") %>%
  select(ts, ERA, sped_adj) %>%
  rename(ASOS = sped_adj) %>%
  left_join(cm3h, by = "ts") %>%
  rename(CM3 = sped_adj) %>%
  select(ts, ERA, ASOS, CM3) %>%
  left_join(ccsm4h, by = "ts") %>%
  rename(CCSM4 = sped_adj) %>%
  select(ts, ERA, ASOS, CM3, CCSM4) %>%
  gather("Source", "Speed", -ts)

means <- df %>% group_by(Source) %>%
  summarise(src.mean = jitter(mean(Speed, na.rm = TRUE)))

plst <- asosWrfHist(df, means, "Nome")

fig_path <- file.path(figdir, "speed_hist_all_sources_PAOM.png")
ggsave(fig_path, plst[[1]], dev = "png", width = 7, height = 5.5)
fig_path <- file.path(figdir, "speed_hist_all_sources_PAOM_over30.png")
ggsave(fig_path, plst[[2]], dev = "png", width = 7, height = 5)

# count_data
PAOM_count_df <- df %>%
  filter(Speed >= 30) %>%
  group_by(Source) %>%
  summarise(Count = n()) %>%
  rename(`Nome Source` = Source)

tbl_path <- file.path(figdir, "counts_over_30mph.png")
png(tbl_path, height = 300, width = 200)
p1 <- tableGrob(PABR_count_df, rows = NULL)
p2 <- tableGrob(PAOM_count_df, rows = NULL)
grid.arrange(p1, p2, nrow = 2)
dev.off()

#------------------------------------------------------------------------------

