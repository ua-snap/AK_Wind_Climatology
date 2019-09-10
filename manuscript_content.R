# Script Summary
#   Generate figures to be included in the journal submission
#
# Map of Alaska showing station locations
#
# Example of wind speed time series before and after adjustment
#
# Quantile mapping ECDF 
# 
# Sample time series of winds during specific events
#
# Checkerboard ttest results
#
# Output files:
#   /figures/manuscript/AK_ASOS_station_locations.pdf
#   /figures/manuscript/asos_discont_adj_ex.pdf
#   /figures/manuscript/ERA_CM3_ECDFs.pdf
#   /figures/manuscript/wind_event_ASOS_ERA.pdf
#   /figures/manuscript/wrf_clim_ttest_signif_heatmap.pdf



#-- Setup ---------------------------------------------------------------------
workdir <- getwd()
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures", "manuscript")

#------------------------------------------------------------------------------

#-- AK Map of Stations --------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(sf)
library(USAboundaries)

# load select stations
select_stations <- readRDS(file.path(datadir, "AK_ASOS_select_stations.RDS"))
stids <- select_stations$stid
# alaska sf object
alaska <- us_states(states = "AK", resolution = "high")
ak_sf <- st_transform(alaska, 26935)
# station coordinates as sf object
coords <- as.data.frame(select_stations[, c("stid", "lon", "lat")])
coords <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
coords <- st_transform(coords, 26935)

# tier 2 stations coordinates as sf (commented out)
#coords_2 <- select_stations %>% 
#  filter(tier == 2) %>% 
#  select(lon, lat) %>%
#  as.data.frame()
#coords_2 <- st_as_sf(coords_2, coords = c("lon", "lat"), crs = 4326)
#coords_2 <- st_transform(coords_2, 26935)

# coastal site names
lab_stids <- c("PABA",
               "PABR",
               "PADQ",
               "PAJN",
               "PANC",
               "PAOM",
               "PASI",
               "PASN")

lab_coords <- coords %>%
  filter(stid %in% lab_stids) %>%
  bind_cols(as.data.frame(matrix(unlist(lab_coords$geometry),
                                 ncol = 2, byrow = TRUE))) %>%
  rename(lonl = V1, latl = V2)

lab_coords$site <- c("Kaktovik",
                     "Utqiatvik (Barrow)",
                     "Kodiak",
                     "Juneau",
                     "Anchorage",
                     "Nome",
                     "Sitka",
                     "Saint Paul")
nudx <- c(400000, -700000, 200000, 200000, 550000, -300000, -50000, -100000)
nudy <- c(100000, 0, -300000, 200000, -300000, 50000, -300000, 200000)

# plot
p <- ggplot(data = ak_sf) + geom_sf(fill = "cornsilk", size = 0.25) +
  xlab("Lng") + ylab("Lat") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(size = 10)) +
  geom_text_repel(data = lab_coords, aes(x = lonl, y = latl, label = site),
                  nudge_x = nudx, nudge_y = nudy, size = 2,
                  min.segment.length = 0, box.padding = 0) +
  geom_sf(data = coords, shape = 19, col = "darkred", size = 0.75) 
  

fig_path <- file.path(figdir, "AK_ASOS_select_locations.pdf")
ggsave(fig_path, p, device = "pdf", width = 3.54, height = 2.05)

#------------------------------------------------------------------------------

#-- Discontinuity Adjustment Time Series --------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)

# select/adj data summarized by month
monthly_adj_path <- file.path(datadir, 
                              "AK_ASOS_monthly_select_adj_19800101_to_20150101.Rds")
asos_monthly <- readRDS(monthly_adj_path)
# changepoints
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
cpts_df <- readRDS(file.path(asos_adj_dir, "cpts_df.Rds"))

# x scale to be used for both plots
date_labels <- c("1980", "1985", "1990", "1995", 
                 "2000", "2005", "2010", "2015")
date_breaks <- ymd(c("1980-01-01", "1985-01-01", "1990-01-01", "1995-01-01", 
                     "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01"))

# display time series for PAED and PAFA
stid1 <- "PADK"
asos_station1 <- asos_monthly %>% 
  filter(stid == stid1)

# changepoint info
cpts_temp1 <- cpts_df[cpts_df$stid == stid1, ]
# starting x for mean 1 horizontal line
x1_start1 <- ymd("1980-01-01")
x1_end1 <- cpts_temp1[1, 2]
x2_end1 <- ymd("2015-01-01")
m1_1 <- cpts_temp1[1, 4]
m2_1 <- cpts_temp1[1, 5]

p1 <- ggplot(asos_station1, aes(ym_date, avg_sped, group = 1)) + 
  geom_line(col = "grey") +
  xlim(ymd("1980-01-01"), ymd("2015-01-01")) + 
  scale_x_date(date_labels = date_labels, breaks = date_breaks) + 
  ggtitle("Kodiak Municipal Airport") + 
  geom_vline(xintercept = x1_start1, col = "gray50", 
             lty = 3, size = 1) + 
  geom_vline(xintercept = x2_end1, col = "gray50", 
             lty = 3, size = 1) + 
  geom_vline(xintercept = x1_end1, 
             col = "red", size = 1.5) + 
  geom_segment(aes(x = x1_start1, xend = x1_end1, y = m1_1, yend = m1_1),
               col = "blue") +
  geom_segment(aes(x = x1_end1, xend = x2_end1, y = m2_1, yend = m2_1),
               col = "blue") +
  geom_line(aes(ym_date, avg_sped_adj, group = 1)) + 
  scale_y_continuous(limits = c(5, 25), breaks = c(5, 10, 15, 20, 25)) +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(),
        plot.title = element_text(size = 10)) 

# second station (two discontinuities)
# display time series for PAED and PAFA
stid2 <- "PAEI"
asos_station2 <- asos_monthly %>% 
  filter(stid == stid2)

# changepoint info
cpts_temp2 <- cpts_df[cpts_df$stid == stid2, ]
# starting x for mean 1 horizontal line
x1_start2 <- ymd("1980-01-01")
x1_end2 <- cpts_temp2[1, 2]
x2_end2 <- cpts_temp2[1, 3]
x3_end2 <- ymd("2015-01-01")
m1_2 <- cpts_temp2[1, 4]
m2_2 <- cpts_temp2[1, 5]
m3_2 <- cpts_temp2[1, 6]

p2 <- ggplot(asos_station2, aes(ym_date, avg_sped, group = 1)) + 
  geom_line(col = "grey") +
  xlim(ymd("1980-01-01"), ymd("2015-01-01")) + 
  scale_x_date(date_labels = date_labels, breaks = date_breaks) + 
  ggtitle("Eielson AFB") + 
  geom_vline(xintercept = x1_start2, col = "gray50", 
             lty = 3, size = 1) + 
  geom_vline(xintercept = x3_end2, col = "gray50", 
             lty = 3, size = 1) + 
  geom_vline(xintercept = x1_end2, 
             col = "red", size = 1.5) + 
  geom_vline(xintercept = c(x1_end2, x2_end2), 
             col = "red", size = 1.5) + 
  geom_segment(aes(x = x1_start2, xend = x1_end2, y = m1_2, yend = m1_2),
               col = "blue") +
  geom_segment(aes(x = x1_end2, xend = x2_end2, y = m2_2, yend = m2_2),
               col = "blue") +
  geom_segment(aes(x = x2_end2, xend = x3_end2, y = m3_2, yend = m3_2),
               col = "blue") +
  geom_line(aes(ym_date, avg_sped_adj, group = 1)) +
  scale_y_continuous(limits = c(0, 8), breaks = c(0, 2, 4, 6, 8)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(margin = margin(l = 5, r = 2)),
        plot.title = element_text(size = 10)) 

p <- arrangeGrob(p1, p2, nrow = 2, 
                 left = textGrob("Avg Wind Speed (mph)", 
                                 gp = gpar(fontsize = 10),
                                 rot = 90),
                 bottom = textGrob("Time", gp = gpar(fontsize = 10)))
fig_path <- file.path(figdir, "asos_discont_adj_ex.pdf")
ggsave(fig_path, p, device = "pdf", width = 7.25, height = 2.65)

#------------------------------------------------------------------------------

#-- ERA & CM3 ECDFs -----------------------------------------------------------
library(dplyr)
library(lubridate)

# ggECDF_compare modified for manuscript
ggECDF_compare <- function(obs, sim, sim_adj, p_tag = " ",
                           sim_lab, obs_lab, cols = 1){
  library(gridExtra)
  library(ggplot2)
  library(grid)
  library(wesanderson)
  
  df1 <- data.frame(sped = c(sim, obs),
                    quality = c(rep("1", length(sim)),
                                rep("2", length(obs))))
  
  df2 <- data.frame(sped = c(sim_adj, obs),
                    quality = c(rep("1", length(sim_adj)),
                                rep("2", length(obs))))
  
  # extract legend, code borrowed from SO (for sharing legend between plots)
  # https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  # original data
  xmax <- quantile(obs, probs = seq(0, 1, 1/500))[500] + 5
  p1 <- ggplot(df1, aes(sped, color = quality)) + 
    stat_ecdf(size = 0.5) + 
    xlim(c(0, xmax)) + #scale_color_discrete(name = "  ", 
                        #                    labels = c(sim_lab, obs_lab)) + 
    # ggtitle(p_title) +
    # labs(tag = p_tag) +
    theme_bw() + 
    theme(legend.position = "top",
          plot.title = element_text(vjust = -1),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.text = element_text(size = 8),
          legend.margin = margin(-5, 0, 0, 0),
          plot.margin = unit(c(2, 2, 2, 2), "mm"))
  
  if(cols == 1){
    p1 <- p1 + scale_color_manual(values = c("#FD6467", "#F1BB7B"),
                                  name = "   ",
                                  labels = c(sim_lab, obs_lab))
  } else {
    p1 <- p1 + scale_color_manual(values = c("#5B1A18", "#FD6467"),
                                  name = "   ",
                                  labels = c(sim_lab, obs_lab))
  }

  # corrected data
  p2 <- ggplot(df2, aes(sped, color = quality)) + 
    stat_ecdf(size = 0.5) + 
    xlim(c(0, xmax))  + #ggtitle(" ") + 
    # labs(tag = "  ") +
    theme_bw() +
    theme(plot.title = element_text(vjust = -1),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 7),
          plot.margin = unit(c(2, 2, 2, 2), "mm"))
  
  if(cols == 1){
    p2 <- p2 + scale_color_manual(values = c("#FD6467", "#F1BB7B"),
                                  name = "   ",
                                  labels = c(sim_lab, obs_lab))
  } else {
    p2 <- p2 + scale_color_manual(values = c("#5B1A18", "#FD6467"),
                                  name = "   ",
                                  labels = c(sim_lab, obs_lab))
  }
  
  # legend code adapted from:
  # https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  tmp <- ggplot_gtable(ggplot_build(p1))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  mylegend <- tmp$grobs[[leg]]
  
  p <- arrangeGrob(mylegend,
                   arrangeGrob(p1 + theme(legend.position = "none"),
                               p2 + theme(legend.position = "none"), 
                               nrow = 1,
                               bottom = textGrob("Wind Speed (mph)",
                                                 x = unit(0.55, "npc"),
                                                 vjust = -0.5,
                                                 gp = gpar(fontsize = 8)),
                               left = textGrob("Cumulative Probability",
                                               gp = gpar(fontsize = 8),
                                               rot = 90, hjust = 0.35)),
                   nrow = 2, heights = c(1, 40),
                   top = textGrob(p_tag,
                                  x = unit(0.05, "npc"),
                                  y = unit(0.30, "npc"), 
                                  just = c("left", "top")))
  return(p)
}

asos_dir <- file.path(datadir, "AK_ASOS_stations_adj")
asos <- readRDS(file.path(asos_dir, "PANC.Rds")) %>%
  filter(t_round < ymd("2015-01-01"))
era_dir <- file.path(datadir, "ERA_stations_adj")
era <- readRDS(file.path(era_dir, "PANC_era_adj.Rds")) %>%
  filter(ts < ymd("2015-01-01"))
cm3_dir <- file.path(datadir, "CM3_stations_adj")
cm3 <- readRDS(file.path(cm3_dir, "PANC_cm3h_adj.Rds"))

p1 <- ggECDF_compare(asos$sped_adj, era$sped, era$sped_adj + 0.5, 
                     "A", "ERA-Interim", "ASOS")

obs2 <- era$sped_adj[era$ts < ymd("2006-01-01")]
p2 <- ggECDF_compare(obs2, cm3$sped, cm3$sped_adj + 0.5,
                     "B", "CM3 Historical", "ERA-Interim", 2)

p <- arrangeGrob(p1, p2, nrow = 2)

fig_path <- file.path(figdir, "ERA_CM3_ECDFs.pdf")
ggsave(fig_path, p, dev = "pdf", width = 3.54, height = 4)

#------------------------------------------------------------------------------

#-- High Wind Events ----------------------------------------------------------
# Need John to choose a couple
# John suggests 6-panel figure, with 3 examples from Barrow on left and 3
#   from Nome on right

library(lubridate)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)

# individual event plotting function
plotEvent <- function(t_start, asos, era, retkey = FALSE){
  library(dplyr)
  library(ggplot2)
  
  start <- t_start - hours(48)
  end <- start + hours(192)
  
  asos_event <- asos %>% 
    filter(t_round >= start & t_round <= end) %>%
    select(t_round, sped_adj) %>% 
    rename(ts = t_round, ASOS = sped_adj)
  
  event_df <- era %>% 
    filter(ts >= start & ts <= end) %>%
    select(ts, sped_adj) %>%
    rename(ERA = sped_adj) %>%
    left_join(asos_event, "ts") %>%
    gather("Source", "sped", -ts)
  
  p <- ggplot(event_df, aes(ts, sped, color = Source)) + 
    geom_line(size = 0.75) +
    scale_color_manual(values = c("#E69F00", "#56B4E9")) +
    theme_bw() +
    ggtitle(year(t_start)) +
    theme(axis.title = element_blank(),
          plot.title = element_text(size = 10,
                                    margin = margin(0, 0, 0, 0)),
          legend.position = "bottom")
  
  p1 <- p + theme(legend.position = "none")
  
  if(retkey){
    tmp <- ggplot_gtable(ggplot_build(p))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    mylegend <- tmp$grobs[[leg]]
    return(list(p1, mylegend))
  }else{return(p1)}
  
}

# Barrow
asos_dir <- file.path(datadir, "AK_ASOS_stations_adj")
era_dir <- file.path(datadir, "ERA_stations_adj")
pabr_era <- readRDS(file.path(era_dir, "PABR_era_adj.Rds"))
pabr_asos <- readRDS(file.path(asos_dir, "PABR.Rds"))

pabr_start <- ymd_hms(c("1989-02-25 13:00:00",
                        "1994-02-01 05:00:00",
                        "2000-11-10 06:00:00"))

p1 <- plotEvent(pabr_start[1], pabr_asos, pabr_era, retkey = TRUE)
p2 <- plotEvent(pabr_start[2], pabr_asos, pabr_era)
p3 <- plotEvent(pabr_start[3], pabr_asos, pabr_era)

# Nome
paom_era <- readRDS(file.path(era_dir, "PAOM_era_adj.Rds"))
paom_asos <- readRDS(file.path(asos_dir, "PAOM.Rds"))

paom_start <- ymd_hms(c("1998-04-09 22:00:00",
                        "2006-02-18 21:00:00",
                        "2011-11-08 21:00:00"))

p4 <- plotEvent(paom_start[1], paom_asos, paom_era)
p5 <- plotEvent(paom_start[2], paom_asos, paom_era)
p6 <- plotEvent(paom_start[3], paom_asos, paom_era)

# combine plots
p_pabr <- arrangeGrob(p1[[1]], p2, p3, nrow = 3,
                      top = textGrob("Utqiatvik (Barrow)",
                                     x = unit(0.05, "npc"),
                                     y = unit(0.60, "npc"), 
                                     just = c("left", "top")))
p_paom <- arrangeGrob(p4, p5, p6, nrow = 3,
                      top = textGrob("Nome",
                                     x = unit(0.05, "npc"),
                                     y = unit(0.60, "npc"), 
                                     just = c("left", "top")))
p_main <- arrangeGrob(p_pabr, p_paom, ncol = 2,
                      bottom = textGrob("Date",
                                        x = unit(0.5, "npc"),
                                        vjust = -0.6,
                                        gp = gpar(fontsize = 12)),
                      left = textGrob("Wind Speed (mph)",
                                      gp = gpar(fontsize = 12),
                                      rot = 90, hjust = 0.4))

p <- arrangeGrob(p_main, p1[[2]], nrow = 2, heights = c(30, 1))

fig_path <- file.path(figdir, "wind_event_ASOS_ERA.pdf")
ggsave(fig_path, p, dev = "pdf", width = 7.25, height = 6)

#------------------------------------------------------------------------------

#-- t-test heatmap ----------------------------------------------------------
source(file.path(workdir, "helpers.R"))

stid_names <- read.csv(file.path(datadir, "AK_ASOS_names_key.csv"),
                       stringsAsFactors = FALSE)

cm3_monthly <- readRDS(file.path(datadir, "CM3_clim_monthly.Rds"))
ccsm4_monthly <- readRDS(file.path(datadir, "CCSM4_clim_monthly.Rds"))

stations <- readRDS(file.path(datadir, "AK_ASOS_select_stations.Rds"))
stids <- stations$stid
stid_names <- stations %>% select(stid, station_name)

# CM3
cm3_ttest <- lapply(stids, t_test_stid, cm3_monthly) %>%
  bind_rows() 

# CCSM4
ccsm4_ttest <- lapply(stids, t_test_stid, ccsm4_monthly) %>%
  bind_rows() 

# t-test heatmap results function
#tHeatmap <- function(t_obj, p_mod){

cm3_sig <- cm3_ttest %>% 
  left_join(stations, by = "stid") %>%
  mutate(sig = if_else(p_val <= 0.05 & mean_x < mean_y, 
                       "Future Signif. Higher", 
                       "Future Signif. Lower"),
         sig = if_else(p_val > 0.05, "No Signif. Difference", sig),
         sig = factor(sig, levels = c("No Signif. Difference",
                                      " ",
                                      "Future Signif. Lower", 
                                      "Future Signif. Higher")),
         mo = factor(mo, levels = month.abb),
         dsrc = factor("CM3", levels = c("CM3", "CCSM4"))) %>%
  select(stid, sig, mo, dsrc)

results_df <- ccsm4_ttest %>% 
  left_join(stations, by = "stid") %>%
  mutate(sig = if_else(p_val <= 0.05 & mean_x < mean_y, 
                       "Future Signif. Higher", 
                       "Future Signif. Lower"),
         sig = if_else(p_val > 0.05, "No Signif. Difference", sig),
         sig = factor(sig, levels = c("No Signif. Difference",
                                      " ",
                                      "Future Signif. Lower", 
                                      "Future Signif. Higher")),
         mo = factor(mo, levels = month.abb),
         dsrc = factor("CCSM4", levels = c("CM3", "CCSM4"))) %>%
  select(stid, sig, mo, dsrc) %>%
  bind_rows(cm3_sig) %>%
  left_join(stid_names, by = "stid") %>%
  select(pub_name, sig, mo, dsrc)

# function to increase vertical spacing between legend keys
# @clauswilke
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

# register new key drawing function, 
# the effect is global & persistent throughout the R session
GeomTile$draw_key = draw_key_polygon3

p <- ggplot(results_df, aes(x = mo, y = reorder(pub_name, desc(pub_name)))) +
  geom_tile(aes(fill = sig, color = sig)) + 
  scale_fill_manual(values = c("White", "white", "#46ACC8", "#E58601"),
                    drop = FALSE) + 
  scale_color_manual(values = c("grey", "white", "grey", "grey"),
                     drop = FALSE) +
  scale_x_discrete(breaks = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  ylab("Location") + xlab("Month") + 
  #labs(fill = "Significance\nat \u03B1 = 0.05") +
  theme_bw() + 
  theme(legend.direction = "vertical",
        legend.key.size = unit(0.6, "line"),
        legend.title = element_blank(),
        legend.margin = margin(c(0, 1, 0, 1)),
        legend.text = element_text(margin = margin(l = 3, r = 3),
                                   size = 8),
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 8), 
        strip.background = element_blank(),
        strip.text = element_text(margin = margin(c(1, 0, 1, 0))),
        legend.key = element_rect(color = NA, fill = NA),
        legend.justification = c(0, 0),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2)) +
  facet_wrap(~dsrc)

plot_path <- file.path(figdir, "wrf_clim_ttest_signif_heatmap.pdf")
ggsave(plot_path, p, device = "pdf", height = 7, width = 3.54)

#------------------------------------------------------------------------------

#-- Wind Roses ----------------------------------------------------------------
# Figure 7

library(dplyr)
library(ggplot2)
library(openair)
library(gridExtra)
library(grid)
library(lattice)

source(file.path(workdir, "windRose.R"))
asos_dir <- file.path(datadir, "AK_ASOS_stations_adj")

# Annual Roses
# id 20 is Fairbanks
saveWindRoses <- function(stid, asos_dir){
  gl <- as.numeric(strsplit(stid, " ")[[1]][2])
  stid <- strsplit(stid, " ")[[1]][1]
  
  asos <- readRDS(file.path(asos_dir, paste0(stid, ".Rds")))
  calm <- nrow(asos[asos$sped_adj == 0, ])/nrow(asos)
  asos$sped_adj[asos$drct == 0] <- 0
  
  p <- windRose(asos, "sped_adj", "drct", 
           paddle = FALSE, breaks = c(0, 6, 10, 14, 18, 22),
           angle = 10, grid.line = gl, calm = calm)
  return(p$plot)
}

stids <- c("PANC 2", "PABT 1", "PAFA 1", "PAJN 2",
           "PADK 2", "PAOM 2", "PASN 1", "PABR 2")
roses <- lapply(stids, saveWindRoses, asos_dir )

# plot for custom legend
speeds <- c("0 - 6", "6 - 10", "10 - 14", "14 - 18", "18 - 22", "22 +")
dfleg <- data.frame(x = 1:6, mph = factor(speeds, levels = speeds))
pleg <- ggplot(dfleg, aes(x, x)) + 
  geom_tile(aes(fill = mph)) +
  scale_fill_manual(guide = guide_legend(
    direction = "vertical",
    title.position = "top",
    label.position = "right",
    ncol = 2
  ),
  values = openColours(n = 6)) +
  theme(legend.position = "right",
        legend.title = element_text(family = "serif"),
        legend.text = element_text(family = "serif"),
        legend.margin = margin(c(0, 0, 0, 0)))
tmp <- ggplot_gtable(ggplot_build(pleg))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
mylegend <- tmp$grobs[[leg]]

# textGrobs for titles
tg1 <- textGrob("Anchorage", gp = gpar(fontfamily = "serif"), 
                x = unit(0.3, "npc"), y = unit(-1, "npc"))
tg2 <- textGrob("Bettles", gp = gpar(fontfamily = "serif"), 
                x = unit(0.25, "npc"), y = unit(-1, "npc"))
tg3 <- textGrob("Fairbanks", gp = gpar(fontfamily = "serif"), 
                x = unit(0.3, "npc"), y = unit(-1, "npc"))
tg4 <- textGrob("Juneau", gp = gpar(fontfamily = "serif"), 
                x = unit(0.25, "npc"), y = unit(-1, "npc"))
tg5 <- textGrob("Kodiak", gp = gpar(fontfamily = "serif"), 
                x = unit(0.25, "npc"), y = unit(-1, "npc"))
tg6 <- textGrob("Nome", gp = gpar(fontfamily = "serif"), 
                x = unit(0.25, "npc"), y = unit(-1, "npc"))
tg7 <- textGrob("Saint Paul", gp = gpar(fontfamily = "serif"), 
                x = unit(0.3, "npc"), y = unit(-1, "npc"))
tg8 <- textGrob("Utqiatvik (Barrow)", gp = gpar(fontfamily = "serif"), 
                x = unit(0.4, "npc"), y = unit(-1, "npc"))
tg9 <- textGrob(" ", just = "left")

r1 <- arrangeGrob(tg1, tg2, tg3, 
                  roses[[1]], roses[[2]], roses[[3]], nrow = 2, ncol = 3, 
                  heights = c(1, 40))
r2 <- arrangeGrob(tg4, tg5, tg6, 
                  roses[[4]], roses[[5]], roses[[6]], nrow = 2, ncol = 3, 
                  heights = c(1, 40))
r3 <- arrangeGrob(tg7, tg8, tg9, 
                  roses[[7]], roses[[8]], mylegend, nrow = 2, ncol = 3, 
                  heights = c(1, 40))

p <- arrangeGrob(r1, r2, r3, nrow = 3)

ggsave(file.path(figdir, "annual_wind_roses.pdf"), p, width = 7.48, height = 8)

# Monthly Wind Roses
saveMonthlyRoses <- function(stid, asos_dir){
  library(lubridate)
  
  gl <- as.numeric(strsplit(stid, " ")[[1]][2])
  #st_name <- strsplit(stid, " ")[[1]][3]
  #st_name <- gsub("_", " ", st_name)
  stid <- strsplit(stid, " ")[[1]][1]
  
  asos <- readRDS(file.path(asos_dir, paste0(stid, ".Rds"))) %>%
    mutate(mo = month(t_round))
  
  lattice.options(
    layout.heights=list(bottom.padding=list(x=-0.75), top.padding=list(x=-0.75)),
    layout.widths=list(left.padding=list(x=-0.75), right.padding=list(x=-0.75))
  )
  monthlyRose <- function(mo.no, df, gl){
    df <- df %>% filter(mo == mo.no)
    
    calm <- nrow(df[df$sped_adj == 0, ])/nrow(df)
    df$sped_adj[df$drct == 0] <- 0
    
    p <- windRose(df, "sped_adj", "drct", 
                  paddle = FALSE, breaks = c(0, 6, 10, 14, 18, 22),
                  angle = 10, grid.line = gl, calm = calm)
    
    tg <- textGrob(month.name[mo.no], gp = gpar(fontfamily = "serif"), 
                   x = unit(0.5, "npc"), y = unit(0.3, "npc"))
    p <- arrangeGrob(p$plot, top = tg)
    
    return(p)
  }
  
  rs <- lapply(1:12, monthlyRose, asos, gl)
  
  arrangeGrob(grobs = rs, nrow = 4)
}

# Anchorage
stids <- c("PANC 2 Anchorage")
roses <- lapply(stids, saveMonthlyRoses, asos_dir)
ggsave(file.path(figdir, "figure_7.pdf"), roses[[1]], width = 7, height = 9)

#------------------------------------------------------------------------------

#-- Fig 6 Monthly Bar Plots ---------------------------------------------------
# Figure 6

monthly_path <- file.path(
  datadir, "AK_ASOS_monthly_select_adj_19800101_to_20150101.Rds")
asos_monthly <- readRDS(monthly_path)

stids <- c("PANC", "PABT", "PAFA", "PAJN",
           "PADK", "PAOM", "PASN", "PABR")
sta_names <- c("Anchorage", "Bettles", "Fairbanks", "Juneau", 
               "Kodiak", "Nome", "Saint Paul", "Utqiatvik (Barrow)")
names_df <- data.frame(stid = stids, 
                       sta_name = factor(sta_names), 
                       stringsAsFactors = FALSE)

asos_temp <- asos_monthly %>%
  filter(stid %in% stids & ym_date < ymd("2015-01-01")) %>%
  left_join(names_df, by = "stid") %>%
  mutate(month = as.factor(month(ym_date))) %>%
  group_by(sta_name, month) %>%
  summarise(sd_sped = sd(avg_sped),
            avg_sped = mean(avg_sped))
levels(asos_temp$month) <- month.abb[as.numeric(levels(asos_temp$month))]

barfill <- "gold1"
barlines <- "goldenrod2"

p <- ggplot(asos_temp, aes(x = month, y = avg_sped)) +
  geom_errorbar(aes(ymin = avg_sped, 
                    ymax = avg_sped + sd_sped, 
                    width = 0.2)) +
  geom_bar(stat = "identity", colour = barlines, fill = barfill) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~sta_name, scales = "free_y", nrow = 4) +
  xlab("Month") + ylab("Avg Wind Speed (mph)") +
  theme_classic() +
  theme(axis.text = element_text("serif",
                                 size = 10,
                                 color = "black"),
        axis.title = element_text("serif",
                                  size = 12),
        strip.background = element_blank(),
        strip.text = element_text("serif",
                                  size = 12)) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

ggsave(file.path(figdir, "figure_6.pdf"), p, 
       width = 7.48, height = 9)

#------------------------------------------------------------------------------

#-- Fig 9 Seasonal Coastal HWEs -----------------------------------------------
# Figure 9

library(ggplot2)

sites <- c("Kaktovik (13)", "Barrow (51)", "Nome (42)", "St. Paul (17)", 
           "Kodiak (47)", "Anchorage (17)", "Juneau (107)", "Sitka (56)")
seasons <- c("Wi", "Sp", "Su", "Au")

seas <- data.frame(site = factor(rep(sites, each = 4), levels = sites),
                   Season = factor(rep(seasons, 8), levels = seasons),
                   prop = c(85, 0, 0, 15, 39, 18, 0, 43, 50, 10, 5, 35, 82, 18, 
                            0, 0, 57, 15, 0, 28, 59, 18, 0, 23, 61, 19, 0, 20, 
                            53, 9, 2, 36))

barfill <- c("cadetblue1", "green3", "gold1", "lightsalmon")
barcols <- c("cadetblue", "forestgreen", "darkgoldenrod", "lightsalmon3")

p <- ggplot(seas, aes(x = site, y = prop, color = Season, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
  scale_fill_manual(values = barfill) +
  scale_color_manual(values = barcols) +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  xlab("Location") + ylab("Proportion (%)") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black",
                                 family = "serif"),
        axis.title = element_text(family = "serif"),
        legend.text = element_text(family = "serif"),
        legend.title = element_text(family = "serif"),
        panel.grid = element_blank())

ggsave(file.path(figdir, "figure_9.pdf"), p, 
       width = 7.48, height = 3)

#------------------------------------------------------------------------------

#-- Fig 10 HWEs by Period -----------------------------------------------------
# Figure 10

library(ggplot2)

sites <- c("Kaktovik", "Barrow", "Nome", "St. Paul", "Kodiak", "Anchorage", 
           "Juneau", "Sitka")
years <- c("1980-1997", "1997-2014")

hwes <- data.frame(site = factor(rep(sites, each = 2), levels = sites),
                   Period = factor(rep(years, 8), levels = years),
                   prop = c(44, 56, 39, 61, 45, 55, 53, 47, 55, 45, 65, 35, 
                            62, 38, 43, 57))

barfill <- c("khaki1", "lightslateblue")
barcols <- c("khaki3", "lightslategrey")

p <- ggplot(hwes, aes(x = site, y = prop, color = Period, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) + 
  scale_fill_manual(values = barfill) +
  scale_color_manual(values = barcols) +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  xlab("Location") + ylab("Proportion (%)") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black",
                                 family = "serif"),
        axis.title = element_text(family = "serif"),
        legend.text = element_text(family = "serif"),
        legend.title = element_text(family = "serif"),
        panel.grid = element_blank())

ggsave(file.path(figdir, "figure_10.pdf"), p, 
       width = 7.48, height = 3)

#------------------------------------------------------------------------------

#-- Fig 12 Model Trends Barplots ----------------------------------------------
# Figure 12

library(ggplot2)

mods <- c("CM3", "CCSM4")
years <- c("1980-2014", "2065-2099")
seasons <- c("cold", "warm")

trends <- data.frame(mod = factor(rep(mods, each = 2), levels = mods),
                     Period = factor(rep(years, 4), levels = years),
                     season = factor(rep(seasons, each = 4), levels = seasons),
                     count = c(102, 21, 95, 2, 13, 162, 18, 38))

barfill <- c("slategray1", "wheat")
barcols <- c("slategray", "wheat3")

labels <- c(cold = "Cold Season\n(Dec - Mar)", warm = "Warm Season\n(Jun - Sep)")

p <- ggplot(trends, aes(x = mod, y = count, color = Period, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), 
           width = 0.5) + 
  facet_wrap(~season, labeller = labeller(season = labels)) + 
  scale_fill_manual(values = barfill) +
  scale_color_manual(values = barcols) +
  scale_y_continuous(limits = c(0, 165), expand = c(0, 0)) +
  xlab("Model") + ylab("Count (station-months)") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black",
                                 family = "serif"),
        axis.title = element_text(family = "serif"),
        legend.text = element_text(family = "serif"),
        legend.title = element_text(family = "serif"),
        strip.text = element_text(size = 12, 
                                  family = "serif",
                                  margin = margin(1, 0, 10, 0)),
        strip.background = element_blank())

ggsave(file.path(figdir, "figure_12.pdf"), p, 
       width = 7.48, height = 3.5)

#------------------------------------------------------------------------------
