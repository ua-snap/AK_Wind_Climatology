# Script Summary
#   Generate figures to be included in the AGU19 poster

# Figure 1: Map of Alaska showing station locations

# Figure 2: Example of wind speed time series before and after adjustment

# Figure 3: Quantile mapping ECDFs

# Figure 4: Sample time series of winds during specific events

# Figure 5: Monthly average speeds for select locations

# Figure 6: Anchorage wind roses

# Figure 7: Checkerboard T-Test results

# Checkerboard ttest results
#
# Output files:
#   /figures/agu19_poster/figure_1.jpeg
#   /figures/agu19_poster/figure_2.jpeg
#   /figures/agu19_poster/figure_3.jpeg
#   /figures/agu19_poster/figure_4.jpeg
#   /figures/agu19_poster/figure_5.jpeg
#   /figures/agu19_poster/figure_6.jpeg



#-- Fig 1 AK Map of Stations --------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(sf)
library(USAboundaries)

# load select stations
select_stations <- readRDS("data/AK_ASOS_select_stations.RDS")
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
  filter(stid %in% lab_stids) 

lab_coords <- lab_coords %>%
  bind_cols(as.data.frame(matrix(unlist(lab_coords$geometry), 
                                 ncol = 2, byrow = TRUE))) %>%
  rename(lonl = V1, latl = V2)

lab_coords$site <- c("Kaktovik",
                     "Utqiagvik (Barrow)",
                     "Kodiak",
                     "Juneau",
                     "Anchorage",
                     "Nome",
                     "Sitka",
                     "Saint Paul")

nudx <- c(400000, -700000, 200000, 200000, 550000, -500000, -50000, -100000)
nudy <- c(100000, 0, -300000, 200000, -300000, 100000, -300000, 200000)

# plot
p <- ggplot(data = ak_sf) + geom_sf(fill = "cornsilk", size = 0.5) +
  xlab("Lon") + ylab("Lat") +
  theme_bw() +
  theme(text = element_text(color = "black", size = 20),
        axis.text = element_text(color = "black", size = 30),
        axis.title = element_text(size = 40)) +
  geom_text_repel(data = lab_coords, aes(x = lonl, y = latl, label = site),
                  nudge_x = nudx, nudge_y = nudy, size = 9, segment.size = 0.4,
                  min.segment.length = 0, box.padding = 0) +
  geom_sf(data = coords, shape = 19, col = "dodgerblue4", size = 5) 

fn <- "figures/agu19_poster/figure_1.jpeg"
ggsave(fn, p, width = 13.81, height = 8, dpi = 500)

#------------------------------------------------------------------------------

#-- Fig 2 Discontinuity Adj Time Series ---------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)

# select/adj data summarized by month
monthly_adj_path <- "data/AK_ASOS_monthly_select_adj_19800101_to_20150101.Rds"
asos_monthly <- readRDS(monthly_adj_path)
# changepoints
cpts_df <- readRDS("data/AK_ASOS_stations_adj/cpts_df.Rds")

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
  geom_line(col = "grey", size = 1.25) +
  xlim(ymd("1980-01-01"), ymd("2015-01-01")) + 
  scale_x_date(date_labels = date_labels, breaks = date_breaks) + 
  ggtitle("Kodiak Municipal Airport") + 
  geom_vline(xintercept = x1_start1, col = "gray50", 
             lty = 3, size = 1) + 
  geom_vline(xintercept = x2_end1, col = "gray50", 
             lty = 3, size = 1) + 
  geom_vline(xintercept = x1_end1, 
             col = "goldenrod1", size = 3) + 
  geom_segment(aes(x = x1_start1, xend = x1_end1, y = m1_1, yend = m1_1),
               col = "dodgerblue4", size = 2) +
  geom_segment(aes(x = x1_end1, xend = x2_end1, y = m2_1, yend = m2_1),
               col = "dodgerblue4", size = 2) +
  geom_line(aes(ym_date, avg_sped_adj, group = 1), size = 1.25) + 
  scale_y_continuous(limits = c(5, 25), breaks = c(5, 10, 15, 20, 25)) +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(),
        plot.title = element_text(size = 30),
        axis.text = element_text(color = "black", size = 25),
        axis.text.y = element_text(margin = margin(l = 10, r = 5))) 

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
  geom_line(col = "grey", size = 1.25) +
  xlim(ymd("1980-01-01"), ymd("2015-01-01")) + 
  scale_x_date(date_labels = date_labels, breaks = date_breaks) + 
  ggtitle("Eielson AFB") + 
  geom_vline(xintercept = x1_start2, col = "gray50", 
             lty = 3, size = 1) + 
  geom_vline(xintercept = x3_end2, col = "gray50", 
             lty = 3, size = 1) + 
  geom_vline(xintercept = x1_end2, 
             col = "goldenrod1", size = 3) + 
  geom_vline(xintercept = c(x1_end2, x2_end2), 
             col = "goldenrod1", size = 3) + 
  geom_segment(aes(x = x1_start2, xend = x1_end2, y = m1_2, yend = m1_2),
               col = "dodgerblue4", size = 2) +
  geom_segment(aes(x = x1_end2, xend = x2_end2, y = m2_2, yend = m2_2),
               col = "dodgerblue4", size = 2) +
  geom_segment(aes(x = x2_end2, xend = x3_end2, y = m3_2, yend = m3_2),
               col = "dodgerblue4", size = 2) +
  geom_line(aes(ym_date, avg_sped_adj, group = 1), size = 1.25) +
  scale_y_continuous(limits = c(0, 8), breaks = c(0, 2, 4, 6, 8)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(color = "black",
                                 size = 25),
        axis.text.y = element_text(margin = margin(l = 25, r = 5)),
        plot.title = element_text(size = 30)) 

p <- arrangeGrob(p1, p2, nrow = 2, 
                 left = textGrob("Avg Wind Speed (mph)", 
                                 gp = gpar(fontsize = 30),
                                 rot = 90),
                 bottom = textGrob("Time", gp = gpar(fontsize = 30)))

fn <- "figures/agu19_poster/figure_2.jpeg"
ggsave(fn, p, width = 18, height = 6, dpi = 500)

#------------------------------------------------------------------------------

#-- Fig 3 ERA & CM3 ECDFs -----------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)

# ggECDF_compare modified for manuscript
ggECDF_compare <- function(obs, sim, sim_adj, p_tag = " ",
                           sim_lab, obs_lab, cols = 1){
  
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
    stat_ecdf(size = 1.5) + 
    xlim(c(0, xmax)) + #scale_color_discrete(name = "  ", 
    #                    labels = c(sim_lab, obs_lab)) + 
    # ggtitle(p_title) +
    # labs(tag = p_tag) +
    theme_bw() + 
    theme(legend.position = "top",
          plot.title = element_text(vjust = -1),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 22,
                                   color = "black"),
          legend.text = element_text(size = 25),
          legend.margin = margin(10, 0, 0, 0),
          plot.margin = unit(c(2, 2, 9, 6), "mm"))
  
  if(cols == 1){
    p1 <- p1 + scale_color_manual(values = c("goldenrod", "black"),
                                  name = "   ",
                                  labels = c(sim_lab, obs_lab))
  } else {
    p1 <- p1 + scale_color_manual(values = c("dodgerblue4", "goldenrod"),
                                  name = "   ",
                                  labels = c(sim_lab, obs_lab))
  }
  
  # corrected data
  p2 <- ggplot(df2, aes(sped, color = quality)) + 
    stat_ecdf(size = 1.5) + 
    xlim(c(0, xmax))  + #ggtitle(" ") + 
    # labs(tag = "  ") +
    theme_bw() +
    theme(plot.title = element_text(vjust = -1),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 22,
                                   color = "black"),
          plot.margin = unit(c(2, 2, 9, 6), "mm"))
  
  if(cols == 1){
    p2 <- p2 + scale_color_manual(values = c("goldenrod", "black"),
                                  name = "   ",
                                  labels = c(sim_lab, obs_lab))
  } else {
    p2 <- p2 + scale_color_manual(values = c("dodgerblue4", "goldenrod"),
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
                                   x = unit(0.5, "npc"),
                                   vjust = -0.5,
                                   gp = gpar(fontsize = 28)),
                 left = textGrob("Cumulative Probability",
                                 gp = gpar(fontsize = 28),
                                 rot = 90, hjust = 0.35)),
     nrow = 2, heights = c(1, 10))#,
    # don't need A/B labels for now
    # top = textGrob(p_tag,
    #                x = unit(0.05, "npc"),
    #                y = unit(0.30, "npc"), 
    #                just = c("left", "top"),
    #                gp = gpar(fontsize = 28)))
  return(p)
}

asos <- readRDS("data/AK_ASOS_stations_adj/PANC.Rds") %>%
  filter(t_round < ymd("2015-01-01"))
era <- readRDS("data/ERA_stations_adj/PANC_era_adj.Rds") %>%
  filter(ts < ymd("2015-01-01"))
cm3 <- readRDS("data/CM3_stations_adj/PANC_cm3h_adj.Rds")

p1 <- ggECDF_compare(asos$sped_adj, era$sped, era$sped_adj + 0.3, 
                     "A", "ERA-Interim", "ASOS")

obs2 <- era$sped_adj[era$ts < ymd("2006-01-01")]
p2 <- ggECDF_compare(obs2, cm3$sped, cm3$sped_adj + 0.3,
                     "B", "CM3 Historical", "ERA-Interim", 2)

p <- arrangeGrob(p1, p2, nrow = 2)

fn <- "figures/agu19_poster/figure_3.jpeg"
ggsave(fn, p, width = 13.28, height = 15, dpi = 500)

#------------------------------------------------------------------------------

