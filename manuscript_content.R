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
#   /figured/manuscript/asos_discont_adj_ex.pdf



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

# plot
p <- ggplot(data = ak_sf) + geom_sf(fill = "cornsilk", size = 0.25) +
  xlab("Lng") + ylab("Lat") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(size = 10)) +
  geom_sf(data = coords, shape = 19, col = "darkred", size = 0.75) #+ 
#  geom_sf(data = coords_2, shape = 21, size = 2, fill = "cyan")

fig_path <- file.path(figdir, "AK_ASOS_select_locations.pdf")
ggsave(fig_path, plot = p, device = "pdf", width = 3.54, height = 2.05)

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
  scale_y_continuous(limits = c(0, 25), breaks = c(0, 5, 10, 15, 20, 25)) +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(),
        plot.title = element_text(size = 10)) 

# second station (two discontinuities)
# display time series for PAED and PAFA
stid2 <- "PABA"
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
  ggtitle("Barter Island") + 
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
  scale_y_continuous(limits = c(0, 25), breaks = c(0, 5, 10, 15, 20, 25)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 10)) 

p <- arrangeGrob(p1, p2, nrow = 2, 
                 left = textGrob("Avg Speed (mph)", gp = gpar(fontsize = 10),
                                 rot = 90),
                 bottom = textGrob("Time", gp = gpar(fontsize = 10)))
fig_path <- file.path(figdir, "asos_discont_adj_ex.pdf")
ggsave(fig_path, plot = p, device = "pdf", width = 7.25, height = 2.65)

#------------------------------------------------------------------------------

#-- ERA & CM3 ECDFs -----------------------------------------------------------
source(file.path(workdir, "code", "helpers.R"))

