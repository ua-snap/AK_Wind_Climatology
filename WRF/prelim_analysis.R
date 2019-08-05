# Script Summary
#
# Compare model output means between periods by site and month
#
# Output files:
#   /figures/climatology_comparison/cm3_ttest_heatmap.png
#   /figures/climatology_comparison/ccsm4_ttest_heatmap.png
#   /figures/climatology_comparison/cm3_boxplots_wind_event_subset.png
#   /figures/climatology_comparison/ccsm4_boxplots_wind_event_subset.png
#   /data/climatology_comparison/cm3_"jan/jul"_df.Rds
#   /data/climatology_comparison/ccsm4_"jan/jul"_df.Rds



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

workdir <- getwd()
code_dir <- file.path(workdir, "code")
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures")

source(file.path(code_dir, "helpers.R"))

#------------------------------------------------------------------------------

#-- Monthly Data --------------------------------------------------------------
# This data used in more than one of the following sections
# CM3 
cm3h_monthly <- readRDS(file.path(datadir, "cm3h_monthly.Rds"))
cm3f_monthly <- readRDS(file.path(datadir, "cm3f_monthly.Rds"))

cm3_monthly <- bind_rows(cm3h_monthly, cm3f_monthly)
cm3h_monthly <- cm3_monthly %>% filter(ym_date < "2015-01-01")
cm3f_monthly <- cm3_monthly %>% filter(ym_date >= "2065-01-01" & 
                                         ym_date < "2100-01-01")
cm3h_monthly$clim <- factor("1980-2015", 
                            levels = c("1980-2015", "2065-2100"))
cm3f_monthly$clim <- factor("2065-2100", 
                            levels = c("1980-2015", "2065-2100"))
cm3_monthly <- bind_rows(cm3h_monthly, cm3f_monthly) %>%
  mutate(mo = factor(month.abb[as.numeric(format(ym_date, "%m"))],
                     levels = month.abb))

# CCSM4
ccsm4h_monthly <- readRDS(file.path(datadir, "ccsm4h_monthly.Rds"))
ccsm4f_monthly <- readRDS(file.path(datadir, "ccsm4f_monthly.Rds"))

ccsm4_monthly <- bind_rows(ccsm4h_monthly, ccsm4f_monthly)
ccsm4h_monthly <- ccsm4_monthly %>% filter(ym_date < "2015-01-01")
ccsm4f_monthly <- ccsm4_monthly %>% filter(ym_date >= "2065-01-01" & 
                                             ym_date < "2100-01-01")
ccsm4h_monthly$clim <- factor("1980-2015", 
                              levels = c("1980-2015", "2065-2100"))
ccsm4f_monthly$clim <- factor("2065-2100", 
                              levels = c("1980-2015", "2065-2100"))
ccsm4_monthly <- bind_rows(ccsm4h_monthly, ccsm4f_monthly) %>%
  mutate(mo = factor(month.abb[as.numeric(format(ym_date, "%m"))],
                     levels = month.abb))

#------------------------------------------------------------------------------

#-- All Sites T-Test ----------------------------------------------------------
stations <- readRDS(file.path(datadir, "AK_ASOS_select_stations.Rds"))
stids <- stations$stid
stid_names <- stations %>% select(stid, station_name)

# CM3
cm3_ttest <- lapply(stids, t_test_stid, cm3_monthly) %>%
  bind_rows() 

cm3_sig <- cm3_ttest %>% 
  left_join(stations, by = "stid") %>%
  mutate(sig = if_else(p_val <= 0.05 & mean_x < mean_y, 
                       "Future Signif. Higher", 
                       "Future Signif. Lower"),
         sig = if_else(p_val > 0.05, "No Signif. Difference", sig),
         sig = factor(sig, levels = c("No Signif. Difference", 
                                      "Future Signif. Lower", 
                                      "Future Signif. Higher")),
         mo = factor(mo, levels = month.abb)) %>%
  select(station_name, sig, mo)

p <- ggplot(cm3_sig, aes(x = mo, y = station_name)) + 
  geom_tile(aes(fill = sig), color = "grey") + 
  scale_fill_manual(values = c("White", "lightblue3", 
                               "lightgoldenrod3")) + 
  ylab("Location") + xlab("Month") + 
  labs(fill = "Significance\nat \u03B1 = 0.05") +
  ggtitle("GFDL CM3 Average Monthly Wind Speeds", 
          subtitle = "T-Tests between future and historical periods") +
  theme_bw()

plot_path <- file.path(figdir, "climatology_comparison",
                       "cm3_ttest_heatmap.png")
ggsave(plot_path, p, device = "png", height = 12, width = 7)

# CCSM4
ccsm4_ttest <- lapply(stids, t_test_stid, ccsm4_monthly) %>%
  bind_rows()

ccsm4_sig <- ccsm4_ttest %>% 
  left_join(stations, by = "stid") %>%
  mutate(sig = if_else(p_val <= 0.05 & 
                         mean_x < mean_y, 
                       "Future Signif. Higher", 
                       "Future Signif. Lower"),
         sig = if_else(p_val > 0.05, "No Signif. Difference", sig),
         sig = factor(sig, levels = c("No Signif. Difference", 
                                      "Future Signif. Lower", 
                                      "Future Signif. Higher")),
         mo = factor(mo, levels = month.abb)) %>%
  select(station_name, sig, mo)

p <- ggplot(ccsm4_sig, aes(x = mo, y = station_name)) + 
  geom_tile(aes(fill = sig), color = "grey") + 
  scale_fill_manual(values = c("White", "lightblue3", 
                               "lightgoldenrod3")) + 
  ylab("Location") + xlab("Month") + 
  labs(fill = "Significance\nat \u03B1 = 0.05") +
  ggtitle("CCSM4 Average Monthly Wind Speeds", 
          subtitle = "T-Tests between future and historical periods") +
  theme_bw()

plot_path <- file.path(figdir, "climatology_comparison",
                       "ccsm4_ttest_heatmap.png")
ggsave(plot_path, p, device = "png", height = 12, width = 7)

#------------------------------------------------------------------------------

#-- Mean/Quantile Check -------------------------------------------------------
# Generate list of stations with difference in monthly means, and difference in 
#   99.00% and 99.90% quantiles between historical and future
# Using the same number of quantiles used in quantile mapping

# Need to determine # of quantiles used
# Number of quantiles used is the minimum of era and cm3h, which is length of 
#   cm3h
cm3 <- readRDS(file.path(datadir, "CM3_stations_adj/PAAQ_cm3h_adj.Rds"))
qn <- nrow(cm3)/12

# Need to read the historical/future data for each station, extract and 
#   compare quantiles
stations <- readRDS(file.path(datadir, "AK_ASOS_select_stations.Rds"))
stids <- stations$stid

qx_diffs <- function(stid, qn, mo, mod = 1){
  mod_l <- c("cm3", "ccsm4")[mod]
  mod <- c("CM3", "CCSM4")[mod]
  cm3h <- readRDS(file.path(datadir, paste0(mod, "_stations_adj"), 
                            paste0(stid, "_", mod_l, "h_adj.Rds"))) %>%
    filter(month(ts) == mo)
  cm3f <- readRDS(file.path(datadir, paste0(mod, "_stations_adj"), 
                            paste0(stid, "_", mod_l, "f_adj.Rds"))) %>%
    filter(month(ts) == mo)
  qxh <- quantile(cm3h$sped, seq(0, 1, length.out = qn), type = 8)
  qxf <- quantile(cm3f$sped, seq(0, 1, length.out = qn), type = 8)
  temp <- round(as.numeric(gsub("%", "", names(qxh))), 2)
  i <- c(which(temp == 99.00)[1], which(temp == 99.90)[1])
 
  qd <- qxf[i] - qxh[i] 
  return(as.numeric(qd))
}

# CM3 January
cm3_jan_qx_diffs <- sapply(stids, qx_diffs, qn, mo = 1)

cm3_jan_qx_df <- data.frame(stid = colnames(cm3_jan_qx_diffs),
                            q99.00_diff = as.numeric(cm3_jan_qx_diffs[1, ]),
                            q99.90_diff = as.numeric(cm3_jan_qx_diffs[2, ]),
                            stringsAsFactors = FALSE)
jan_df <- cm3_ttest %>%
  filter(mo == "Jan") %>%
  mutate(mean_diff = mean_y - mean_x) %>%
  rename(signif = sig) %>%
  left_join(cm3_jan_qx_df, by = "stid") %>%
  left_join(stations, by = "stid") %>%
  select(station_name, q99.00_diff,  q99.90_diff, mean_diff, signif) %>%
  mutate(q99.00_diff = round(q99.00_diff, 2),
         q99.90_diff = round(q99.90_diff, 2), 
         mean_diff = round(mean_diff, 2))
saveRDS(jan_df, file.path(datadir, "model_climatology_comparison",
                          "cm3_jan_df.Rds"))

# July
cm3_jul_qx_diffs <- sapply(stids, qx_diffs, qn, mo = 7)

cm3_jul_qx_df <- data.frame(stid = colnames(cm3_jul_qx_diffs),
                            q99.00_diff = as.numeric(cm3_jul_qx_diffs[1, ]),
                            q99.90_diff = as.numeric(cm3_jul_qx_diffs[2, ]),
                            stringsAsFactors = FALSE)
jul_df <- cm3_ttest %>%
  filter(mo == "Jul") %>%
  mutate(mean_diff = mean_y - mean_x) %>%
  rename(signif = sig) %>%
  left_join(cm3_jul_qx_df, by = "stid") %>%
  left_join(stations, by = "stid") %>%
  select(station_name, q99.00_diff,  q99.90_diff, mean_diff, signif) %>%
  mutate(q99.00_diff = round(q99.00_diff, 2),
         q99.90_diff = round(q99.90_diff, 2), 
         mean_diff = round(mean_diff, 2))
saveRDS(jul_df, file.path(datadir, "model_climatology_comparison",
                          "cm3_jul_df.Rds"))

# CCSM4 January
ccsm4_jan_qx_diffs <- sapply(stids, qx_diffs, qn, mo = 1, mod = 2)

ccsm4_jan_qx_df <- data.frame(stid = colnames(ccsm4_jan_qx_diffs),
                            q99.00_diff = as.numeric(ccsm4_jan_qx_diffs[1, ]),
                            q99.90_diff = as.numeric(ccsm4_jan_qx_diffs[2, ]),
                            stringsAsFactors = FALSE)
jan_df <- ccsm4_ttest %>%
  filter(mo == "Jan") %>%
  mutate(mean_diff = mean_y - mean_x) %>%
  rename(signif = sig) %>%
  left_join(ccsm4_jan_qx_df, by = "stid") %>%
  left_join(stations, by = "stid") %>%
  select(station_name, q99.00_diff,  q99.90_diff, mean_diff, signif) %>%
  mutate(q99.00_diff = round(q99.00_diff, 2),
         q99.90_diff = round(q99.90_diff, 2), 
         mean_diff = round(mean_diff, 2))
saveRDS(jan_df, file.path(datadir, "model_climatology_comparison",
                          "ccsm4_jan_df.Rds"))

# CCSM4 July
ccsm4_jul_qx_diffs <- sapply(stids, qx_diffs, qn, mo = 7, mod = 2)

ccsm4_jul_qx_df <- data.frame(stid = colnames(ccsm4_jul_qx_diffs),
                            q99.00_diff = as.numeric(ccsm4_jul_qx_diffs[1, ]),
                            q99.90_diff = as.numeric(ccsm4_jul_qx_diffs[2, ]),
                            stringsAsFactors = FALSE)
jul_df <- ccsm4_ttest %>%
  filter(mo == "Jul") %>%
  mutate(mean_diff = mean_y - mean_x) %>%
  rename(signif = sig) %>%
  left_join(ccsm4_jul_qx_df, by = "stid") %>%
  left_join(stations, by = "stid") %>%
  select(station_name, q99.00_diff,  q99.90_diff, mean_diff, signif) %>%
  mutate(q99.00_diff = round(q99.00_diff, 2),
         q99.90_diff = round(q99.90_diff, 2), 
         mean_diff = round(mean_diff, 2))
saveRDS(jul_df, file.path(datadir, "model_climatology_comparison",
                          "ccsm4_jul_df.Rds"))

#------------------------------------------------------------------------------

#-- T-Test Signifiance Barplots -----------------------------------------------
cm3_sig_tab <- cm3_sig %>%
  group_by(mo) %>%
  summarize(sig_l = table(sig)[2],
            sig_h = table(sig)[3]) %>%
  gather("sig", "count", -mo) %>%
  mutate(sig = factor(sig, levels = c("sig_l", "sig_h")),
         mod = factor("CM3", levels = c("CM3", "CCSM4")))

sig_tab <- ccsm4_sig %>%
  group_by(mo) %>%
  summarize(sig_l = table(sig)[2],
            sig_h = table(sig)[3]) %>%
  gather("sig", "count", -mo) %>%
  mutate(sig = factor(sig, levels = c("sig_l", "sig_h")),
         mod = factor("CCSM4", levels = c("CM3", "CCSM4"))) %>%
  bind_rows(cm3_sig_tab)

p <- ggplot(sig_tab, aes(x = mo, y = count, fill = sig)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(fill = "Significance\nat p = 0.05") +
 
  scale_fill_manual(labels = c("Future Signif. Lower",
                               "Future Signif. Higher"),
                    values = c("lightblue3", "lightgoldenrod3")) +
  xlab("Month") + ylab("Number of Locations") + 
  facet_wrap(~mod, nrow = 2) +
  theme_classic() +
  theme(legend.position = "bottom")
  
plot_path <- file.path(figdir, "climatology_comparison",
                       "signif_barplot.png")
ggsave(plot_path, p, device = "png", height = 8, width = 6)

#------------------------------------------------------------------------------

#-- Significance Maps ---------------------------------------------------------
library(sf)
library(USAboundaries)

stations <- readRDS(file.path(datadir, "AK_ASOS_select_stations.Rds"))
# alaska sf object
alaska <- us_states(states = "AK", resolution = "high")
ak_sf <- st_transform(alaska, 26935)

# July
coords <- cm3_sig %>% 
  filter(mo == "Jul") %>%
  left_join(stations, by = "station_name") %>%
  select(stid, sig, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(26935)

# plot function
signif_map <- function(coords, ak){
  p <- ggplot(data = ak) + geom_sf(fill = "cornsilk") +
    ggtitle("Significant Differences in July Climatologies, GFDL CM3", 
            subtitle = "Historical (1980-2015) vs Future (2065-2100)") +
    xlab("Lng") + ylab("Lat") +
    geom_sf(data = coords, aes(color = sig, shape = sig),
            show.legend = "point") +
    scale_shape_manual(values = c(21, 16, 16), guide = FALSE) +
    scale_color_manual(name = "Significance",
                       values = c("black", "dodgerblue", "red"),
                       labels = c("Not Significant", "Future Signif. Lower", 
                                  "Future Signif. Higher"),
                       guide = guide_legend(override.aes = 
                                              list(shape = c(21, 16, 16)))) + 
    theme_bw()
  return(p)
}

p <- signif_map(coords, ak_sf)
plot_path <- file.path(figdir, "climatology_comparison",
                       "cm3_signif_jul_locations.png")
ggsave(plot_path, p, device = "png", scale = 1)

# January
coords <- cm3_sig %>% 
  filter(mo == "Jan") %>%
  left_join(stations, by = "station_name") %>%
  select(stid, sig, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(26935)

# plot
p <- signif_map(coords, ak_sf)
plot_path <- file.path(figdir, "climatology_comparison",
                       "cm3_signif_jan_locations.png")
ggsave(plot_path, p, device = "png", scale = 1)

#------------------------------------------------------------------------------

#-- Boxplot Comparisons -------------------------------------------------------
# labels will apply to both models
wind_event_stids <- c("PAOM", "PABR", "PASI", "PAJN", 
                      "PABA", "PANC", "PADQ", "PASN",
                      "PAFA")
stid_names <- list(
  "PAOM" = "Nome",
  "PABR" = "Barrow",
  "PASI" = "Sitka",
  "PAJN" = "Juneau",
  "PABA" = "Kaktovik",
  "PANC" = "Anchorage",
  "PADQ" = "Kodiak",
  "PASN" = "Saint Paul",
  "PAFA" = "Fairbanks"
)
stid_labeller <- function(variable,value){
  return(stid_names[value])
}

# CM3
cm3_ttest <- lapply(wind_event_stids, t_test_stid, cm3_monthly) %>%
  bind_rows()

cm3_results <- cm3_monthly %>% 
  filter(stid %in% wind_event_stids) %>%
  full_join(cm3_ttest, by = c("stid", "mo"))

p <- ggplot(cm3_results, 
            aes(x = mo, y = avg_sped_adj, fill = clim, col = sig)) + 
  geom_boxplot() +
  scale_color_manual(values = c("#E69F00", "black")) + 
  facet_wrap(~stid, labeller = stid_labeller) + 
  labs(fill = "Climatological\nPeriod",
       col = "Significance\nat p = 0.05") + 
  xlab("Month") + ylab("Avg Wind Speed (mph)") + 
  ggtitle("GFDL CM3 Average Wind Speeds",
          subtitle = "Boxplots by Month and Climatological Period") +
  theme_classic() + 
  theme(strip.text.x = element_text(size = 12))
plot_path <- file.path(figdir, "climatology_comparison",
                       "cm3_boxplots_wind_event_subset.png")
ggsave(plot_path, p, device = "png", scale = 1.5)

# CCSM4
ccsm4_ttest <- lapply(wind_event_stids, t_test_stid, ccsm4_monthly) %>%
  bind_rows()

ccsm4_results <- ccsm4_monthly %>% 
  filter(stid %in% wind_event_stids) %>%
  full_join(ccsm4_ttest, by = c("stid", "mo"))

p <- ggplot(ccsm4_results, 
            aes(x = mo, y = avg_sped_adj, fill = clim, col = sig)) + 
  geom_boxplot() +
  scale_color_manual(values = c("#E69F00", "black")) + 
  facet_wrap(~stid, labeller = stid_labeller) + 
  labs(fill = "Climatological\nPeriod",
       col = "Significance\nat p = 0.05") + 
  xlab("Month") + ylab("Avg Wind Speed (mph)") + 
  ggtitle("NCAR CCSM4 Average Wind Speeds",
          subtitle = "Boxplots by Month and Climatological Period") +
  theme_classic() + 
  theme(strip.text.x = element_text(size = 12))
plot_path <- file.path(figdir, "climatology_comparison",
                       "ccsm4_boxplots_wind_event_subset.png")
ggsave(plot_path, p, device = "png", scale = 1.5)

#------------------------------------------------------------------------------
