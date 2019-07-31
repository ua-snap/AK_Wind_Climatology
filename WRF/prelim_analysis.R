# Script Summary
#
# Compare model output means between periods by site and month
#
# Output files:
#   /figures/climatology_comparison/cm3_ttest_heatmap.png
#   /figures/climatology_comparison/ccsm4_ttest_heatmap.png
#   /figures/climatology_comparison/cm3_boxplots_wind_event_subset.png
#   /figures/climatology_comparison/ccsm4_boxplots_wind_event_subset.png



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
stid_names <- select_stations %>% select(stid, station_name)

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

# plot
p <- ggplot(data = ak_sf) + geom_sf(fill = "cornsilk") +
  ggtitle("Significant Differences in July Climatologies, GFDL CM3", 
          subtitle = "Historical (1980-2015) vs Future (2065-2100)") +
  xlab("Lng") + ylab("Lat") +
  geom_sf(data = coords, aes(color = sig), show.legend = "point") +
  scale_color_manual(name = "Significance",
                     values = c("black", "dodgerblue", "firebrick"),
                     labels = c("Not Significant", "Future Signif. Lower", 
                                "Future Signif. Higher")) + 
  theme_bw()
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
p <- ggplot(data = ak_sf) + geom_sf(fill = "cornsilk") +
  ggtitle("Significant Differences in January Climatologies, GFDL CM3", 
          subtitle = "Historical (1980-2015) vs Future (2065-2100)") +
  xlab("Lng") + ylab("Lat") +
  geom_sf(data = coords, aes(color = sig), show.legend = "point") +
  scale_color_manual(name = "Significance",
                     values = c("black", "dodgerblue", "firebrick"),
                     labels = c("Not Significant", "Future Signif. Lower", 
                                "Future Signif. Higher")) + 
  theme_bw()
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
