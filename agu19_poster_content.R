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

fn <- "figures/agu19_poster/figure1.jpeg"
ggsave(fn, p, width = 13.81, height = 8, dpi = 500)

#------------------------------------------------------------------------------