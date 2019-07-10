# Script Summary
#   example figures for climatology website
#
# Example wind roses
#
# Example monthly average barplots
#
# Output files:
#   /figures/website_examples/PAFA_windrose.tiff
#   /figures/website_examples/PABR_windrose.tiff
#   /figures/website_examples/PAFA_mo_speeds.tiff
#   /figures/website_examples/PABR_mo_speeds.tiff



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(openair)
library(RColorBrewer)
library(sf)
library(USAboundaries)

# Work directory
workdir <- getwd()
datadir <- file.path(workdir, "data")
fig_dir <- file.path(workdir, "figures")
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
# website examples directory
examples_dir <- file.path(fig_dir, "website_examples")
# Monthly speeds
monthly_path <- file.path(datadir, 
                          "AK_ASOS_monthly_select_adj_19800101_to_20150101.Rds")
asos_monthly <- readRDS(monthly_path)
# select stations meta
select_stations_path <- file.path(datadir, "AK_ASOS_select_stations.Rds")
select_stations <- readRDS(select_stations_path)
stids <- select_stations$stid

#------------------------------------------------------------------------------

#-- Wind Roses ----------------------------------------------------------------
library(openair)
# id 20 is Fairbanks
{id_1 <- 20
asos_station_path <- file.path(asos_adj_dir, paste0(stids[id_1], ".Rds"))
asos_station <- readRDS(asos_station_path)
save_path <- file.path(examples_dir, paste0(stids[id_1], "_windrose.png"))
png(save_path, width = 2500, height = 2500, res = 300)
windRose(asos_station, "sped_adj", "drct", 
         paddle = FALSE, breaks = c(0, 4, 8, 12, 16, 20),
         angle = 10,
         key.header = paste0(stids[id_1]),
         key.footer = "mph")
dev.off()}

# id 5 is barrow
{id_2 <- 5
asos_station_path <- file.path(asos_adj_dir, paste0(stids[id_2], ".Rds"))
asos_station <- readRDS(asos_station_path)
save_path <- file.path(examples_dir, paste0(stids[id_2], "_windrose.png"))
png(save_path, width = 2500, height = 2500, res = 300)
windRose(asos_station, "sped_adj", "drct", 
         paddle = FALSE, breaks = c(0, 6, 10, 14, 18, 22),
         angle = 10,
         key.header = paste0(stids[id_2]),
         key.footer = "mph")
dev.off()}

#------------------------------------------------------------------------------

#-- Bar plots -----------------------------------------------------------------

barfill <- "gold1"
barlines <- "goldenrod2"
# Fairbanks
{id_1 <- 20
asos_station <- asos_monthly %>%
  filter(stid == stids[id_1]) %>%
  mutate(month = as.factor(month(ym_date))) %>%
  group_by(month) %>%
  summarise(sd_sped = sd(avg_sped),
            avg_sped = mean(avg_sped))
levels(asos_station$month) <- month.abb[as.numeric(levels(asos_station$month))]

p1 <- ggplot(asos_station, aes(x = month, y = avg_sped)) +
  geom_errorbar(aes(ymin = avg_sped, ymax = avg_sped + sd_sped, width = 0.2)) +
  geom_bar(stat = "identity", colour = barlines, fill = barfill) +
  ggtitle(stids[id_1], subtitle = "Fairbanks, AK") + 
  xlab("Month") + ylab("Average Wind Speed (mph)") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
save_path <- file.path(examples_dir, paste0(stids[id_1], "_mo_speeds.png"))
ggsave(save_path, p1)}

# Barrow
{id_2 <- 5
asos_station <- asos_monthly %>%
  filter(stid == stids[id_2]) %>%
  mutate(month = as.factor(month(ym_date))) %>%
  group_by(month) %>%
  summarise(sd_sped = sd(avg_sped),
            avg_sped = mean(avg_sped))
levels(asos_station$month) <- month.abb[as.numeric(levels(asos_station$month))]

p1 <- ggplot(asos_station, aes(x = month, y = avg_sped)) +
  geom_errorbar(aes(ymin = avg_sped, ymax = avg_sped + sd_sped, width = 0.2)) +
  geom_bar(stat = "identity", colour = barlines, fill = barfill) +
  ggtitle(stids[id_2], subtitle = "Utqiagvik, AK") + 
  xlab("Month") + ylab("Average Wind Speed (mph)") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
save_path <- file.path(examples_dir, paste0(stids[id_2], "_mo_speeds.png"))
ggsave(save_path, p1)}

#------------------------------------------------------------------------------
