# Script Summary
#   example figures for climatology website
#
# Example wind roses
#
# Example monthly average barplots
#
# Output files:
#   /figures/
#   /figures/



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
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
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
# id 20 is Fairbanks
{id <- 20
asos_station_path <- file.path(asos_select_adj_dir, paste0(stids[id], ".Rds"))
asos_station <- readRDS(asos_station_path)
save_path <- file.path(examples_dir, paste0(stids[id], "_windrose.tiff"))
tiff(save_path, width = 2500, height = 2500, res = 300)
windRose(asos_station, "sped_adj", "drct", 
         paddle = FALSE, breaks = c(0, 4, 8, 12, 16, 20),
         angle = 10,
         key.footer = "mph")
dev.off()}

# id 5 is barrow
{id <- 5
asos_station_path <- file.path(asos_select_adj_dir, paste0(stids[id], ".Rds"))
asos_station <- readRDS(asos_station_path)
save_path <- file.path(examples_dir, paste0(stids[id], "_windrose.tiff"))
tiff(save_path, width = 2500, height = 2500, res = 300)
windRose(asos_station, "sped_adj", "drct", 
         paddle = FALSE, breaks = c(0, 6, 10, 14, 18, 22),
         angle = 10,
         key.header = paste0(stids[id]),
         key.footer = "mph")
dev.off()}

#------------------------------------------------------------------------------

#-- Bar plots ----------------------------------------------------------------

barfill <- "gold1"
barlines <- "goldenrod2"

p7 <- ggplot(asos_temp(), aes(x = month, y = avg_sped)) +
  geom_bar(stat = "identity", colour = barlines, fill = barfill) +
  ggtitle(input$stid) + xlab("Month") + ylab("Average Speed (mph)") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

p7

#------------------------------------------------------------------------------
