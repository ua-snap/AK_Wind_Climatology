# Script summary
#
# Create figure to check alignment of WRF grid with station locations



#-- Setup ---------------------------------------------------------------------
library(ncdf4) 
library(raster) 
library(rgdal)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(USAboundaries)

workdir <- getwd()
datadir <- file.path(workdir, "data")

# WRF output dir (external drive)
wrf_output_dir <- "F:/Wind_Climatology/data/WRF_output"

# select stations meta data
wrf_ind_path <- file.path(datadir, "stid_wrf_ind.Rds")
wrf_ind <- readRDS(wrf_ind_path)
select_path <- file.path(datadir, "AK_ASOS_select_stations.Rds")
select_stations <- readRDS(select_path)
# select station stids
stids <- unique(wrf_ind$stid)

#------------------------------------------------------------------------------

#-- WRF Grid Alignment Check --------------------------------------------------
# u10 historical directories (from external drive)
u10_dir <- file.path(wrf_output_dir, "ERA_u10")
# open connection to one of the files 
u10_fname <- "u10_hourly_wrf_ERA-Interim_historical_"
u10_path <- file.path(u10_dir, paste0(u10_fname, "2000.nc"))
u10 <- nc_open(u10_path)
xc <- ncvar_get(u10, varid = "xc")
yc <- ncvar_get(u10, varid = "yc")
nx <- length(xc)
ny <- length(yc)
# close connection
nc_close(u10)
# resolution of grid would be the difference between two centroids
res <- xc[2] - xc[1]
# define extent bounds - pad with half of unit grid size 
bounds <- c(min(xc), max(xc), min(yc), max(yc)) + c(-res, res)/2
# Projection from WRF
wrf_crs <- CRS("+units=m +proj=stere +lat_ts=64.0 +lon_0=-152.0 +lat_0=90.0 +x_0=0 +y_0=0 +a=6370000 +b=6370000")
vals <- matrix(0, nrow = ny, ncol = nx)
# set grid cell corresponding to Nome to 1
paom_b <- stid_wrf_ind %>% 
  filter(stid == "PAOM") %>%
  dplyr::select(-stid) %>% unlist()
vals[paom_b[1], paom_b[2]] <- 1
r <- raster(vals, xmn = bounds[1], xmx = bounds[2],
            ymn = bounds[3], ymx = bounds[4],
            crs = wrf_crs)
r_df <- as.data.frame(r, xy = TRUE) %>%
  mutate(wrf_stid = as.factor(layer))

# alaska sf object
alaska <- us_states(states = "AK", resolution = "high")
ak_sf <- st_transform(alaska, wrf_crs)
# station coordinates as sf object
coords <- as.data.frame(select_stations[select_stations$stid == "PAOM", 
                                        c("stid", "lon", "lat")])
coords <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
coords <- st_transform(coords, wrf_crs)


# plot
theme_set(theme_bw())
p <- ggplot(data = ak_sf) + geom_sf(fill = "cornsilk") +
  xlab("Lng") + ylab("Lat") +
  
  
  geom_raster(data = r_df, aes(x, y, fill = wrf_stid),
              alpha = 0.5) +
  scale_fill_manual(values = c("white", "blue")) +
  geom_sf(data = coords, shape = 21, col = "white", cex = 0.1, bg = "white") +
  coord_sf(xlim = c(-1000000, 0), 
         ylim = c(-2891763, -2000000)) 
  
#------------------------------------------------------------------------------
