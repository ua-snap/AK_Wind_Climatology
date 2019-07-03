
# Scrap code for working with WRF data

#-- Setup ---------------------------------------------------------------------
library(ncdf4) 
library(raster) 
library(rgdal)
library(ggplot2)


workdir <- getwd()
datadir <- file.path(workdir, "data")
wrf_data_dir <- file.path(datadir, "WRF")

#-- WRF Data Explore ----------------------------------------------------------
u10_path <- file.path(wrf_data_dir, 
                      "u10_hourly_wrf_ERA-Interim_historical_1998.nc")
# open connection for pulling data
u10 <- nc_open(u10_path)

# variables: x, y, and time
xc <- ncvar_get(u10, varid = "xc")
yc <- ncvar_get(u10, varid = "yc")
ts <- ncvar_get(u10, varid = "time")
u10_vals <- ncvar_get(u10, "u10", count = rep(1, 3))

# testing extraction of data 
u10_t1 <- ncvar_get(u10, "u10", start = c(150, 150, 1), count = c(1, 1, 5))
u10_t2 <- ncvar_get(u10, "u10", start = c(150, 150, 2), count = rep(1, 3))
u10_t3 <- ncvar_get(u10, "u10", start = c(150, 150, 3), count = rep(1, 3))

# goal here: overlay the "grid" on a map of alaska
# approach: project alaska polygon data using projection from WRF data
library(sp)
library(rgdal)
library(sf)
library(raster)
df <- expand.grid(xc, yc)
names(df) <- c("x", "y")

us <- getData("GADM", country = "US", level = 1)
ak <- us[us$NAME_1 == "Alaska", ]
wrf_crs <- CRS("+units=m +proj=stere +lat_ts=64.0 +lon_0=-152.0 +lat_0=90.0 +x_0=0 +y_0=0 +a=6370000 +b=6370000")
ak <- spTransform(ak, wrf_crs)
grid <- SpatialPoints(df, proj4string = CRS(proj4string(ak)))
# Great, this appears to work!
plot(ak)
plot(grid, pch = ".", add = T)

# working on getting grid values for select stations
# trying with one observation so we only have a 2-d grid
t1 <- ncvar_get(u10, "u10", count = c(262, 262, 1))
# grid resolution is about 20 km
xres <- (max(xc) + abs(min(xc))) / (dim(t1)[1] - 1)
yres <- (max(yc) + abs(min(yc))) / (dim(t1)[2] - 1)
r <- raster(t1, xmn = min(xc) - xres/2, xmx = max(xc) + xres/2,
            ymn = min(yc) - yres/2, ymx = max(yc) + yres/2,
            crs = wrf_crs)
crs1 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
stations_sp <- select_stations %>% 
  dplyr::select(lon, lat) %>% 
  SpatialPoints(proj4string = crs1) %>%
  spTransform(wrf_crs)

test <- extract(r, stations_sp, method = "simple")
# can try to match extracted wind components
#   not useful if there are duplicate values
anyDuplicated(t1)
# cool, we can match stations based on predicted wind component
which(t1 == test, arr.ind = TRUE)
f1 <- function(x, set){which(set == x, arr.ind = TRUE)}
station_cells <- sapply(test, f1, t1)
#------------------------------------------------------------------------------