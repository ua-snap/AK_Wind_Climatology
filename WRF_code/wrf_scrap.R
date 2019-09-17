
# Scrap code for working with WRF data

#-- Setup ---------------------------------------------------------------------
library(ncdf4) 
library(raster) 
library(rgdal)
library(ggplot2)
library(dplyr)
library(lubridate)


workdir <- getwd()
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures", "wrf_scrap")
wrf_data_dir <- file.path(datadir, "WRF")
#------------------------------------------------------------------------------

#-- WRF grid coordinates ------------------------------------------------------
# James asked if I could get the coordinates of the corners of the WRF grid.

# couldn't get this section finished - the grid seems to be upside down when 
#   re-projected to long lat
library(ncdf4) 
library(sf)

workdir <- getwd()
datadir <- file.path(workdir, "data")

wrf_output_dir <- "F:/AK_Wind_Climatology/data/WRF_output"
u10_dir <- file.path(wrf_output_dir, "ERA_u10")
u10_fname <- "u10_hourly_wrf_ERA-Interim_historical_"
u10_path <- file.path(u10_dir, paste0(u10_fname, 1980, ".nc"))
u10 <- nc_open(u10_path)
# grid is based on centroids, so need to determine correct extent for building
#   corresponding raster
# extract x and y from u10
xc <- ncvar_get(u10, varid = "xc")
yc <- ncvar_get(u10, varid = "yc")
nc <- length(xc)
# close connection
nc_close(u10)
# resolution of grid would be the difference between two centroids
hres <- (xc[2] - xc[1])/2
# data frame of coordinates of WRF corners
xnew <- seq(xc[1] - hres, xc[262] + hres, by = 20000)
ynew <- seq(yc[262] - hres, yc[1] + hres, by = 20000)
lon <- c(rep(xnew[1], 263), rep(xnew[263], 263), rep(xnew, 2))
lat <- c(rep(ynew, 2), rep(ynew[1], 263), rep(ynew[263], 263))
out_coords <- data.frame(lon, lat)
# CRS from WRF netcdfs
wrf_crs <- "+units=m +proj=stere +lat_ts=64.0 +lon_0=-152.0 +lat_0=90.0 +x_0=0 +y_0=0 +a=6370000 +b=6370000"
# make data spatial
out_coords_sf <- st_as_sf(out_coords, coords = c("lon", "lat"), crs = wrf_crs)
# CRS for output coordinates
new_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
# transform
out_coords_sf <- st_transform(out_coords_sf, crs = 4326)
plot(out_coords_sf, pch = ".")

coords <- data.frame(lon = rep(c(xc[1] - hres, xc[nc] + hres), 2),
                     lat = rep(c(yc[nc] - hres, yc[1] + hres), each = 2),
                     point = c("Lower Left", "Lower Right", 
                               "Upper Left", "Upper Right"))
# CRS from WRF netcdfs
wrf_crs <- "+units=m +proj=stere +lat_ts=64.0 +lon_0=-152.0 +lat_0=90.0 +x_0=0 +y_0=0 +a=6370000 +b=6370000"
# make data spatial
coords_sf <- st_as_sf(coords, coords = c("lon", "lat"), crs = wrf_crs)

p1 <- st_point(c(-2620000, -5402425))
p1 <- st_point(c(2620000, -5402425))
sfc <- st_sfc(p1, crs = wrf_crs)
st_transform(sfc, 4326)

anc <- nc_open(file.path(datadir, "geo_em.d01.nc"))

xlat_m <- ncvar_get(anc, varid = "XLAT_M")
xlon_m <- ncvar_get(anc, varid = "XLONG_M")

# CRS for output coordinates
new_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
# transform
st_transform(coords_sf, crs = 4326)

library(sp)
library(rgdal)
library(dplyr)

wrf_crs <- CRS("+units=m +proj=stere +lat_ts=64.0 +lon_0=-152.0 +lat_0=90.0 +x_0=0 +y_0=0 +a=6370000 +b=6370000")
new_crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")

coords %>% 
  select(lon, lat) %>%
  SpatialPoints(proj4string = wrf_crs) %>%
  spTransform(new_crs)
#------------------------------------------------------------------------------

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



# COMPARE TIME SERIES BETWEEN WRF INDICES FROM RICK
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
asos <- readRDS(file.path(asos_adj_dir, "PAFA.Rds"))
wrf_output_dir <- "F:/Wind_Climatology/data/WRF_output"
u10_dir <- file.path(wrf_output_dir, "ERA_u10")
u10_fname <- "u10_hourly_wrf_ERA-Interim_historical_"
u10_path <- file.path(u10_dir, paste0(u10_fname, 2014, ".nc"))
v10_dir <- file.path(wrf_output_dir, "ERA_v10")
v10_fname <- "v10_hourly_wrf_ERA-Interim_historical_"
v10_path <- file.path(v10_dir, paste0(v10_fname, 2014, ".nc"))
u10 <- nc_open(u10_path)
v10 <- nc_open(v10_path)
# values for fairbanks provided by Rick
wrf_i <- 127
wrf_j <- 141
era_u10 <- ncvar_get(u10, "u10", start = c(wrf_i, wrf_j, 1),
                 count = c(1, 1, -1))
era_v10 <- ncvar_get(v10, "v10", start = c(wrf_i, wrf_j, 1),
                     count = c(1, 1, -1))
nc_close(u10)
nc_close(v10)
start <- ymd_hms("2014-01-01 00:00:00")
end <- ymd_hms("2014-12-31 23:00:00")
era_ts <- seq(start, end, by = "hour")
era <- bind_cols(data.frame(ts = era_ts), 
                 as.data.frame(uv2wdws(era_u10, era_v10)))
asos_event <- asos %>% 
  filter(t_round >= start & t_round <= end) %>%
  dplyr::select(t_round, sped_adj) %>% 
  rename(ts = t_round, ASOS = sped_adj)
event_df <- era %>% 
  dplyr::select(ts, ws) %>%
  rename(ERA = ws) %>%
  left_join(asos_event, by = "ts") %>%
  gather("Source", "sped", -ts) %>%
  mutate(Date = ymd(format(ts, "%Y-%m-%d"))) %>%
  group_by(Source, Date) %>%
  summarise(avg_sped = mean(sped, na.rm = TRUE))
p <- ggplot(event_df, aes(Date, avg_sped, color = Source)) + 
  geom_line(size = 1) +
  xlab("") + ylab("Speed (mph)") 


anc <- nc_open(file.path(datadir, "geo_em.d01.nc"))
xlat_m <- ncvar_get(anc, "XLAT_M")
xlon_m <- ncvar_get(anc, "XLONG_M")
clat <- ncvar_get(anc, "CLAT")

coords <- array(c(64.80389, -147.87611), dim = c(1, 1, 2))
coords <- c(64.80389, -147.87611)
coords <- c(58.35497, -134.57628)
wrf_coords <- array(c(xlat_m, xlon_m), dim = c(262, 262, 2))
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
temp <- apply(wrf_coords, c(1, 2), euc.dist, coords)
which(temp == min(temp), arr.ind = TRUE)

#------------------------------------------------------------------------------

#-- WRF Leap days? ------------------------------------------------------------
cm3_dir <- "F:/Wind_Climatology/data/WRF_output/CM3_u10"
u10_path <- file.path(cm3_dir, 
                      "u10_hourly_wrf_GFDL-CM3_historical_1980.nc")
# open connection for pulling data
u10 <- nc_open(u10_path)
ts_1980 <- ncvar_get(u10, varid = "time")
nc_close(u10)

u10_path <- file.path(cm3_dir, 
                      "u10_hourly_wrf_GFDL-CM3_historical_1981.nc")
# open connection for pulling data
u10 <- nc_open(u10_path)
ts_1981 <- ncvar_get(u10, varid = "time")
nc_close(u10)

length(ts_1980)
length(ts_1981)

#------------------------------------------------------------------------------

#-- Quantile Mapping Errors ---------------------------------------------------
# did a mistake in the quantile mapping of WRF data cause a problem?
#   ~10% differences in count of high wind events
era_adj_dir <- file.path(datadir, "ERA_stations_adj")
cm3_adj_dir <- file.path(datadir, "CM3_stations_adj")
ccsm4_adj_dir <- file.path(datadir, "CCSM4_stations_adj")

# first, load some quantile mapped data
paom_era <- readRDS(file.path(era_adj_dir, "PAOM_era_adj.Rds"))
paom_erah <- paom_era %>% filter(ts < ymd("2006-01-01"))
paom_cm3h <- readRDS(file.path(cm3_adj_dir, "PAOM_cm3h_adj.Rds"))
paom_cm3f <- readRDS(file.path(cm3_adj_dir, "PAOM_cm3f_adj.Rds"))
paom_ccsm4h <- readRDS(file.path(ccsm4_adj_dir, "PAOM_ccsm4h_adj.Rds"))
paom_ccsm4f <- readRDS(file.path(ccsm4_adj_dir, "PAOM_ccsm4f_adj.Rds"))

# original qMapWind function
qMapWindOrig <- function(obs = NULL, sim, 
                     ret.deltas = FALSE, 
                     use.deltas = NULL){
  
  if(is.null(use.deltas)){
    qn <- min(length(obs), length(sim))
    qx <- quantile(sim, seq(0, 1, length.out = qn), type = 8)
    qy <- quantile(obs, seq(0, 1, length.out = qn), type = 8)
    q_deltas <- qx - qy
  } else {
    qx <- quantile(sim, seq(0, 1, length.out = length(use.deltas)), 
                   type = 8)
    q_deltas = use.deltas
  }
  
  # bin "sim" observations into quantiles. Will use these indices to 
  #   index deltas vector for adjustment
  qi <- .bincode(sim, qx, include.lowest = TRUE)
  
  # duplicate quantiles are not represented in this binning,
  #   need to represent for all deltas to be applied
  dup_qx <- unique(qx[duplicated(qx)])
  dup_qi <- sort(unique(qi[which(qx[qi + 1] %in% dup_qx)]))
  last_dupi <- c((dup_qi - 1)[-1], length(qx))
  dup_qi <- dup_qi + as.numeric(paste0("0.", last_dupi))
  
  # distribute duplicated quantile indices in place of repeated 
  tempFun <- function(dup_qi, qi){
    end <- as.integer(substring(round(dup_qi - trunc(dup_qi), 3), 3))
    dup_qi <- trunc(dup_qi)
    qij <- which(qi == dup_qi)
    n <- length(qij)
    qis <- rep(0, n)
    suppressWarnings(qis[rep(TRUE, n)] <- dup_qi:end)
    names(qis) <- qij
    qis
  }
  # and replace qi's with these recycled indices
  new_qi <- unlist(lapply(dup_qi, tempFun, qi))
  qij <- as.integer(names(new_qi))
  qi[qij] <- new_qi
  
  sim_adj <- sim - as.numeric(q_deltas)[qi]
  # return adjusted
  if(ret.deltas == TRUE){
    return(list(deltas = q_deltas, sim_adj = sim_adj))
  } else {return(sim_adj)}
}


# New qMapWind function
qMapWindNew <- function(obs = NULL, sim, 
                     ret.deltas = FALSE, 
                     use.deltas = NULL){
  
  if(is.null(use.deltas)){
    qn <- min(length(obs), length(sim))
    qx <- quantile(sim, seq(0, 1, length.out = qn), type = 8)
    qy <- quantile(obs, seq(0, 1, length.out = qn), type = 8)
    q_deltas <- qx - qy
  } else {
    qx <- quantile(sim, seq(0, 1, length.out = length(use.deltas)), 
                   type = 8)
    q_deltas = use.deltas
  }
  
  # bin "sim" observations into quantiles. Will use these indices to 
  #   index deltas vector for adjustment
  qi <- .bincode(sim, qx, include.lowest = TRUE)
  
  df <- data.frame(lower=sort(unique(qi)), freq=as.integer(table(qi)))
  df$upper <- c(df$lower[-1] - df$lower[-nrow(df)], 1) + df$lower - 1
  # want to omit this adjustment if the first quantile is also the first
  #   duplicate
  ub <- df$lower != 1
  df$upper[ub] <- df$upper[ub] - as.numeric(df$upper[ub] > df$lower[ub] & 
                                              qx[df$upper[ub]] < qx[df$upper[ub] + 1])
  
  recycled <- apply(df, 1, function(x) {
    out <- rep(x["lower"]:x["upper"], length.out=x["freq"])
    
    return(out)
  })
  
  qi <- unlist(recycled)[order(order(qi))]
  
  sim_adj <- sim - as.numeric(q_deltas)[qi]
  # return adjusted
  if(ret.deltas == TRUE){
    return(list(deltas = q_deltas, sim_adj = sim_adj))
  } else {return(sim_adj)}
}

sim <- paom_cm3h$sped
obs <- paom_erah$sped_adj
adj <- qMapWindNew(obs = obs, sim = sim, ret.deltas = TRUE)
paom_cm3h$sped_adj_new <- adj[[1]]

df <- paom_erah %>%
  select(ts, sped_adj) %>%
  rename(ERA = sped_adj) %>%
  left_join(paom_cm3h, by = "ts") %>%
  select(ts, ERA, sped_adj) %>%
  rename(CM3h = sped_adj) %>%
  gather("Source", "Speed", -ts)

df %>%
  filter(Speed >= 30) %>%
  group_by(Source) %>%
  summarise(Count = n()) %>%
  rename(`Nome Source` = Source)

sim <- paom_cm3f$sped
adj <- qMapWindNew(sim = sim, use.deltas = adj[[2]])
paom_cm3f$sped_adj_new <- adj

# counts of winds over 30 from CM3 and ERA between 2006-01-01 and 
#   2014-12-31. Herein lie zeh issue
paom_era %>% 
  filter(ts >= ymd("2006-01-01")) %>%
  select(ts, sped_adj) %>%
  rename(ERA = sped_adj) %>%
  left_join(paom_cm3f, by = "ts") %>%
  select(ts, ERA, sped_adj) %>%
  rename(CM3f = sped_adj) %>%
  filter(ts < ymd("2015-01-01")) %>%
  gather("Source", "Speed", -ts) %>%
  filter(Speed >= 30) %>%
  group_by(Source) %>%
  summarise(Count = n()) %>%
  rename(`Nome Source` = Source)

# counts of winds over 30 from CCSM4 and ERA between 2006-01-01 and 
#   2014-12-31. 
paom_era %>% 
  filter(ts >= ymd("2006-01-01")) %>%
  select(ts, sped_adj) %>%
  rename(ERA = sped_adj) %>%
  left_join(paom_ccsm4f, by = "ts") %>%
  select(ts, ERA, sped_adj) %>%
  rename(CCSM4f = sped_adj) %>%
  filter(ts < ymd("2015-01-01")) %>%
  gather("Source", "Speed", -ts) %>%
  filter(Speed >= 30) %>%
  group_by(Source) %>%
  summarise(Count = n()) %>%
  rename(`Nome Source` = Source)


# generate ECDFs comparing WRF historical and future models
# CM3
temp_cm3f <- paom_cm3f %>%
  select(ts, sped) %>%
  rename(CM3F = sped) %>%
  gather("Source", "Speed", -ts)

df1 <- paom_cm3h %>%
  select(ts, sped) %>%
  rename(CM3H = sped) %>%
  gather("Source", "Speed", -ts) %>%
  bind_rows(temp_cm3f)

# CCSM4
temp_ccsm4f <- paom_ccsm4f %>%
  select(ts, sped) %>%
  rename(CCSM4F = sped) %>%
  gather("Source", "Speed", -ts)

df2 <- paom_ccsm4h %>%
  select(ts, sped) %>%
  rename(CCSM4H = sped) %>%
  gather("Source", "Speed", -ts) %>%
  bind_rows(temp_ccsm4f)

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

xmax <- 50
p1 <- ggplot(df1, aes(Speed, color = Source)) + 
  stat_ecdf(size = 0.5) + 
  xlab("Wind Speed (MPH)") + ylab("Cumulative Probability") + 
  xlim(c(0, xmax)) + scale_color_discrete(name = "Model: ", 
                                         labels = c("Future", "Historical")) + 
  theme(legend.position = "bottom") +
  ggtitle("CM3")

# corrected data
p2 <- ggplot(df2, aes(Speed, color = Source)) + 
  stat_ecdf(size = 0.5) + 
  xlab("Wind Speed (MPH)") + ylab(element_blank()) + 
  xlim(c(0, xmax))  + ggtitle("CCSM4")

# legend code adapted from:
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
tmp <- ggplot_gtable(ggplot_build(p1))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
mylegend <- tmp$grobs[[leg]]

p <- arrangeGrob(arrangeGrob(p1 + theme(legend.position = "none"),
                             p2 + theme(legend.position = "none"), 
                             nrow = 1),
                 mylegend, nrow=2, heights = c(10, 1))

fig_path <- file.path(figdir, "PAOM_WRF_historical_future_ECDFs.png")
ggsave(fig_path, p, dev = "png", width = 8.93, height = 6)
#------------------------------------------------------------------------------

#-- Bar Plots for Wind Speeds over 30 -----------------------------------------
# did a mistake in the quantile mapping of WRF data cause a problem?
#   ~10% differences in count of high wind events
asos_adj_dir <- file.path(datadir, "AK_ASOS_stations_adj")
era_adj_dir <- file.path(datadir, "ERA_stations_adj")
cm3_adj_dir <- file.path(datadir, "CM3_stations_adj")
ccsm4_adj_dir <- file.path(datadir, "CCSM4_stations_adj")

# first, load some quantile mapped data
# Nome
paom_asos_30 <- readRDS(file.path(asos_adj_dir, "PAOM.Rds")) %>%
  filter(t_round < ymd("2015-01-01"),
         sped_adj >= 30) %>%
  rename(ts = t_round, ASOS = sped_adj) %>%
  select(ts, ASOS) %>%
  gather("Source", "Speed", -ts)
paom_era_30 <- readRDS(file.path(era_adj_dir, "PAOM_era_adj.Rds")) %>%
  filter(ts < ymd("2015-01-01"),
         sped_adj >= 30) %>%
  rename(ERA = sped_adj) %>%
  select(ts, ERA)%>%
  gather("Source", "Speed", -ts)
paom_cm3_30 <- readRDS(file.path(cm3_adj_dir, "PAOM_cm3h_adj.Rds")) %>%
  bind_rows(readRDS(file.path(cm3_adj_dir, "PAOM_cm3f_adj.Rds"))) %>%
  filter(ts < ymd("2015-01-01"),
         sped_adj >= 30) %>%
  rename(CM3 = sped_adj) %>%
  select(ts, CM3)%>%
  gather("Source", "Speed", -ts)
paom_ccsm4_30 <- readRDS(file.path(ccsm4_adj_dir, "PAOM_ccsm4h_adj.Rds")) %>%
  bind_rows(readRDS(file.path(ccsm4_adj_dir, "PAOM_ccsm4f_adj.Rds"))) %>%
  filter(ts < ymd("2015-01-01"),
         sped_adj >= 30) %>%
  rename(CCSM4 = sped_adj) %>%
  select(ts, CCSM4)%>%
  gather("Source", "Speed", -ts)

df <- paom_asos_30 %>%
  bind_rows(paom_era_30, paom_cm3_30, paom_ccsm4_30) %>%
  mutate(Y = factor(format(ts, "%Y"), 
                    levels = as.character(1980:2014)),
         Source = factor(Source, levels = c("ASOS", "ERA", 
                                            "CM3", "CCSM4"))) %>%
  group_by(Source, Y) %>%
  summarise(count = n())

p1 <- ggplot(df, aes(x = Y, y = count, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_classic() + xlab("Year") + ylab("Count") +
  ggtitle("Nome")

fig_path <- file.path(figdir, "Over30_barplot_PAOM.png")
ggsave(fig_path, p1, dev = "png", width = 15, height = 8)

# Barrow
pabr_asos_30 <- readRDS(file.path(asos_adj_dir, "PABR.Rds")) %>%
  filter(t_round < ymd("2015-01-01"),
         sped_adj >= 30) %>%
  rename(ts = t_round, ASOS = sped_adj) %>%
  select(ts, ASOS) %>%
  gather("Source", "Speed", -ts)
pabr_era_30 <- readRDS(file.path(era_adj_dir, "PABR_era_adj.Rds")) %>%
  filter(ts < ymd("2015-01-01"),
         sped_adj >= 30) %>%
  rename(ERA = sped_adj) %>%
  select(ts, ERA)%>%
  gather("Source", "Speed", -ts)
pabr_cm3_30 <- readRDS(file.path(cm3_adj_dir, "PABR_cm3h_adj.Rds")) %>%
  bind_rows(readRDS(file.path(cm3_adj_dir, "PABR_cm3f_adj.Rds"))) %>%
  filter(ts < ymd("2015-01-01"),
         sped_adj >= 30) %>%
  rename(CM3 = sped_adj) %>%
  select(ts, CM3)%>%
  gather("Source", "Speed", -ts)
pabr_ccsm4_30 <- readRDS(file.path(ccsm4_adj_dir, "PABR_ccsm4h_adj.Rds")) %>%
  bind_rows(readRDS(file.path(ccsm4_adj_dir, "PABR_ccsm4f_adj.Rds"))) %>%
  filter(ts < ymd("2015-01-01"),
         sped_adj >= 30) %>%
  rename(CCSM4 = sped_adj) %>%
  select(ts, CCSM4)%>%
  gather("Source", "Speed", -ts)

df <- pabr_asos_30 %>%
  bind_rows(pabr_era_30, pabr_cm3_30, pabr_ccsm4_30) %>%
  mutate(Y = factor(format(ts, "%Y"), 
                    levels = as.character(1980:2014)),
         Source = factor(Source, levels = c("ASOS", "ERA", 
                                            "CM3", "CCSM4"))) %>%
  group_by(Source, Y) %>%
  summarise(count = n())

p2 <- ggplot(df, aes(x = Y, y = count, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_classic() + xlab("Year") + ylab("Count") +
  ggtitle("Barrow")

fig_path <- file.path(figdir, "Over30_barplot_PABR.png")
ggsave(fig_path, p2, dev = "png", width = 15, height = 8)
