# convert wind components from available GCMs/years of WRF downscaled output
#   for all locations
#-- Setup ---------------------------------------------------------------------
# generate boolean list for filename args
mk_args <- function() {
  tf <- c(TRUE, FALSE)
  split(cbind(rep(tf, each = 2), rep(tf, 2)), 1:4)
}

# 3-element bool vector arg
# args[1] TRUE = "u10", FALSE = "v10"
# args[2] TRUE = "GFDL-CM3", FALSE = "NCAR-CCSM4"
# args[3] TRUE = "historical", FALSE = "rcp85"
mk_paths <- function(args, u10 = TRUE) {
  var_str <- if(u10) "u10" else "v10"
  gcm_str <- if(args[1]) "GFDL-CM3" else "NCAR-CCSM4"
  pd_str <- if(args[2]) "historical" else "rcp85"
  list.files(
    paste0("../raw_data/WRF/hourly/", gcm_str, "/", pd_str, "/", var_str),
    full.names = TRUE
  )
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
library(snaptools)
library(lubridate)

source("../Nome_Mets/helpers.R")

args_lst <- mk_args()
u10_fns <- lapply(args_lst, mk_paths)
v10_fns <- lapply(args_lst, mk_paths, u10 = FALSE)

fn <- "../AK_Wind_Climatology_aux/data/AK_ASOS_select_stations.Rds"
stations <- readRDS(fn)
st_coords <- as.data.frame(stations[, c("lon", "lat")])

# cat("extracting u component")
# wrf_u10 <- lapply(u10_fns, function(x) {
#   wrf_get(x[1:2], st_coords, use_par = TRUE, cores = 64)
# })
# cat("extracting v component")
# wrf_v10 <- lapply(v10_fns, function(x) {
#   wrf_get(x[1:2], st_coords, use_par = TRUE, cores = 64)
# })

# mclapply fails on first run with wrf_get for some reason
try(wrf_fail <- wrf_get(u10_fns[[1]], st_coords[1, ], use_par = TRUE, cores = 64))
cat("extracting u10 component...\n")
t1 <- system.time(
  wrf_u10 <- lapply(u10_fns, wrf_get, st_coords, use_par = TRUE, cores = 64)
)
cat(paste0("u10 component extracted, ", t1[3], " seconds\n"))
cat("extracting v10 component...\n")
t1 <- system.time(
  wrf_v10 <- lapply(v10_fns, wrf_get, st_coords, use_par = TRUE, cores = 64)
)
cat(paste0("v10 component extracted, ", t1[3], " seconds\n"))

wrf_uv <- list()
for(i in 1:4) {
  ts <- ymd_hms(rownames(wrf_u10[[i]]))
  wrf_uv[[i]] <- data.frame(
    u10 = unlist(wrf_u10[[i]]),
    v10 = unlist(wrf_v10[[i]]),
    ts = rep(ts, ncol(wrf_u10[[i]]))
  )
}

cat("converting wind components...\n")
t1 <- system.time(
  wrf_wdws <- lapply(wrf_uv, uv2wdws)
)
cat(paste0("wind components converted, ", t1[3], " seconds\n"))

# split data frames into lists
n <- nrow(stations)
stids <- stations$stid
wrf_wdws <- lapply(wrf_wdws, function(x) {
  out_lst <- split(x, rep(1:n, each = nrow(x)/n))
  names(out_lst) <- stids
  out_lst
})

names(wrf_wdws) <- c("CM3h", "CM3f", "CCSM4h", "CCSM4f")

fn_out <- "../AK_Wind_Climatology_aux/data/wrf_wdws.Rds"
saveRDS(wrf_wdws, fn_out, compress = FALSE)
cat(paste0("winds saved as ", fn_out, "\n"))

#------------------------------------------------------------------------------
