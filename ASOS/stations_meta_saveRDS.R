# helper code to create R object containing AK ASOS sites
library(data.table)
workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")

asos_path <- file.path(datadir, "AK_ASOS_allsites_wind_19700101_to_20190528.txt")
asos <- read.csv(asos_path)

sites <- levels(asos$station)
save_path <- file.path(datadir, "AK_ASOS_sites.rds")

saveRDS(sites, save_path)
