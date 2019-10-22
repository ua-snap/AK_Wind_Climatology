# Get the PAWI data for 1988-2018 for RT

library(dplyr)
library(lubridate)

# load data
pawi <- readRDS("data/AK_ASOS_stations_raw/PAWI_raw.Rds")

pawi_new <- pawi %>%
  filter(valid >= ymd("1988-01-01") & valid < ymd("2019-01-01")) %>%
  mutate(sped = sped * 0.44704) %>%
  rename(ts = valid, wd = drct, ws = sped) %>%
  select(ts, ws, wd)

write.csv(pawi_new, "data/PAWI_winds_1988-2018_raw.csv", 
          row.names = FALSE)
