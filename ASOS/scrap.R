library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)



#-- Setup ---------------------------------------------------------------------

workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")
figdir <- file.path(workdir, "figures")

# daily data
# all daily data
asos_daily_path <- file.path(datadir, "AK_ASOS_daily_allsites_wind_19700101_to_20190528.csv")
asos_daily <- fread(asos_daily_path)
# select and adjusted daily data
asos_daily_path <- file.path(datadir, "AK_ASOS_daily_select_adjspeed_19800101_to_20150101.csv")
asos_daily <- fread(asos_daily_path)

# select stations meta
select_stations_path <- file.path(datadir, "select_stations.Rds")
select_stations <- readRDS(select_stations_path)
stids <- select_stations$stid

# ASOS data tentatively selected for climatology
start_date <- as.Date("1985-01-01")
end_date <- start_date + years(30)
asos_select <- asos_daily %>% filter(date >= start_date &
                                     date <= end_date)

#-- geom_smooth ---------------------------------------------------------------
# having trouble getting geom_smooth to show on these data
asos_path <- file.path(asos_select_adj_dir, "PAFA_hour.Rds")
asos_station <- readRDS(asos_path)
asos_station <- asos_station %>%
  mutate(year = year(date), 
         month = month(date),
         day = day(date)) 
asos_monthly <- asos_station %>%
  group_by(month, year) %>%
  summarise(avg_sped = mean(sped_adj))
# will try custom df, this works
df <- data.frame(x = rnorm(40),
                 year = rep(1:5, 8))
df %>% mutate(y = x^3 + x^2) %>%
  ggplot(aes(x, y)) + 
  geom_point() + 
  geom_smooth()
# maybe because I need to group by year?
df %>% mutate(y = x^3 + x^2) %>%
  ggplot(aes(x, y, group = year)) + 
  geom_point() + 
  geom_smooth()
#------------------------------------------------------------------------------


#-- Wind rose -----------------------------------------------------------------------
library(openair)
library(circular)

# read fairbanks station data in
asos_station_path <- file.path(asos_select_adj_dir, "PAFA_hour.Rds")
asos_station <- readRDS(asos_station_path)
asos_station$drct <- as.numeric(asos_station$drct)
# openair package (blocky and junk)
save_path <- file.path(figdir, "PAFA_monthly_windrose.tiff")
tiff(save_path, width = 3000, height = 3000, res = 300)
windRose(asos_station, "sped_adj", "drct", 
         paddle = FALSE, type = "month", 
         breaks = c(0, 4, 8, 12, 16, 20),
         angle = 10)
dev.off()

save_path <- file.path(figdir, "PAFA_windrose.tiff")
tiff(save_path, width = 2500, height = 2500, res = 300)
windRose(asos_station, "sped_adj", "drct", 
         paddle = FALSE, breaks = c(0, 4, 8, 12, 16, 20),
         angle = 10)
dev.off()

pollutionRose(asos_station, "sped_adj")
# circular package (also junk)
windrose(as.data.frame(asos_station[, c("drct", "sped_adj")]),
         units = "degrees")
#------------------------------------------------------------------------------


#-- Avg speeds ----------------------------------------------------------------

# make "week" date variable for grouping by week Date
# daily ASOS data by station
asos_station <- asos_daily %>% filter(station == "PADK") %>%
  mutate(week = format(as.Date(date, origin = "1970-01-01"), 
                       format = "%Y-%U"), one = 1) %>% 
  group_by(week) %>% summarise(avg_sped_wk = mean(avg_sped), 
                             se = sd(avg_sped)/sqrt(sum(one)))

asos_station <- asos_daily %>% filter(station == stations[i]) %>%
  mutate(month = format(as.Date(date, origin = "1970-01-01"), 
                       format = "%Y-%m"), one = 1) %>% 
  group_by(month) %>% summarise(avg_sped_mo = mean(avg_sped),
                                med_sped_mo = median(avg_sped),
                               se = sd(avg_sped)/sqrt(sum(one))) %>%
  mutate(date = as.Date(paste0(month, "-01"),
                        origin = "1970-01-01"))
theme_set(theme_bw())

ggplot(asos_station, aes(date, med_sped_mo, group = 1)) + geom_line() +
  xlab("") + ylab("Avg Speed (mph)") + scale_x_date(date_labels = "%Y") + 
  ggtitle(stations[i])
#------------------------------------------------------------------------------


#-- change point --------------------------------------------------------------

###
# change point scratch
temp_m_amoc <- data.frame(cpt = numeric(), conf = numeric())
temp_v <- data.frame(cpt = numeric(), conf = numeric())

mvar_binseg_df <- data.frame(cp1 = numeric(), cp2 = numeric())
alpha <- 0.01

for(i in 1:56){
  asos_station <- asos_daily %>% filter(stid == stations[i]) %>%
    mutate(month = format(ymd(date), format = "%Y-%m"), one = 1) %>% 
    group_by(month) %>% summarise(avg_sped_mo = mean(avg_sped),
                                  #max_sped_mo = max(avg_sped),
                                  se = sd(avg_sped)/sqrt(sum(one))) %>%
    mutate(date = as.Date(paste0(month, "-01"),
                          origin = "1970-01-01"))
  
  value.ts <- ts(asos_station$avg_sped_mo, frequency = 12, 
                 start = c(1970, 1), end = c(2018, 12))  
  
  
  #pet <- pettitt.test(asos_station$avg_sped_mo)
  
  mvar_binseg <- cpt.meanvar(value.ts, method = "BinSeg", Q = 2, pen.value = alpha, penalty = "Asymptotic")
  
  mvar_cps <- cpts(mvar_binseg)
  no_cps <- length(mvar_cps)
  if( no_cps == 2){
    result <- mvar_cps
  } else if ( no_cps == 1) {
    result <- c(mvar_cps, NA)
  } else {result <- rep(NA, 2)}
  
  mvar_binseg_df <- rbind(mvar_binseg_df, result)
  
  #v_amoc <- cpt.var(value.ts, method = "AMOC", class = FALSE)
  #if(mvar_amoc[2] > alpha){
  #  mvar_bs <- 0
  #} #else {
  #mvar_bs <- cpt.meanvar(value.ts, method = "BinSeg", Q = 2, class = FALSE)
  #}
  
  #l <- length(mvar_bs)
  #if(l > 1){
  #  if(l == 2){cp = mvar_bs[1]} else {cp = mvar_bs[1:2]}
  #} 
}

cp <- cpts(m_bs)  
p <- ggplot(asos_station, aes(date, avg_sped_mo, group = 1)) + geom_line() +
  xlab("") + ylab("Avg Speed (mph)") + xlim(ymd("1970-01-01"), ymd("2019-01-01")) + 
  #scale_x_date(date_labels = date_labels, breaks = date_breaks) + 
  ggtitle(stations[i]) + geom_vline(xintercept = ymd("1985-01-01"), col = "blue") + 
  geom_vline(xintercept = ymd("2015-01-01"), col = "blue")

if(m_amoc[2] < 0.05){
  p <- p + geom_vline(xintercept = ymd(asos_station$date[cp]), col = "red")
}
if(v_amoc[2] < 0.05 & v_amoc[1] < 588){
  p <- p + geom_vline(xintercept = ymd(asos_station$date[v_amoc[1]]), col = "darkgreen")
}
if(pet$p.value <= 0.05){
  p <- p + geom_vline(xintercept = ymd(asos_station$date[pet$estimate]), col = "darkorange")
}
#------------------------------------------------------------------------------

#-- END -----------------------------------------------------------------------