
# Scrap code for working with ASOS data



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)

workdir <- getwd()
datadir <- file.path(workdir, "data")
asos_dir <- file.path(datadir, "AK_ASOS_stations_adj")
figdir <- file.path(workdir, "figures")

# WRF directories
era_adj_dir <- file.path(datadir, "ERA_stations_adj")

# daily data
# all daily data
asos_daily_path <- file.path(datadir, "AK_ASOS_daily_allsites_wind_19700101_to_20190528.csv")
asos_daily <- fread(asos_daily_path)
# select and adjusted daily data
asos_daily_path <- file.path(datadir, "AK_ASOS_daily_select_adjspeed_19800101_to_20150101.csv")
asos_daily <- fread(asos_daily_path)

# hourly data
asos_qc_dir <- file.path(datadir, "AK_ASOS_stations_qc")

# select stations meta
select_stations_path <- file.path(datadir, "select_stations.Rds")
select_stations <- readRDS(select_stations_path)
stids <- select_stations$stid

# ASOS data tentatively selected for climatology
start_date <- as.Date("1985-01-01")
end_date <- start_date + years(30)
asos_select <- asos_daily %>% filter(date >= start_date &
                                     date <= end_date)

# changepoints
cpts_path <- file.path(asos_dir, "cpts_df.Rds")
cpts_df <- readRDS(cpts_path)

#------------------------------------------------------------------------------

#-- Count Check for Bruce -----------------------------------------------------
# check counts of observations with wind speed and direction > 0 
# write a function that reads in a subset of stations into one data frame
read_ASOS <- function(stids){
  require(dplyr)
  
  dfs <- lapply(stids, function(x){
    asos_dir <- file.path(getwd(), "data", "AK_ASOS_stations_adj")
    df <- readRDS(file.path(asos_dir, paste0(x, ".Rds")))
    df
  })
  bind_rows(dfs)
}
stids <- c("PASC", "PAEN", "PAFB", "PAIM", "PAIL", "PADK")
asos <- read_ASOS(stids) %>%
  filter(drct > 0 & sped_adj > 0) %>%
  group_by(stid) %>%
  summarise(count = n())

#------------------------------------------------------------------------------

#-- Monthly Analysis ----------------------------------------------------------
# Preliminary monthly trend analysis of station data (regression of monthly
#   average)

library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Work directory
workdir <- file.path("C:/Users/Keal/Desktop/IARC/Wind_Climatology/")
datadir <- file.path(workdir, "data")
asos_select_adj_dir <- file.path(datadir, "AK_ASOS_allsites_wind_19800101_to_20150101_adj")

# select stations meta
select_stations_path <- file.path(datadir, "select_stations.Rds")
select_stations <- readRDS(select_stations_path)
stids <- select_stations$stid

# read data
asos_path <- file.path(asos_select_adj_dir, "PAFA_hour.Rds")
asos_station <- readRDS(asos_path)
asos_station <- asos_station %>%
  mutate(year = year(date), 
         month = month(date),
         day = day(date)) 

asos_monthly <- asos_station %>%
  group_by(month, year) %>%
  summarise(avg_sped = mean(sped_adj))

# function for labelling equation on plot 
# found at https://stackoverflow.com/questions/7549694/adding-regression-line-equation-and-r2-on-graph
# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

lm_eqn <- function(df){
  m <- lm(avg_sped ~ x, df);
  eq <- substitute(italic(beta) == b*","~~italic(r)^2~"="~r2, 
                   list(b = as.character(format(coef(m)[2], digits = 2)),
                        r2 = format(round(summary(m)$r.squared, digits = 2), nsmall = 2)))
  as.character(as.expression(eq));
}
lm_pval <- function(df){
  m <- lm(avg_sped ~ x, df);
  pval <- substitute(italic(p) == a, 
                     list(a = as.character(format(round(summary(m)$coefficients[2, 4], 
                                                        digits = 3), nsmall = 3))))
  as.character(as.expression(pval));
}
# avg speed vs year, by month
asos_monthly$x <- asos_monthly$year - 1980
asos_jan <- asos_monthly %>% filter(month == 1)
asos_may <- asos_monthly %>% filter(month == 5)

fit1 <- glm(avg_sped ~ x, 
            data = asos_jan, 
            family = Gamma)
fit5 <- glm(avg_sped ~ x, 
            data = asos_may, 
            family = Gamma)
lm1 <- lm(avg_sped ~ x, 
          data = asos_jan)
lm2 <- lm(avg_sped ~ x, 
          data = asos_may)
# faceted plot with regressoin eq's
# from this so post: https://stackoverflow.com/questions/19699858/ggplot-adding-regression-line-equation-and-r2-with-facet
# re-level months
asos_monthly$month <- as.factor(asos_monthly$month)
levels(asos_monthly$month) <- month.abb
# create equation labels
eq <- ddply(asos_monthly,.(month),lm_eqn)
pval <- ddply(asos_monthly, .(month), lm_pval)

asos_monthly %>% 
  ggplot(aes(year, avg_sped)) + 
  geom_point() + 
  geom_smooth(method = "glm",
              method.args = list(family = "Gamma")) + 
  geom_text(data = eq, aes(x = 2000, y = 12, label = V1), 
            parse = TRUE, inherit.aes = FALSE,
            size = 3) +
  geom_text(data = pval, aes(x = 2005, y = 10, label = V1), 
            parse = TRUE, inherit.aes = FALSE,
            size = 4) +
  facet_wrap(~month) + 
  xlab("Year") + ylab("Avg Wind Speed (mph)") + 
  ggtitle("PAFA")

p + facet_wrap(~month)

#------------------------------------------------------------------------------

#-- ERA-Interim high wind events mismatch -------------------------------------
# Sarah noticed discrepancies between observed and ERA-interim data for high
#   wind events
library(dplyr)
library(lubridate)
library(ggplot2)

era <- readRDS(file.path(era_adj_dir, "PAOM_era_adj.Rds"))
asos <- readRDS(file.path(asos_dir, "PAOM.Rds"))

start <- ymd_hms("2011-11-08 20:00:00")
end <- ymd_hms("2011-11-10 23:59:59")

asos_event <- asos %>% 
  filter(t_round >= start & t_round <= end) %>%
  select(t_round, sped_adj) %>% 
  rename(ts = t_round, asos = sped_adj)

event_df <- era %>% 
  filter(ts >= start & ts <= end) %>%
  select(ts, sped_adj) %>%
  rename(era = sped_adj) %>%
  left_join(asos_event, by = "ts") %>%
  gather("source", "sped", -ts)
  
ggplot(event_df, aes(ts, sped, col = source)) + 
  geom_line(size = 1) +
  xlab("") + ylab("Speed (mph)") + ggtitle("PAOM")
#------------------------------------------------------------------------------

#-- Quantile Mapping ----------------------------------------------------------
# quantile mapping performed using custom function,
#   still would like to use quantile mapping method from 
#   qmap package, or at least use it to inform cutsom function

library(qmap)

workdir <- getwd()
datadir <- file.path(workdir, "data")
asos_dir <- file.path(datadir, "AK_ASOS_stations_adj")
cpts_df <- readRDS(file.path(asos_dir, "cpts_df.Rds"))
# PALU has m1 < m2, PABT has m1 > m2
pabt <- readRDS(file.path(asos_dir, "PABT.Rds"))
palu <- readRDS(file.path(asos_dir, "PALU.Rds"))
# break into periods
pabt_sim <- pabt$sped[pabt$period == 1]
pabt_obs <- pabt$sped[pabt$period == 2]
palu_sim <- palu$sped[palu$period == 1]
palu_obs <- palu$sped[palu$period == 2]

# perform quantile mapping using qmap
qm_pabt <- fitQmapQUANT(pabt_obs, pabt_sim)
qm_palu <- fitQmapQUANT(palu_obs, palu_sim, wet.day = FALSE)
new_pabt <- doQmapQUANT(pabt_sim, qm_pabt)
new_palu <- doQmapQUANT(palu_sim, qm_palu)

ggECDF_compare(pabt_obs, pabt_sim, new_pabt)
# when m1 < m2, no zero values (but very close)
ggECDF_compare(palu_obs, palu_sim, new_palu)

# this is what the qmap functions are doing:
hn <- min(length(palu_sim), length(palu_obs))
hn <- 1000
xs <- quantile(palu_sim, seq(0, 1, length.out = hn), type = 8)
ys <- quantile(palu_obs, seq(0, 1, length.out = hn), type = 8)
xnew <- quantile(xs, seq(0, 1, 0.01), type = 8)
booty <- quantile(ys, seq(0, 1, 0.01), type = 8)

q_diff <- xs -ys

# sample just to try and figure shit out
samp_x <- palu_sim[61:65]
est <- approx(xnew, booty, xout = samp_x)$y
# trying to recreate this, thinking just fitting a line between
#   the first two unique quantiles. 
# If this is the case, then the first value would be the intercept
# second unique x quantile is 2.3
j <- match(2.3, xnew) -1
# intecept of line is mean of all y quantiles at 0
mean(booty[1:j]) == est[1]

# attmepting to quantile map with non-unique quantiles
dup_qx <- unique(xs[duplicated(xs)])
qi <- .bincode(palu_sim, xs, include.lowest = TRUE)
dup_qi <- unique(qi[which(xs[qi + 1] %in% dup_qx)])
# Could try adding last index of duplicated quantiles as names
#   so could include "sampling" operation in lapply
dup_qi <- sort(dup_qi)
last_dupi <- c((dup_qi - 1)[-1], length(xs))
dup_qi <- dup_qi + as.numeric(paste0("0.", last_dupi))

# extract duplicate
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

new_qi <- unlist(lapply(dup_qi, tempFun, qi))
qij <- as.integer(names(new_qi))
qi[qij] <- new_qi

#q_ids <- q_ids[order(q_ids)]
new_palu <- palu_sim - q_diff[qi]

new_palu <- qMapWind(palu_obs, palu_sim)
ggECDF_compare(palu_obs, palu_sim, new_palu)


# suggestion from stack overflow
hn <- 1000
x <- palu_sim
qx <- quantile(x, seq(0, 1, length.out = hn), type = 8)
q_i <- .bincode(x, qx, include.lowest = TRUE)
## Start off with the code provided in the question...
#  1. For each distinct q_i, calculate the number of occurrances, and how far we can recycle it
df <- data.frame(lower=sort(unique(q_i)), freq=as.integer(table(q_i)))
df$upper <- c(df$lower[-1] - df$lower[-nrow(df)], 1) + df$lower - 1
df$upper <- df$upper - as.numeric(df$upper > df$lower & qx[df$upper] < qx[df$upper + 1])

#  2. Identify when there's a (single) number we can't recycle, and identify which position it's in
#     e.g. is it the third time q_i == 10?
df$special_case <- rep(NA, nrow(df))
df$special_case[df$lower < df$upper] <- sapply(df$lower[df$lower < df$upper], function(low) {
  bin <- x[q_i==low]
  if(length(unique(bin)) > 1) {
    return(match(min(bin), bin))} 
  else return(NA)})

# 3. For each row of df, get a vector of (possibly recycled) numbers
recycled <- apply(df, 1, function(x) {
  out <- rep(x["lower"]:x["upper"], length.out=x["freq"])
  
  # This part modifies the vector created to handle the 'special case'
  if(!is.na(x["special_case"])) {
    out[x["special_case"]] <- x["lower"]
    if(x["special_case"] < x["freq"]) {
      out[(x["special_case"]+1):x["freq"]] <- out[x["special_case"]:(x["freq"]-1)]
    }
  }
  return(out)
})

# 3b. Make this follow the same order as q_i
q_i_final <- unlist(recycled)[order(order(q_i))]

q_i_final
[1] 10  1 19 11  5 19 13 10 17 16 17  6  2 15  3  9 11  7  1 16  2  3  5 13  6



#------------------------------------------------------------------------------

#-- Find Qmap Warning ---------------------------------------------------------
# There was a warning for qmapping with one of the stations
#   Goal here to find which it was
cp_stids <- cpts_df$stid[cpts_df$cpts > 0]
options(warn = 2)
for(i in seq_along(cp_stids)){
  asos_hourly_path <- file.path(asos_qc_dir, paste0(cp_stids[i], "_qc.Rds"))
  asos_hourly <- readRDS(asos_hourly_path)
  cpt <- cpts_df[cpts_df$stid == cp_stids[i], ]
  
  if(cpt$cpts == 1){
    sim <- asos_hourly$sped[asos_hourly$date <= cpt$cp1]
    obs <- asos_hourly$sped[asos_hourly$date > cpt$cp1]
    
    temp <- qMapWind(obs, sim)
  } else {
    sim1 <- asos_hourly$sped[asos_hourly$date <= cpt$cp1]
    sim2 <- asos_hourly$sped[asos_hourly$date > cpt$cp1 &
                               asos_hourly$date <= cpt$cp2]
    obs <- asos_hourly$sped[asos_hourly$date > cpt$cp2]
    
    temp1 <- qMapWind(obs, sim1)
    temp2 <- qMapWind(obs, sim2)
  }
}
options(warn = 0)
#------------------------------------------------------------------------------

#-- ECDF plot -----------------------------------------------------------------
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
# 
df1 <- data.frame(data = c(sim, obs),
                  period = as.factor(c(rep("sim", length(sim)),
                                       rep("obs", length(obs)))))
df2 <- data.frame(data = c(q_adj, obs),
                  period = as.factor(c(rep("sim", length(sim)),
                                       rep("obs", length(obs)))))

xmax <- 7 * summary(c(sim, obs))[5]
p1 <- ggplot(df1, aes(data, color = period)) + stat_ecdf(size = 1) + 
  xlab("Wind Speed (MPH)") + ylab("Cumulative Probability") + 
  xlim(c(0, xmax)) + scale_color_discrete(name = "Data", 
                                          labels = c("Sim", "Obs")) + 
  theme(legend.position = "bottom") 

p2 <- ggplot(df2, aes(data, color = period)) + stat_ecdf(size = 1) + 
  xlab("Wind Speed (MPH)") + ylab(element_blank()) + 
  xlim(c(0, xmax))  + ggtitle(" ")

mylegend <- g_legend(p1)

#barfill <- "gold1"
#barlines <- "goldenrod2"
#p3 <- ggplot(asos_hourly_station, aes(x = adj_diff)) +
#  geom_histogram(aes(y = ..count..), binwidth =  0.5,
#                 colour = barlines, fill = barfill) +
#  scale_x_continuous(name = "Adjusted differences",
#                     breaks = seq(0, 10, 5),
#                     limits=c(0, 10)) +
#  scale_y_continuous(name = "Count") +
#  ggtitle(" ")

p4 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"), 
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))

#------------------------------------------------------------------------------

#-- geom_smooth ---------------------------------------------------------------
# having trouble getting geom_smooth to show on these data
asos_path <- file.path(asos_dir, "PAFA_hour.Rds")
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
asos_station_path <- file.path(asos_dir, "PAFA_hour.Rds")
asos_station <- readRDS(asos_station_path)
asos_station$drct <- as.numeric(asos_station$drct)
# openair package (blocky and junk)
save_path <- file.path(figdir, "scrap_PAFA_monthly_windrose.tiff")
tiff(save_path, width = 3000, height = 3000, res = 300)
windRose(asos_station, "sped_adj", "drct", 
         paddle = FALSE, type = "month", 
         breaks = c(0, 4, 8, 12, 16, 20),
         angle = 10)
dev.off()

save_path <- file.path(figdir, "scrap_PAFA_windrose.tiff")
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