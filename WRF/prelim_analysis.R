# Script Summary
#
# Compare model output means between periods by site and month
#
# 



#-- Setup ---------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)

workdir <- getwd()
datadir <- file.path(workdir, "data")
figdir <- file.path(workdir, "figures")

#------------------------------------------------------------------------------

#-- CM3 Comparisons -----------------------------------------------------------
# function to run t tests by month between climatology periods
t_test_stid <- function(stidf, monthly_df){
  temp <- lapply(month.abb, function(mo_abb, df){
    x <- df %>% filter(mo == mo_abb & clim == "1980-2015") %>%
      ungroup() %>% select(avg_sped_adj) %>% unlist() %>% unname()
    y <- df %>% filter(mo == mo_abb & clim == "2065-2100") %>%
      ungroup() %>% select(avg_sped_adj) %>% unlist() %>% unname()
    mod <- t.test(x, y)
    ret.vec <- c(round(mod$p.value, 3), mod$estimate)
    names(ret.vec)[1] <- "p-val"
    return(ret.vec)
  }, df = monthly_df %>% filter(stid == stidf))
  temp <- data.frame(matrix(unlist(temp), nrow = 12, byrow = TRUE))
  temp <- temp %>%
    rename(p_val = X1, mean_x = X2, mean_y = X3) %>%
    mutate(sig = if_else(p_val <= 0.05, "Signif", "Not Signif"),
           stid = stidf)
  temp$mo <- factor(month.abb, levels = month.abb)
  levels(temp$sig) <- c("Signif", "Not Signif")
  temp
}

# labels will apply to both models
wind_event_stids <- c("PAOM", "PABR", "PASI", "PAJN", 
                      "PABA", "PANC", "PADQ", "PASN",
                      "PAFA")
stid_names <- list(
  "PAOM" = "Nome",
  "PABR" = "Barrow",
  "PASI" = "Sitka",
  "PAJN" = "Juneau",
  "PABA" = "Kaktovik",
  "PANC" = "Anchorage",
  "PADQ" = "Kodiak",
  "PASN" = "Saint Paul",
  "PAFA" = "Fairbanks"
)
stid_labeller <- function(variable,value){
  return(stid_names[value])
}

# CM3 
cm3h_monthly <- readRDS(file.path(datadir, "cm3h_monthly.Rds"))
cm3f_monthly <- readRDS(file.path(datadir, "cm3f_monthly.Rds"))

cm3_monthly <- bind_rows(cm3h_monthly, cm3f_monthly)
cm3h_monthly <- cm3_monthly %>% filter(ym_date < "2015-01-01")
cm3f_monthly <- cm3_monthly %>% filter(ym_date >= "2065-01-01" & 
                                         ym_date < "2100-01-01")
cm3h_monthly$clim <- factor("1980-2015", 
                            levels = c("1980-2015", "2065-2100"))
cm3f_monthly$clim <- factor("2065-2100", 
                            levels = c("1980-2015", "2065-2100"))
cm3_monthly <- bind_rows(cm3h_monthly, cm3f_monthly) %>%
  mutate(mo = factor(month.abb[as.numeric(format(ym_date, "%m"))],
                     levels = month.abb))

cm3_ttest <- lapply(wind_event_stids, t_test_stid, cm3_monthly) %>%
  bind_rows()

cm3_results <- cm3_monthly %>% 
  filter(stid %in% wind_event_stids) %>%
  full_join(cm3_ttest, by = c("stid", "mo"))

p <- ggplot(cm3_results, 
            aes(x = mo, y = avg_sped_adj, fill = clim, col = sig)) + 
  geom_boxplot() +
  scale_color_manual(values = c("black", "#E69F00")) + 
  facet_wrap(~stid, labeller = stid_labeller) + 
  labs(fill = "Climatological\nPeriod",
       col = "Significance\nat p = 0.05") + 
  xlab("Month") + ylab("Avg Wind Speed (mph)") + 
  ggtitle("GFDL CM3 Average Wind Speeds",
          subtitle = "Boxplots by Month and Climatological Period") +
  theme_classic() + 
  theme(strip.text.x = element_text(size = 12))
plot_path <- file.path(figdir, "climatology_comparison",
                       "cm3_boxplots_wind_event_subset.png")
ggsave(plot_path, p, device = "png", scale = 1.5)

# CCSM4
ccsm4h_monthly <- readRDS(file.path(datadir, "ccsm4h_monthly.Rds"))
ccsm4f_monthly <- readRDS(file.path(datadir, "ccsm4f_monthly.Rds"))

ccsm4_monthly <- bind_rows(ccsm4h_monthly, ccsm4f_monthly)
ccsm4h_monthly <- ccsm4_monthly %>% filter(ym_date < "2015-01-01")
ccsm4f_monthly <- ccsm4_monthly %>% filter(ym_date >= "2065-01-01" & 
                                         ym_date < "2100-01-01")
ccsm4h_monthly$clim <- factor("1980-2015", 
                            levels = c("1980-2015", "2065-2100"))
ccsm4f_monthly$clim <- factor("2065-2100", 
                            levels = c("1980-2015", "2065-2100"))
ccsm4_monthly <- bind_rows(ccsm4h_monthly, ccsm4f_monthly) %>%
  mutate(mo = factor(month.abb[as.numeric(format(ym_date, "%m"))],
                     levels = month.abb))

ccsm4_ttest <- lapply(wind_event_stids, t_test_stid, ccsm4_monthly) %>%
  bind_rows()

ccsm4_results <- ccsm4_monthly %>% 
  filter(stid %in% wind_event_stids) %>%
  full_join(ccsm4_ttest, by = c("stid", "mo"))

p <- ggplot(ccsm4_results, 
            aes(x = mo, y = avg_sped_adj, fill = clim, col = sig)) + 
  geom_boxplot() +
  scale_color_manual(values = c("black", "#E69F00")) + 
  facet_wrap(~stid, labeller = stid_labeller) + 
  labs(fill = "Climatological\nPeriod",
       col = "Significance\nat p = 0.05") + 
  xlab("Month") + ylab("Avg Wind Speed (mph)") + 
  ggtitle("NCAR CCSM4 Average Wind Speeds",
          subtitle = "Boxplots by Month and Climatological Period") +
  theme_classic() + 
  theme(strip.text.x = element_text(size = 12))
plot_path <- file.path(figdir, "climatology_comparison",
                       "ccsm4_boxplots_wind_event_subset.png")
ggsave(plot_path, p, device = "png", scale = 1.5)

#------------------------------------------------------------------------------
