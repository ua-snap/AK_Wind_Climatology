# Script summary
#
# Helper functions that I find myself coding repeatedly
#
# t_test_stid
#   compute t-tests for stations by stid using monthly averaged data
#
# uv2wdws
#   convert wind velocity components to direction (deg) and speed (mph)
#
# qMapWind
#   custom quantile mapping function for wind speeds
#
# ggECDF_compare
#   use ggplot2 to plot ECDFs before and after bias correction



#-- t_test_stid ---------------------------------------------------------------
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

#------------------------------------------------------------------------------

#-- uv2wdws -------------------------------------------------------------------
# function to apply to convert m/s components to mph and directions
# borrowed from github/environmentalinformatics-marburg/Rsenal
# modified to return mph
uv2wdws <- function(u,v) {
  
  degrees <- function(radians) 180 * radians / pi
  
  mathdegs <- degrees(atan2(v, u))
  wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
  wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
  ws <- sqrt(u^2 + v^2) * 2.23694
  
  return(cbind(wd, ws))
  
}

#------------------------------------------------------------------------------

#-- qMapWind ------------------------------------------------------------------
# Custom quantile mapping function
qMapWind <- function(obs = NULL, sim, 
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

#------------------------------------------------------------------------------

#-- ggECDF_compare ------------------------------------------------------------
# Arguments
#   sim: "simulated" or "biased" data
#   obs: data from unbiased source
#   sim_adj: sim data that has been adjusted

ggECDF_compare <- function(obs, sim, sim_adj, p_title = " "){
  require(gridExtra)
  require(ggplot2)
  
  df1 <- data.frame(sped = c(sim, obs),
                   quality = c(rep("1", length(sim)),
                               rep("2", length(obs))))
  
  df2 <- data.frame(sped = c(sim_adj, obs),
                    quality = c(rep("1", length(sim_adj)),
                                rep("2", length(obs))))
  
  # extract legend, code borrowed from SO (for sharing legend between plots)
  # https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  # original data
  xmax <- quantile(obs, probs = seq(0, 1, 1/100))[100] + 5
  p1 <- ggplot(df1, aes(sped, color = quality)) + 
    stat_ecdf(size = 1) + 
    xlab("Wind Speed (MPH)") + ylab("Cumulative Probability") + 
    xlim(c(0, xmax)) + scale_color_discrete(name = "Data", 
                                            labels = c("Sim", "Obs")) + 
    theme(legend.position = "bottom") +
    ggtitle(p_title)
  
  # corrected data
  p2 <- ggplot(df2, aes(sped, color = quality)) + 
    stat_ecdf(size = 1) + 
    xlab("Wind Speed (MPH)") + ylab(element_blank()) + 
    xlim(c(0, xmax))  + ggtitle(" ")
  
  # legend code adapted from:
  # https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  tmp <- ggplot_gtable(ggplot_build(p1))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  mylegend <- tmp$grobs[[leg]]
 
  p <- arrangeGrob(arrangeGrob(p1 + theme(legend.position = "none"),
                           p2 + theme(legend.position = "none"), 
                           nrow = 1),
               mylegend, nrow=2, heights = c(10, 1))
  return(p)
}

#------------------------------------------------------------------------------
