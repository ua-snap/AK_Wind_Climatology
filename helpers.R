# Script summary
#
# Helper functions that I find myself coding repeatedly
#
# qMapWind
#   custom quantile mapping function for wind speeds
#
# ggECDF_compare
#   use ggplot2 to plot ECDFs before and after bias correction



#-- qMapWind ------------------------------------------------------------------
# Custom quantile mapping function
#   Values adjusted below zero set to zero
qMapWind <- function(obs, sim){
  require(ggplot2)
  qn <- min(length(obs), length(sim))
  q_obs <- quantile(obs, seq(0, 1, length.out = qn), type = 8)
  q_sim <- quantile(sim, seq(0, 1, length.out = qn), type = 8)
  q_diff <- q_sim - q_obs
  # assign quantiles to observations
  # round() used to alleviate troubles with fp comparison
  qs <- unique(round(q_sim, 10))
  # might be able to speed things up here
  # can try cut function with unique breaks
  # Then for duplicated quantiles (e.g. zero), need to randomly assign 
  #   indices 
  q_t <- table(round(q_sim, 10))
  q_ids <- c()
  
  # loop through unique quantiles
  for(i in 1:length(qs)){
    if(i == 1){
      i_s <- which(sim == qs[i])
      q_mem <- 0
    }else{
      i_s <- which(round(sim, 10) <= qs[i] & 
                     round(sim, 10) > qs[i - 1])
    }
    # occasionally, quantiles are determined where no data will fall
    #   just skip these
    n_i <- length(i_s)
    if(n_i == 0){next}
    dup_q <- q_t[i]
    
    # randomly apply quantile id's to the indices of duplicated quantiles
    if(n_i > 1){
      names(i_s) <- (as.numeric(cut_number(i_s, dup_q)) + q_mem)[sample(n_i)]
    } else {
      names(i_s) <- 1 + q_mem
    }
    q_ids <- c(q_ids, i_s)
    q_mem <- dup_q + q_mem
  }
  q_ids <- q_ids[order(q_ids)]
  q_adj <- sim - as.numeric(q_diff[as.numeric(names(q_ids))])
  # q_adj <- if_else(q_adj < 0, 0, q_adj)
  return(q_adj)
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
 
  grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                           p2 + theme(legend.position="none"), 
                           nrow=1),
               mylegend, nrow=2,heights=c(10, 1))
}

#------------------------------------------------------------------------------
