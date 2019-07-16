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
qMapWind <- function(obs, sim, qn = 0.001){
  require(ggplot2)
  qn <- round(1/qn)
  qy <- quantile(obs, seq(0, 1, length.out = qn), type = 8)
  qx <- quantile(sim, seq(0, 1, length.out = qn), type = 8)
  q_deltas <- qx - qy
  
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
  
  # return adjusted
  sim - as.numeric(q_deltas)[qi]
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
