

x <- c(5.8,  0.0, 16.1,  5.8,  3.5, 13.8,  6.9,  5.8, 11.5,  9.2, 11.5,
       3.5,  0.0,  8.1,  0.0,  4.6,  5.8,  3.5,  0.0, 10.3,  0.0,  0.0,
       3.5, 6.9, 3.5)
pq <- seq(0, 1, length.out = 20)
qx <- quantile(x, pq)

q_i <- .bincode(x, qx, include.lowest = TRUE)


# my original solution
qi <- .bincode(x, qx, include.lowest = TRUE)

# only if x falls between these values and the value of the next smallest quantile
#   or if x is equal to the value which is equal to the smallest quantile 
dup_qx <- unique(qx[duplicated(qx)])
# get the indices of the first quantiles of sequences of duplicate quantiles
dup_qi <- sort(unique(qi[which(qx[qi + 1] %in% dup_qx)]))
last_dupi <- (dup_qi - 1)[-1]
sort(unique(qi)) last_dupi[length(last_dupi)]
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



# original modified
dup_qx <- unique(qx[duplicated(qx)])
# q_i's of duplicate quantiles
dup_qi <- sort(unique(q_i[which(qx[q_i + 1] %in% dup_qx)]))
upper_dup_qi <- vapply(dup_qx, 
                       function(dup_qx, qx){length(which(qx == dup_qx)) - 1},
                       numeric(1), qx = qx) + dup_qi

# defintely ugly but a way to get both first and 
#   final q_i's of each sequence of duplicated quantiles into
#   lapply
dup_qi <- dup_qi + as.numeric(paste0("0.", upper_dup_qi))
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



# hodgenovice code original
df <- data.frame(lower=sort(unique(q_i)), freq=as.integer(table(q_i)))
df$upper <- c(df$lower[-1] - df$lower[-nrow(df)], 1) + df$lower - 1
df$upper <- df$upper - as.numeric(df$upper > df$lower & qx[df$upper] < qx[df$upper + 1])

df$special_case <- rep(NA, nrow(df))
df$special_case[df$lower < df$upper] <- sapply(df$lower[df$lower < df$upper], function(low) {
  bin <- x[q_i==low]
  if(length(unique(bin)) > 1) {
    return(match(min(bin), bin))} 
  else return(NA)})

recycled <- apply(df, 1, function(x) {
  out <- rep(x["lower"]:x["upper"], length.out=x["freq"])
  
  return(out)
})

q_i_final <- unlist(recycled)[order(order(q_i))]



# hodgenovice code modified

df <- data.frame(lower=sort(unique(q_i)), freq=as.integer(table(q_i)))
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

q_i_final <- unlist(recycled)[order(order(q_i))]


# test data
x <- round(runif(20, 0, 30), 2)
# sample some indices much more frequently than others to generate duplicate
#   quantiles
hf <- sample(1:20, 5)
lf <- setdiff(1:20, hf)
x <- x[c(sample(hf, 8000, replace = TRUE), sample(lf, 2000, replace = TRUE))]
# add some zeros for posterity
x[sample(1:10000, 1000)] <- 0
pq <- seq(0, 1, length.out = 8000)
qx <- quantile(x, pq)

q_i <- .bincode(x, qx, include.lowest = TRUE)
system.time({
df <- data.frame(lower=sort(unique(q_i)), freq=as.integer(table(q_i)))
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

q_i_final <- unlist(recycled)[order(order(q_i))]
})

q_i <- .bincode(x, qx, include.lowest = TRUE)
system.time({
dup_qx <- unique(qx[duplicated(qx)])
# q_i's of duplicate quantiles
dup_qi <- sort(unique(q_i[which(qx[q_i + 1] %in% dup_qx)]))
upper_dup_qi <- vapply(dup_qx, 
                       function(dup_qx, qx){length(which(qx == dup_qx)) - 1},
                       numeric(1), qx = qx) + dup_qi

# defintely ugly but a way to get both first and 
#   final q_i's of each sequence of duplicated quantiles into
#   lapply
dup_qi <- dup_qi + as.numeric(paste0("0.", upper_dup_qi))
# distribute duplicated quantile indices in place of repeated 
qx_recycle <- function(dup_qi, qi){
  end <- as.integer(substring(round(dup_qi - trunc(dup_qi), 3), 3))
  dup_qi <- trunc(dup_qi)
  q_ij <- which(q_i == dup_qi)
  n <- length(q_ij)
  q_is <- rep(0, n)
  suppressWarnings(q_is[rep(TRUE, n)] <- dup_qi:end)
  names(q_is) <- q_ij
  q_is
}
# and replace qi's with these recycled indices
new_qi <- unlist(lapply(dup_qi, qx_recycle, q_i))
q_ij <- as.integer(names(new_qi))
q_i[q_ij] <- new_qi
})



# Actual quantile mapping wind scrap
era <- readRDS(file.path(datadir, "ERA_stations_adj/PAAQ_era_adj.Rds"))
cm3h <- readRDS(file.path(datadir, "CM3_stations_adj/PAAQ_cm3h_adj.Rds"))
cm3f <- readRDS(file.path(datadir, "CM3_stations_adj/PAAQ_cm3f_adj.Rds"))
qn <- nrow(cm3h)
qxn <- seq(0, 1, length.out = qn)
obs <- era$sped_adj
sim <- cm3h$sped 
# modified quantile mapping function
qmap_obj <- qMapWind(obs, sim, ret.deltas = TRUE)

sim_adj <- qmap_obj$sim_adj
sim_adj[sim_adj < 1] <- 0
cm3h$sped_adj <- sim_adj
# future
sim <- cm3f$sped 
sim_adj <- qMapWind(sim = sim, use.deltas = qmap_obj$deltas)
sim_adj[sim_adj < 1] <- 0
cm3f$sped_adj <- sim_adj

qxh1 <- quantile(cm3h$sped, qxn, type = 8)
qxh2 <- quantile(cm3h$sped_adj, qxn, type = 8)
head(qxh2 - qxh1)

qxf1 <- quantile(cm3f$sped, qxn, type= 8)
qxf2 <- quantile(cm3f$sped_adj, qxn, type = 8)
head(qxf2 - qxf1)

# wondering if you are guarenteed equivalent quantile differences by applying same deltas
set.seed(123)
x <- round(runif(1000, 0, 20), 2)
y <- round(runif(5000, 2, 25), 2)
qd <- round(runif(300), 2)
qx1 <- quantile(x, seq(0, 1, length.out = 300))
qy1 <- quantile(y, seq(0, 1, length.out = 300))
anyDuplicated(qx1)
anyDuplicated(qy1)
qix <- .bincode(x, qx1, include.lowest = TRUE)
qiy <- .bincode(y, qy1, include.lowest = TRUE)
x2 <- x - qd[qix]
y2 <- y - qd[qiy]
qx2 <- quantile(x2, seq(0, 1, length.out = 300))
qy2 <- quantile(y2, seq(0, 1, length.out = 300))
head(qx2 - qx1)
head(qy2 - qy1)
