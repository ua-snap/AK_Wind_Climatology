

x <- c(5.8,  0.0, 16.1,  5.8,  3.5, 13.8,  6.9,  5.8, 11.5,  9.2, 11.5,
       3.5,  0.0,  8.1,  0.0,  4.6,  5.8,  3.5,  0.0, 10.3,  0.0,  0.0,
       3.5, 6.9, 3.5)
pq <- seq(0, 1, length.out = 20)
qx <- quantile(x, pq)

q_i <- .bincode(x, qx, include.lowest = TRUE)


# my original solution
qi <- .bincode(x, qx, include.lowest = TRUE)

dup_qx <- unique(qx[duplicated(qx)])
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
un_qi <- sort(unique(q_i[which(!qx[q_i + 1] %in% dup_qx)]))


# corresponding q_i's we can recycle up to
upper_dup_qi <- c((dup_qi - 1)[-1], length(qx))
qi[]

# defintely ugly but a way to get both first and 
#   final q_i's of each sequence of duplicated quantiles into
#   lapply
dup_qi <- dup_qi + as.numeric(paste0("0.", last_dupi))




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
dup_qi <- data.frame(lower = unique(qx[duplicated(qx)]),
                     upper = sort(unique(q_i[which(qx[q_i + 1] %in% dup_qx)])),
                     freq = as.integer(table(q_i)))





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