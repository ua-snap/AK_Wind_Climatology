#-- Utilities.R ---------------------------------------------------------------
# This is code created by David Carslaw - I am just using some of these
#   so I can use a custom windrose function

## TODO: Add comment
#
## Author: David Carslaw
## useful utility functions
## with some updates and modification by Karl Ropkins

startYear <- function(dat) as.numeric(format(min(dat[order(dat)]), "%Y"))
endYear <- function(dat) as.numeric(format(max(dat[order(dat)]), "%Y"))
startMonth <- function(dat) as.numeric(format(min(dat[order(dat)]), "%m"))
endMonth <- function(dat) as.numeric(format(max(dat[order(dat)]), "%m"))

## these are pre-defined type that need a field "date"; used by cutData
dateTypes <- c(
  "year", "hour", "month", "season", "weekday", "weekend",
  "monthyear", "gmtbst", "bstgmt", "dst", "daylight",
  "seasonyear", "yearseason"
)

## sets up how openair graphics look by default and resets on exit

setGraphics <- function(fontsize = 5) {
  current.strip <- trellis.par.get("strip.background")
  trellis.par.set(fontsize = list(text = fontsize))
  
  ## reset graphic parameters
  font.orig <- trellis.par.get("fontsize")$text
  on.exit(trellis.par.set(
    
    fontsize = list(text = font.orig)
  ))
}


# function to test of a suggested package is available and warn if not
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    library(package, character.only = TRUE)
    return(invisible())
  }
  
  stop(
    "Package `", package, "` required for `", fun, "`.\n",
    "Please install and try again.",
    call. = FALSE
  )
}


## function to find averaging period of data, returns "xx sec"
## for use in filling in gaps in time series data
## it finds the table of values of time gaps and picks the biggest
## can't think of better way unless user specifies what the time interval is meant to be

find.time.interval <- function(dates) {
  
  ## could have several sites, dates may be unordered
  ## find the most common time gap in all the data
  dates <- unique(dates) ## make sure they are unique
  
  # work out the most common time gap of unique, ordered dates
  id <- which.max(table(diff(as.numeric(unique(dates[order(dates)])))))
  seconds <- as.numeric(names(id))
  
  if ("POSIXt" %in% class(dates)) seconds <- paste(seconds, "sec")
  
  if (class(dates)[1] == "Date") {
    seconds <- seconds * 3600 * 24
    seconds <- paste(seconds, "sec")
  }
  
  seconds
}



date.pad2 <- function(mydata, type = NULL, interval = "month") {
  
  # assume by the time we get here the data have been split into types
  # This means we just need to pad out the missing types based on first
  # line.
  
  start.date <- min(mydata$date, na.rm = TRUE)
  end.date <- max(mydata$date, na.rm = TRUE)
  
  all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
  mydata <- mydata %>% full_join(all.dates, by = "date")
  
  # add in missing types if gaps are made
  if (!is.null(type)) {
    mydata[type] <- mydata[1, type]
  }
  
  # make sure order is correct
  mydata <- arrange(mydata, date)
  
  return(mydata)
}


# Function to pad out missing time data
# assumes data have already been split by type, so just take first
# tries to work out time interval of input based on most common gap
# can print assumed gap to screen

date.pad <- function(mydata, type = NULL, print.int = FALSE) {
  
  # if one line, just return
  if (nrow(mydata) < 2) return(mydata)
  
  ## time zone of data
  TZ <- attr(mydata$date, "tzone")
  if (is.null(TZ)) TZ <- "GMT" ## as it is on Windows for BST
  
  ## function to fill missing data gaps
  ## assume no missing data to begin with
  
  
  ## pad out missing data
  start.date <- min(mydata$date, na.rm = TRUE)
  end.date <- max(mydata$date, na.rm = TRUE)
  
  ## interval in seconds
  interval <- find.time.interval(mydata$date)
  
  ## equivalent number of days, used to refine interval for month/year
  days <- as.numeric(strsplit(interval, split = " ")[[1]][1]) /
    24 / 3600
  
  ## find time interval of data
  if (class(mydata$date)[1] == "Date") {
    interval <- paste(days, "day")
  } else {
    ## this will be in seconds
    interval <- find.time.interval(mydata$date)
  }
  
  ## better interval, most common interval in a year
  if (days == 31) interval <- "month"
  if (days %in% c(365, 366)) interval <- "year"
  
  ## only pad if there are missing data
  if (length(unique(diff(mydata$date))) != 1L) {
    all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
    mydata <- mydata %>% full_join(all.dates, by = "date")
    
    # add missing types - if type is present
    if (!is.null(type)) {
      mydata[type] <- mydata[1, type]
    }
  }
  
  ## return the same TZ that we started with
  attr(mydata$date, "tzone") <- TZ
  
  if (print.int) print(paste0("Input data time interval assumed is ", interval))
  
  # make sure date-sorted
  mydata <- arrange(mydata, date)
  
  mydata
}


## unitility function to convert decimal date to POSIXct
decimalDate <- function(x, date = "date") {
  thedata <- x
  x <- x[, date]
  x.year <- floor(x)
  ## fraction of the year
  x.frac <- x - x.year
  ## number of seconds in each year
  x.sec.yr <- unclass(ISOdate(x.year + 1, 1, 1, 0, 0, 0)) - unclass(ISOdate(x.year, 1, 1, 0, 0, 0))
  ## now get the actual time
  x.actual <- ISOdate(x.year, 1, 1, 0, 0, 0) + x.frac * x.sec.yr
  x.actual <- as.POSIXct(trunc(x.actual, "hours"), "GMT")
  thedata$date <- x.actual
  thedata
}




##' Calculate rollingMean values
##'
##' Calculate rollingMean values taking account of data capture thresholds
##'
##' This is a utility function mostly designed to calculate rolling
##' mean statistics relevant to some pollutant limits e.g. 8 hour
##' rolling means for ozone and 24 hour rolling means for
##' PM10. However, the function has a more general use in helping to
##' display rolling mean values in flexible ways e.g. with the rolling
##' window width left, right or centre aligned.
##'
##' The function will try and fill in missing time gaps to get a full
##' time sequence but return a data frame with the same number of rows
##' supplied.
##'
##' @param mydata A data frame containing a \code{date}
##' field. \code{mydata} must contain a \code{date} field in
##' \code{Date} or \code{POSIXct} format. The input time series must
##' be regular e.g. hourly, daily.
##' @param pollutant The name of a pollutant e.g. \code{pollutant = "o3"}.
##' @param width The averaging period (rolling window width) to use
##' e.g. \code{width = 8} will generate 8-hour rolling mean values
##' when hourly data are analysed.
##' @param new.name The name given to the new rollingMean variable. If
##' not supplied it will create a name based on the name of the
##' pollutant and the averaging period used.
##' @param data.thresh The data capture threshold in %. No values are
##' calculated if data capture over the period of interest is less
##' than this value. For example, with \code{width = 8} and
##' \code{data.thresh = 75} at least 6 hours are required to calculate
##' the mean, else \code{NA} is returned.
##' @param align specifyies how the moving window should be
##' aligned. \code{"right"} means that the previous \code{hours}
##' (including the current) are averaged. This seems to be the default
##' for UK air quality rolling mean statistics. \code{"left"} means
##' that the forward \code{hours} are averaged, and \code{"centre"} or
##' \code{"center"}, which is the default.
##' @param ... other arguments, currently unused.
##' @export
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## rolling 8-hour mean for ozone
##' mydata <- rollingMean(mydata, pollutant = "o3", width = 8, new.name =
##' "rollingo3", data.thresh = 75, align = "right")
##'
##'
rollingMean <- function(mydata, pollutant = "o3", width = 8, new.name = "rolling",
                        data.thresh = 75, align = "centre", ...) {
  ## function to calculate rolling means
  ## uses C++ code
  
  ## get rid of R check annoyances
  site <- NULL
  if (!align %in% c("left", "right", "centre", "center")) stop("align should be one of 'right', 'left', 'centre' or 'center'.")
  
  
  if (missing(new.name)) new.name <- paste("rolling", width, pollutant, sep = "")
  if (data.thresh < 0 | data.thresh > 100) stop("Data threshold must be between 0 and 100.")
  
  calc.rolling <- function(mydata, ...) {
    
    ## data needs to be numeric
    if (!is.numeric(mydata[[pollutant]])) {
      warning("Data are not numeric.")
      return(mydata)
    }
    
    ## need to know whether dates added
    dates <- mydata$date
    
    ## pad missing hours
    mydata <- date.pad(mydata)
    
    ## make sure function is not called with window width longer than data
    if (width > nrow(mydata)) return(mydata)
    
    mydata[[new.name]] <- .Call(
      "rollMean", mydata[[pollutant]],
      width, data.thresh, align,
      PACKAGE = "openair"
    )
    
    if (length(dates) != nrow(mydata)) {
      ## return what was put in
      ## avoids adding missing data e.g. for factors
      mydata <- mydata[mydata$date %in% dates, ]
    }
    
    mydata
  }
  
  ## split if several sites
  if ("site" %in% names(mydata)) { ## split by site
    
    mydata <- group_by(mydata, site) %>%
      do(calc.rolling(., ...))
    
    mydata
  } else {
    mydata <- calc.rolling(mydata, ...)
    mydata
  }
}




convert.date <- function(mydata, format = "%d/%m/%Y %H:%M") {
  mydata$date <- as.POSIXct(strptime(mydata$date, format = format), "GMT")
  mydata
}



## splits data frame into date chunks. Allows users to supply simple dates and labels
## useful for type = "site", interventions



##' Divide up a data frame by time
##'
##' Utility function to prepare input data for use in openair functions
##'
##' This function partitions a data frame up into different time segments. It
##' produces a new column called controlled by \code{name} that can be used in many
##' \code{openair} functions. Note that there must be one more label than there
##' are dates. See examples below and in full \code{openair} documentation.
##'
##' @param mydata A data frame containing a \code{date} field in hourly or high
##'   resolution format.
##' @param dates A date or dates to split data by.
##' @param labels Labels for each time partition.
##' @param name The name to give the new column to identify the periods split
##' @export
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## split data up into "before" and "after"
##' mydata <- splitByDate(mydata, dates = "1/04/2000",
##' labels = c("before", "after"))
##'
##' ## split data into 3 partitions:
##' mydata <- splitByDate(mydata, dates = c("1/1/2000", "1/3/2003"),
##' labels = c("before", "during", "after"))
##'
##'
splitByDate <- function(mydata, dates = "1/1/2003", labels = c("before", "after"), name = "split.by") {
  ## if date in format dd/mm/yyyy hh:mm (basic check)
  if (missing(mydata)) stop("No data frame was supplied!")
  
  mydata <- checkPrep(mydata, names(mydata), "default", remove.calm = FALSE)
  ## check there are sufficent labels for number of dates
  if (length(dates) != length(labels) - 1) {
    stop("There is a mis-match between dates and labels. There should be
         one more label than date")
  }
  
  if (length(grep("/", as.character(dates))) > 0) {
    if (class(mydata$date)[1] == "Date") {
      dates <- as_date(as.POSIXct(strptime(dates, "%d/%m/%Y"), "GMT"))
    } else {
      dates <- as.POSIXct(strptime(dates, "%d/%m/%Y"), "GMT")
    }
  } else { ## asume format yyyy-mm-dd
    
    if (class(mydata$date)[1] == "Date") {
      dates <- as_date(dates)
    } else {
      dates <- as.POSIXct(dates, "GMT")
    }
  }
  
  
  mydata[, name] <- cut(
    as.numeric(mydata$date),
    breaks = c(
      0, as.numeric(dates),
      max(mydata$date)
    ), labels = labels,
    ordered_result = TRUE
  )
  mydata
  }

## function to make it easy to use d/m/y format for subsetting by date


##' Subset a data frame based on date
##'
##' Utility function to make it easier to select periods from a data frame
##' before sending to a function
##'
##' This function makes it much easier to select periods of interest from a data
##' frame based on dates in a British format. Selecting date/times in R format
##' can be intimidating for new users. This function can be used to select quite
##' complex dates simply - see examples below.
##'
##' Dates are assumed to be inclusive, so \code{start = "1/1/1999"} means that
##' times are selected from hour zero. Similarly, \code{end = "31/12/1999"} will
##' include all hours of the 31st December. \code{start} and \code{end} can also
##' be in standard R format as a string i.e. "YYYY-mm-dd", so \code{start =
##' "1999-01-01"} is fine.
##'
##' All options are applied in turn making it possible to select quite complex
##' dates
##'
##' @param mydata A data frame containing a \code{date} field in hourly or high
##'   resolution format.
##' @param start A start date string in the form d/m/yyyy e.g. \dQuote{1/2/1999}
##'   or in \sQuote{R} format i.e. \dQuote{YYYY-mm-dd}, \dQuote{1999-02-01}
##' @param end See \code{start} for format.
##' @param year A year or years to select e.g. \code{year = 1998:2004} to select
##'   1998-2004 inclusive or \code{year = c(1998, 2004)} to select 1998 and
##'   2004.
##' @param month A month or months to select. Can either be numeric e.g.
##'   \code{month = 1:6} to select months 1-6 (January to June), or by name e.g.
##'   \code{month = c("January", "December")}. Names can be abbreviated to 3
##'   letters and be in lower or upper case.
##' @param day A day name or or days to select. \code{day} can be numeric (1 to
##'   31) or character. For example \code{day = c("Monday", "Wednesday")} or
##'   \code{day = 1:10} (to select the 1st to 10th of each month). Names can be
##'   abbreviated to 3 letters and be in lower or upper case. Also accepts
##'   \dQuote{weekday} (Monday - Friday) and \dQuote{weekend} for convenience.
##' @param hour An hour or hours to select from 0-23 e.g. \code{hour = 0:12} to
##'   select hours 0 to 12 inclusive.
##' @importFrom lubridate dst year month hour wday force_tz day as_date dmy ymd_hm
##'   round_date parse_date_time floor_date ceiling_date
##' @export
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## select all of 1999
##' data.1999 <- selectByDate(mydata, start = "1/1/1999", end = "31/12/1999")
##' head(data.1999)
##' tail(data.1999)
##'
##' # or...
##' data.1999 <- selectByDate(mydata, start = "1999-01-01", end = "1999-12-31")
##'
##' # easier way
##' data.1999 <- selectByDate(mydata, year = 1999)
##'
##'
##' # more complex use: select weekdays between the hours of 7 am to 7 pm
##' sub.data <- selectByDate(mydata, day = "weekday", hour = 7:19)
##'
##' # select weekends between the hours of 7 am to 7 pm in winter (Dec, Jan, Feb)
##' sub.data <- selectByDate(mydata, day = "weekend", hour = 7:19, month =
##' c("dec", "jan", "feb"))
##' 
selectByDate <- function(mydata, start = "1/1/2008",
                         end = "31/12/2008", year = 2008,
                         month = 1, day = "weekday", hour = 1) {
  ## extract variables of interest
  vars <- names(mydata)
  
  ## check data - mostly date format
  mydata <- checkPrep(
    mydata, vars, "default",
    remove.calm = FALSE,
    strip.white = FALSE
  )
  
  weekday.names <- format(ISOdate(2000, 1, 3:9), "%A")
  
  
  if (!missing(start)) {
    
    
    ## assume R date format
    start <- as_date(parse_date_time(start, c("ymd", "dmy")))
    
    mydata <- subset(mydata, as_date(date) >= start)
  }
  
  if (!missing(end)) {
    
    
    ## assume R date format
    end <-as_date(parse_date_time(end, c("ymd", "dmy")))
    
    mydata <- subset(mydata, as_date(date) <= end)
  }
  
  
  if (!missing(year)) {
    mydata <- mydata[which(year(mydata$date) %in% year), ]
  }
  
  
  if (!missing(month)) {
    if (is.numeric(month)) {
      if (any(month < 1 | month > 12)) {
        stop("Month must be between 1 to 12.")
      }
      
      mydata <- mydata[which(month(mydata$date) %in% month), ]
    }
    
    else {
      mydata <- subset(mydata, substr(tolower(format(
        date,
        "%B"
      )), 1, 3) %in% substr(tolower(month), 1, 3))
    }
  }
  if (!missing(hour)) {
    if (any(hour < 0 | hour > 23)) stop("Hour must be between 0 to 23.")
    
    mydata <- mydata[which(hour(mydata$date) %in% hour), ]
  }
  
  if (!missing(day)) {
    days <- day
    
    if (is.numeric(day)) {
      if (any(day < 1 | day > 31)) {
        stop("Day must be between 1 to 31.")
      }
      mydata <- mydata[which(day(mydata$date) %in% day), ]
    } else {
      if (day[1] == "weekday") {
        days <- weekday.names[1:5]
      }
      if (day[1] == "weekend") {
        days <- weekday.names[6:7]
      }
      mydata <- subset(mydata, substr(tolower(format(date, "%A")), 1, 3) %in%
                         substr(tolower(days), 1, 3))
    }
  }
  mydata
}


## from Deepayan Sarkar
panel.smooth.spline <-
  function(x, y,
           w = NULL, df, spar = NULL, cv = FALSE,
           lwd = lwd, lty = plot.line$lty, col, col.line = plot.line$col,
           type, horizontal = FALSE, all.knots = TRUE, ...) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) {
      return()
    }
    if (!missing(col)) {
      if (missing(col.line)) {
        col.line <- col
      }
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal) {
      spline <-
        smooth.spline(
          y[ok], x[ok],
          w = w, df = df, spar = spar, cv = cv
        )
      panel.lines(
        x = spline$y, y = spline$x, col = col.line,
        lty = lty, lwd = lwd, ...
      )
    }
    else {
      spline <-
        smooth.spline(
          x[ok], y[ok],
          w = w, df = df, spar = spar, cv = cv
        )
      panel.lines(
        x = spline$x, y = spline$y, col = col.line,
        lty = lty, lwd = lwd, ...
      )
    }
  }


panel.gam <- function(x, y, form = y ~ x, method = "loess", k = k, Args, ..., simulate = FALSE, n.sim = 200,
                      autocor = FALSE, se = TRUE,
                      level = 0.95, n = 100, col = plot.line$col, col.se = col,
                      lty = plot.line$lty, lwd = plot.line$lwd, alpha = plot.line$alpha,
                      alpha.se = 0.20, border = NA, subscripts, group.number, group.value,
                      type, col.line, col.symbol, fill, pch, cex, font, fontface,
                      fontfamily) {
  
  ## panel function to add a smooth line to a plot
  ## Uses a GAM (mgcv) to fit smooth
  ## Optionally can plot 95% confidence intervals and run bootstrap simulations
  ## to estimate uncertainties. Simple block bootstrap is also available for correlated data
  
  ## get rid of R check annoyances#
  plot.line <- NULL
  
  thedata <- data.frame(x = x, y = y)
  thedata <- na.omit(thedata)
  
  tryCatch({
    if (!simulate) {
      if (is.null(k)) {
        mod <- suppressWarnings(gam(y ~ s(x), select = TRUE, data = thedata, ...))
      } else {
        mod <- suppressWarnings(gam(y ~ s(x, k = k), select = TRUE, data = thedata, ...))
      }
      
      
      lims <- current.panel.limits()
      xrange <- c(max(min(lims$x), min(x, na.rm = TRUE)), min(max(lims$x), max(x, na.rm = TRUE)))
      xseq <- seq(xrange[1], xrange[2], length = n)
      
      ## for uncertainties
      std <- qnorm(level / 2 + 0.5)
      
      pred <- predict(mod, data.frame(x = xseq), se = TRUE)
      
      panel.lines(xseq, pred$fit, col = col, alpha = alpha, lty = lty, lwd = 2)
      
      results <- data.frame(
        date = xseq, pred = pred$fit,
        lower = pred$fit - std * pred$se,
        upper = pred$fit + std * pred$se
      )
      
      if (se) {
        panel.polygon(
          x = c(xseq, rev(xseq)), y = c(pred$fit -
                                          std * pred$se, rev(pred$fit + std * pred$se)),
          col = col.se, alpha = alpha.se, border = border
        )
        pred <- pred$fit
      }
      
    } else { ## simulations required
      
      x <- thedata$x
      y <- thedata$y
      
      sam.size <- length(x)
      
      lims <- current.panel.limits()
      xrange <- c(max(min(lims$x), min(x)), min(max(lims$x), max(x)))
      xseq <- seq(xrange[1], xrange[2], length = sam.size)
      
      boot.pred <- matrix(nrow = sam.size, ncol = n.sim)
      
      print("Taking bootstrap samples. Please wait...")
      
      ## set up bootstrap
      block.length <- 1
      
      if (autocor) block.length <- round(sam.size ^ (1 / 3))
      index <- samp.boot.block(sam.size, n.sim, block.length)
      
      ## predict first
      if (is.null(k)) {
        mod <- gam(y ~ s(x), data = thedata, ...)
      } else {
        mod <- gam(y ~ s(x, k = k), data = thedata, ...)
      }
      
      residuals <- residuals(mod) ## residuals of the model
      
      pred.input <- predict(mod, thedata)
      
      for (i in 1:n.sim) {
        ## make new data
        new.data <- data.frame(x = xseq, y = pred.input + residuals[index[, i]])
        
        mod <- gam(y ~ s(x), data = new.data, ...)
        
        pred <- predict(mod, new.data)
        
        boot.pred[, i] <- as.vector(pred)
      }
      
      ## calculate percentiles
      percentiles <- apply(boot.pred, 1, function(x) quantile(x, probs = c(0.025, 0.975)))
      
      results <- as.data.frame(cbind(
        pred = rowMeans(boot.pred),
        lower = percentiles[1, ], upper = percentiles[2, ]
      ))
      
      if (se) {
        panel.polygon(
          x = c(xseq, rev(xseq)), y = c(results$lower, rev(results$upper)),
          col = col.se, alpha = alpha.se, border = border
        )
      }
      
      panel.lines(xseq, pred.input, col = col, alpha = alpha, lty = lty, lwd = 2)
    }
    results
  }, error = function(x) return)
}


## version of GAM fitting not for plotting - need to rationalise both...
fitGam <- function(thedata, x = "date", y = "conc", form = y ~ x, k = k,
                   Args, ..., simulate = FALSE, n.sim = 200, autocor = FALSE, se = TRUE,
                   level = 0.95, n = 100) {
  
  ## panel function to add a smooth line to a plot
  ## Uses a GAM (mgcv) to fit smooth
  ## Optionally can plot 95% confidence intervals and run bootstrap simulations
  ## to estimate uncertainties. Simple block bootstrap is also available for correlated data
  
  data.orig <- thedata ## return this if all else fails
  
  id <- which(names(thedata) == x)
  names(thedata)[id] <- "x"
  id <- which(names(thedata) == y)
  names(thedata)[id] <- "y"
  
  # can only fit numeric, so convert back after fitting
  class_x <- class(thedata$x)
  
  thedata$x <- as.numeric(thedata$x)
  
  tryCatch({
    if (!simulate) {
      if (is.null(k)) {
        mod <- suppressWarnings(gam(y ~ s(x), select = TRUE, data = thedata, ...))
      } else {
        mod <- suppressWarnings(gam(y ~ s(x, k = k), select = TRUE, data = thedata, ...))
      }
      
      xseq <- seq(min(thedata$x, na.rm = TRUE), max(thedata$x, na.rm = TRUE), length = n)
      
      ## for uncertainties
      std <- qnorm(level / 2 + 0.5)
      
      pred <- predict(mod, data.frame(x = xseq), se = se)
      
      
      results <- data.frame(
        date = xseq, pred = pred$fit,
        lower = pred$fit - std * pred$se,
        upper = pred$fit + std * pred$se
      )
    } else { ## simulations required
      
      sam.size <- nrow(thedata)
      
      xseq <- seq(min(thedata$x, na.rm = TRUE), max(thedata$x, na.rm = TRUE), length = n)
      
      boot.pred <- matrix(nrow = sam.size, ncol = n.sim)
      
      print("Taking bootstrap samples. Please wait...")
      
      ## set up bootstrap
      block.length <- 1
      
      if (autocor) block.length <- round(sam.size ^ (1 / 3))
      index <- samp.boot.block(sam.size, n.sim, block.length)
      
      ## predict first
      if (is.null(k)) {
        mod <- gam(y ~ s(x), data = thedata, ...)
      } else {
        mod <- gam(y ~ s(x, k = k), data = thedata, ...)
      }
      
      residuals <- residuals(mod) ## residuals of the model
      
      pred.input <- predict(mod, thedata)
      
      for (i in 1:n.sim) {
        ## make new data
        new.data <- data.frame(x = xseq, y = pred.input + residuals[index[, i]])
        
        mod <- gam(y ~ s(x), data = new.data, ...)
        
        pred <- predict(mod, new.data)
        
        boot.pred[, i] <- as.vector(pred)
      }
      
      ## calculate percentiles
      percentiles <- apply(boot.pred, 1, function(x) quantile(x, probs = c(0.025, 0.975)))
      
      results <- as.data.frame(cbind(
        pred = rowMeans(boot.pred),
        lower = percentiles[1, ], upper = percentiles[2, ]
      ))
    }
    
    # convert class back to orginal
    class(results[[x]]) <- class_x
    return(results)
  }, error = function(x) {
    data.orig
  })
}





## error in mean from Hmisc

errorInMean <- function(x, mult = qt((1 + conf.int) / 2, n - 1), conf.int = 0.95,
                        na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  if (n < 2) {
    return(c(Mean = mean(x), Lower = NA, Upper = NA))
  }
  xbar <- sum(x) / n
  se <- sqrt(sum((x - xbar) ^ 2) / n / (n - 1))
  c(Mean = xbar, Lower = xbar - mult * se, Upper = xbar + mult *
      se)
}

## bootsrap confidence intervals in the mean from Hmisc
bootMean <- function(x, conf.int = 0.95, B = 1000, ...) {
  x <- x[!is.na(x)] # remove missings
  n <- length(x)
  xbar <- mean(x)
  if (n < 2) {
    return(c(Mean = xbar, Lower = NA, Upper = NA))
  }
  z <- unlist(lapply(1:B, function(i, x, N)
    sum(x[(sample.int(N, N, TRUE, NULL))]), x = x, N = n)) / n
  quant <- quantile(z, c((1 - conf.int) / 2, (1 + conf.int) / 2))
  names(quant) <- NULL
  res <- c(Mean = xbar, Lower = quant[1], Upper = quant[2])
  
  res
}


#' Bootsrap confidence intervals in the mean
#'
#' A utility function to calculation the uncertainty intervals in the mean of a
#' vector. The function removes any missing data before the calculation.
#'
#' @param x A vector from which the mean and bootstrap confidence intervals in
#'   the mean are to be calculated
#' @param conf.int The confidence interval; default = 0.95.
#' @param B The number of bootstrap simulations
#'
#' @return Returns a data frame with the mean, lower uncertainty, upper
#'   uncertainty and number of values used in the calculation
#' @export
#'
#' @examples
#' test <- rnorm(20, mean = 10)
#' bootMeanDF(test)
bootMeanDF <- function(x, conf.int = 0.95, B = 1000) {
  if (!is.vector(x)) {
    stop("x should be a vector.")
  }
  
  res <- bootMean(x = x, conf.int = conf.int, B = B)
  res <- data.frame(mean = res[1], min = res[2], max = res[3], n = length(na.omit(x)))
  res <- return(res)
}


bootMeanDiff <- function(mydata, x = "x", y = "y", conf.int = 0.95, B = 1000) {
  
  ## calculates bootstrap mean differences
  ## assumes y - x
  x.name <- x
  y.name <- y
  x <- na.omit(mydata[[x]])
  y <- na.omit(mydata[[y]])
  Mean <- mean(y) - mean(x)
  
  if (nrow(mydata) < 2) {
    res1 <- data.frame(variable = x.name, Mean = mean(x), Lower = NA, Upper = NA,
                       stringsAsFactors = FALSE)
    
    res2 <- data.frame(variable = y.name, Mean = mean(y), Lower = NA, Upper = NA,
                       stringsAsFactors = FALSE)
    
    res <- data.frame(variable = paste(y.name, "-", x.name), Mean = Mean, 
                      Lower = NA, Upper = NA,
                      stringsAsFactors = FALSE)
    
    res <- bind_rows(res1, res2, res)
    res$variable <- factor(res$variable)
    return(res)
  }
  
  x <- bootMean(x, B = B)
  y <- bootMean(y, B = B)
  quant1 <- quantile(x, c((1 - conf.int) / 2, (1 + conf.int) / 2))
  quant2 <- quantile(y, c((1 - conf.int) / 2, (1 + conf.int) / 2))
  quant <- quantile(y - x, c((1 - conf.int) / 2, (1 + conf.int) / 2))
  names(quant1) <- NULL
  names(quant2) <- NULL
  names(quant) <- NULL
  
  res1 <- data.frame(variable = x.name, Mean = mean(x), 
                     Lower = quant1[1], Upper = quant1[2],
                     stringsAsFactors = FALSE)
  
  res2 <- data.frame(variable = y.name, Mean = mean(y), Lower = quant2[1], 
                     Upper = quant2[2],
                     stringsAsFactors = FALSE)
  
  res <- data.frame(variable = paste(y.name, "-", x.name), 
                    Mean = Mean, Lower = quant[1], Upper = quant[2],
                    stringsAsFactors = FALSE)
  
  res <- bind_rows(res1, res2, res)
  res$variable <- factor(res$variable)
  res
}


## list update function
## for lattice type object structure and ... handling

## (currently used by)
## (all openair plots that include colorkey controlled by drawOpenKey)

## listUpdate function
# [in development]
listUpdate <- function(a, b, drop.dots = TRUE,
                       subset.a = NULL, subset.b = NULL) {
  if (drop.dots) {
    a <- a[names(a) != "..."]
    b <- b[names(b) != "..."]
  }
  if (!is.null(subset.a)) {
    a <- a[names(a) %in% subset.a]
  }
  if (!is.null(subset.b)) {
    b <- b[names(b) %in% subset.b]
  }
  if (length(names(b) > 0)) {
    a <- modifyList(a, b)
  }
  a
}


## makeOpenKeyLegend v0.1

## common code for making legend list
## objects for use with drawOpenkey outputs

## uses listUpdate in utilities

makeOpenKeyLegend <- function(key, default.key, fun.name = "function") {
  # handle logicals and lists
  if (is.logical(key)) {
    legend <- if (key) default.key else NULL
  } else if (is.list(key)) {
    legend <- listUpdate(default.key, key)
  } else {
    if (!is.null(key)) {
      warning(
        paste(
          "In ", fun.name, "(...):\n unrecognised key not exported/applied\n",
          " [see ?drawOpenKey for key structure/options]",
          sep = ""
        ),
        call. = FALSE
      )
    }
    legend <- NULL
  }
  
  # structure like legend for drawOpenKey
  if (!is.null(legend)) {
    legend <- list(right = list(
      fun = drawOpenKey, args = list(key = legend),
      draw = FALSE
    ))
    if ("space" %in% names(legend$right$args$key)) {
      names(legend)[[1]] <- legend$right$args$key$space
    }
  }
  legend
}

## polygon that can deal with missing data for use in lattice plots with groups
poly.na <- function(x1, y1, x2, y2, group.number, myColors, alpha = 0.4, border = NA) {
  for (i in seq(2, length(x1)))
    if (!any(is.na(y2[c(i - 1, i)]))) {
      lpolygon(
        c(x1[i - 1], x1[i], x2[i], x2[i - 1]),
        c(y1[i - 1], y1[i], y2[i], y2[i - 1]),
        col = myColors[group.number], border = border, alpha = alpha
      )
    }
}


## gives names of lattice strips
strip.fun <- function(results.grid, type, auto.text) {
  ## proper names of labelling ###################################################
  pol.name <- sapply(
    levels(factor(results.grid[[type[1]]])),
    function(x) quickText(x, auto.text)
  )
  strip <- strip.custom(factor.levels = pol.name)
  
  if (length(type) == 1) {
    strip.left <- FALSE
  } else { ## two conditioning variables
    
    pol.name <- sapply(
      levels(factor(results.grid[[type[2]]])),
      function(x) quickText(x, auto.text)
    )
    strip.left <- strip.custom(factor.levels = pol.name)
  }
  if (length(type) == 1 & type[1] == "default") strip <- FALSE ## remove strip
  list(strip, strip.left, pol.name)
}



## from lattice
chooseFace <- function(fontface = NULL, font = 1) {
  if (is.null(fontface)) {
    font
  } else {
    fontface
  }
}


## .smoothScatterCalcDensity() is also in graphics, but not exported.
.smoothScatterCalcDensity <- function(x, nbin, bandwidth, range.x) {
  if (!("KernSmooth" %in% loadedNamespaces())) {
    ns <- try(loadNamespace("KernSmooth"))
    if (isNamespace(ns)) {
      message("(loaded the KernSmooth namespace)")
    } else {
      stop("panel.smoothScatter() requires the KernSmooth package, but unable to load KernSmooth namespace")
    }
  }
  if (length(nbin) == 1) {
    nbin <- c(nbin, nbin)
  }
  if (!is.numeric(nbin) || (length(nbin) != 2)) stop("'nbin' must be numeric of length 1 or 2")
  if (missing(bandwidth)) {
    bandwidth <- diff(apply(x, 2, quantile, probs = c(0.05, 0.95), na.rm = TRUE)) / 25
  } else {
    if (!is.numeric(bandwidth)) stop("'bandwidth' must be numeric")
  }
  bandwidth[bandwidth == 0] <- 1
  ## create density map
  if (missing(range.x)) {
    rv <- KernSmooth::bkde2D(x, gridsize = nbin, bandwidth = bandwidth)
  } else {
    rv <- KernSmooth::bkde2D(x, gridsize = nbin, bandwidth = bandwidth, range.x = range.x)
  }
  rv$bandwidth <- bandwidth
  return(rv)
}




## simple rounding function from plyr
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

## pretty gap calculator
prettyGap <- function(x, n = 100) {
  return(diff(pretty(x, n))[1])
}

# function to check variables are numeric, if not force with warning
checkNum <- function(mydata, vars) {
  for (i in seq_along(vars)) {
    if (!is.numeric(mydata[[vars[i]]])) {
      mydata[[vars[i]]] <- as.numeric(as.character(mydata[[vars[i]]]))
      
      warning(
        paste(vars[i], "is not numeric, forcing to numeric..."),
        call. = FALSE
      )
    }
  }
  
  return(mydata)
}



#' Bin data, calculate mean and bootstrap 95\% confidence interval in the mean
#'
#' Bin a variable and calculate mean an uncertainties in mean
#'
#' This function summarises data by intervals and calculates the mean and
#' bootstrap 95\% confidence intervals in the mean of a chosen variable in a data
#' frame. Any other numeric variables are summarised by their mean intervals.
#'
#' There are three options for binning. The default is to bon \code{bin} into 40
#' intervals. Second, the user can choose an binning interval e.g.
#' \code{interval = 5}. Third, the user can supply their own breaks to use as
#' binning intervals.
#'
#' @param mydata Name of the data frame to process.
#' @param bin The name of the column to divide into intervals
#' @param uncer The name of the column for which the mean, lower and upper
#'   uncertainties should be calculated for each interval of \code{bin}.
#' @param n The number of intervals to split \code{bin} into.
#' @param interval The interval to be used for binning the data.
#' @param breaks User specified breaks to use for binning.
#'
#' @return Retruns a summarised data frame with new columns for the mean and
#'   upper / lower 95\% confidence intervals in the mean.
#' @export
#'
#' @examples
#' # how does nox vary by intervals of wind speed?
#' results <- binData(mydata, bin = "ws", uncer = "nox")
#'
#' # easy to plot this using ggplot2
#' \dontrun{
#' library(ggplot2)
#' ggplot(results, aes(ws, mean, ymin = min, ymax = max)) +
#' geom_pointrange()
#'
#' }
binData <- function(mydata, bin = "nox", uncer = "no2", n = 40, interval = NA,
                    breaks = NA) {
  if (!is.na(interval)) {
    mydata$interval <- cut(
      mydata[[bin]], sort(unique(round_any(mydata[[bin]], interval))),
      include.lowest = TRUE
    )
  } else if (!anyNA(breaks)) {
    mydata$interval <- cut(mydata[[bin]], breaks = breaks, include.lowest = TRUE)
  } else {
    mydata$interval <- cut(mydata[[bin]], breaks = n)
  }
  
  # remove any missing intervals
  id <- which(is.na(mydata$interval))
  if (length(id) > 0)
    mydata <- mydata[-id, ]
  
  # calculate 95% CI in mean
  uncert <- group_by(mydata, interval) %>%
    do(bootMeanDF(.[[uncer]], B = 250)) 
  
  mydata <- group_by(mydata, interval) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  mydata <- inner_join(mydata, uncert, by = "interval")
  
  mydata
}


#------------------------------------------------------------------------------

#-- checkPrep -----------------------------------------------------------------
checkPrep <- function(mydata, Names, type, remove.calm = TRUE, remove.neg = TRUE,
                      strip.white = TRUE, wd = "wd") {
  library(lubridate)
  
  ## deal with conditioning variable if present, if user-defined, must exist in data
  ## pre-defined types
  ## existing conditioning variables that only depend on date (which is checked)
  conds <- c(
    "default", "year", "hour", "month", "season", "weekday",
    "weekend", "monthyear", "gmtbst", "bstgmt", "dst", "daylight",
    "yearseason", "seasonyear"
  )
  all.vars <- unique(c(names(mydata), conds))
  
  varNames <- c(Names, type) ## names we want to be there
  matching <- varNames %in% all.vars
  
  if (any(!matching)) {
    ## not all variables are present
    stop(cat("Can't find the variable(s)", varNames[!matching], "\n"))
  }
  
  ## add type to names if not in pre-defined list
  if (any(type %in% conds == FALSE)) {
    ids <- which(type %in% conds == FALSE)
    Names <- c(Names, type[ids])
  }
  
  ## if type already present in data frame
  if (any(type %in% names(mydata))) {
    ids <- which(type %in% names(mydata))
    Names <- unique(c(Names, type[ids]))
  }
  
  ## just select data needed
  mydata <- mydata[, Names]
  
  ## if site is in the data set, check none are missing
  ## seems to be a problem for some KCL data...
  if ("site" %in% names(mydata)) { ## split by site
    
    ## remove any NA sites
    if (anyNA(mydata$site)) {
      id <- which(is.na(mydata$site))
      mydata <- mydata[-id, ]
    }
  }
  
  
  ## sometimes ratios are considered which can results in infinite values
  ## make sure all infinite values are set to NA
  mydata[] <- lapply(mydata, function(x) {
    replace(x, x == Inf | x == -Inf, NA)
  })
  
  if ("ws" %in% Names) {
    if ("ws" %in% Names & is.numeric(mydata$ws)) {
      
      ## check for negative wind speeds
      if (any(sign(mydata$ws[!is.na(mydata$ws)]) == -1)) {
        if (remove.neg) { ## remove negative ws only if TRUE
          warning("Wind speed <0; removing negative data")
          mydata$ws[mydata$ws < 0] <- NA
        }
      }
    }
  }
  
  ## round wd to make processing obvious
  ## data already rounded to nearest 10 degress will not be affected
  ## data not rounded will be rounded to nearest 10 degrees
  ## assumes 10 is average of 5-15 etc
  if (wd %in% Names) {
    if (wd %in% Names & is.numeric(mydata[, wd])) {
      
      ## check for wd <0 or > 360
      if (any(sign(mydata[[wd]][!is.na(mydata[[wd]])]) == -1 |
              mydata[[wd]][!is.na(mydata[[wd]])] > 360)) {
        warning("Wind direction < 0 or > 360; removing these data")
        mydata[[wd]][mydata[[wd]] < 0] <- NA
        mydata[[wd]][mydata[[wd]] > 360] <- NA
      }
      
      if (remove.calm) {
        if ("ws" %in% names(mydata)) {
          mydata[[wd]][mydata$ws == 0] <- NA ## set wd to NA where there are calms
          mydata$ws[mydata$ws == 0] <- NA ## remove calm ws
        }
        mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360
        
        ## round wd for use in functions - except windRose/pollutionRose
        mydata[[wd]] <- 10 * ceiling(mydata[[wd]] / 10 - 0.5)
        mydata[[wd]][mydata[[wd]] == 0] <- 360 # angles <5 should be in 360 bin
      }
      mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360
    }
  }
  
  
  ## make sure date is ordered in time if present
  if ("date" %in% Names) {
    if ("POSIXlt" %in% class(mydata$date)) {
      stop("date should be in POSIXct format not POSIXlt")
    }
    
    ## if date in format dd/mm/yyyy hh:mm (basic check)
    if (length(grep("/", as.character(mydata$date[1]))) > 0) {
      mydata$date <- as.POSIXct(strptime(mydata$date, "%d/%m/%Y %H:%M"), "GMT")
    }
    
    ## try and work with a factor date - but probably a problem in original data
    if (is.factor(mydata$date)) {
      warning("date field is a factor, check date format")
      mydata$date <- as.POSIXct(mydata$date, "GMT")
    }
    
    mydata <- arrange(mydata, date)
    
    ## make sure date is the first field
    if (names(mydata)[1] != "date") {
      mydata <- mydata[c("date", setdiff(names(mydata), "date"))]
    }
    
    ## check to see if there are any missing dates, stop if there are
    ids <- which(is.na(mydata$date))
    if (length(ids) > 0) {
      mydata <- mydata[-ids, ]
      warning(paste(
        "Missing dates detected, removing",
        length(ids), "lines"
      ), call. = FALSE)
    }
    
    ## daylight saving time can cause terrible problems - best avoided!!
    
    if (any(dst(mydata$date))) {
      warning("Detected data with Daylight Saving Time, converting to UTC/GMT")
      mydata$date <- lubridate::force_tz(mydata$date, tzone = "GMT")
    }
  }
  
  
  
  
  if (strip.white) {
    ## set panel strip to white
    suppressWarnings(trellis.par.set(list(strip.background = list(col = "white"))))
  }
  
  
  ## return data frame
  return(mydata)
}

#------------------------------------------------------------------------------

#-- drawOpenKey ---------------------------------------------------------------
########################################
# drawOpenKey v0.2
########################################
# drawOpenKey is a modification of:
# draw.colorkey
#
# original code from lattice, reference:
# Deepayan Sarkar (2010). lattice: Lattice Graphics.
# R package version 0.18-5.
# http://r-forge.r-project.org/projects/lattice/
#
# additional code by Karl Ropkins, allows:
# some crude header and footer labelling
# text formatting by openair::quickText
# addition plot style and layout control
########################################

########################################
# The help, advice and extreme patience
# of Deepayan Sarkar are also gratefully
# acknowledged
########################################



##' Scale key handling for openair
##'
##' General function for producing scale keys for other openair functions.  The
##' function is a crude modification of the draw.colorkey function developed by
##' Deepayan Sarkar as part of the lattice package, and allows additional key
##' labelling to added, and provides some additional control of the appearance
##' and scaling.
##'
##' The \code{drawOpenKey} function produces scale keys for other openair
##' functions.
##'
##' Most \code{drawOpenKey} options are identical to those of
##' \code{lattice::draw.colorkey}.  For example, scale key size and position
##' are controlled via \code{height}, \code{width} and \code{space}. Likewise,
##' the axis labelling can be set in and formatted by \code{labels}. See
##' \code{\link{draw.colorkey}} for further details.
##'
##' Additional scale labelling may be added above and below the scale using
##' \code{header} and \code{footer} options within \code{key}. As in other
##' \code{openair} functions, automatic text formatting can be enabled via
##' \code{auto.key}.
##'
##' (Note: Currently, the formatting of \code{header} and \code{footer} text
##' are fixed to the same style as \code{labels} (the scale axis) and cannot be
##' defined locally.)
##'
##' The relationship between \code{header}, \code{footer} and the scale key
##' itself can be controlled using \code{fit} options. These can be set in
##' \code{key$fit} to apply uniform control or individually in
##' \code{key$header$fit} and/or \code{key$footer$fit} to control locally.
##'
##' The appearance of the scale can be controlled using \code{plot.style}.
##'
##' @param key List defining the scale key structure to be produced. Most
##'   options are identical to original \code{draw.colorkey} function.
##'
##' Original \code{draw.colorkey} options:
##'
##' \code{space} location of the scale key ("left", "right", "top" or
##'   "bottom").  Defaults to "right".
##'
##' \code{col} vector of colours, used in scale key.
##'
##' \code{at} numeric vector specifying where the colors change. Must be of
##'   length 1 more than the col vector.
##'
##' \code{labels} a character vector for labelling the at values, or more
##'   commonly, a list describing characteristics of the labels. This list may
##'   include components \code{labels}, \code{at}, \code{cex}, \code{col},
##'   \code{rot}, \code{font}, \code{fontface} and \code{fontfamily}.
##'
##' \code{tick.number} approximate number of ticks.
##'
##' \code{width} width of the key.
##'
##' \code{height} height of key.
##'
##' Note: \code{width} and \code{height} refer to the key dimensions.
##'   \code{height} is the length of the key along the plot axis it is
##'   positioned against, and \code{width} is the length perpendicular to that.
##'
##' Additional options include:
##'
##' \code{header} a character vector of extra text to be added above the key,
##'   or a list describing some characteristics of the \code{header}. This list
##'   may include components \code{header}, the character vector of header
##'   labels, \code{tweaks}, a list of local controls, e.g. 'gap' and 'balance'
##'   for spacing relative to scale and footer, respectively, \code{auto.text},
##'   \code{TRUE/FALSE} option to apply \code{quickText}, and \code{slot}, a
##'   numeric vector setting the size of the text boxes \code{header} text is
##'   placed in.
##'
##' \code{footer} as in \code{header} but for labels below the scale key.
##'
##' Notes: \code{header} and \code{footer} formatting can not be set locally,
##'   but instead are matched to those set in \code{labels}. \code{drawOpenKey}
##'   allows for up to six additional labels (three above and three below scale
##'   key). Any additional text is ignored.
##'
##' \code{tweak, auto.text, slot} as in \code{header} and \code{footer} but
##'   sets all options uniformly. This also overwrites anything in
##'   \code{header} and/or \code{footer}.
##'
##' \code{fit} the fit method to be applied to the header, scale key and footer
##'   when placing the scale key left or right of the plot. Options include:
##'   'all', 'soft' and 'scale'.  The default 'all' fits header, key and footer
##'   into \code{height} range. The alternative 'scale' fits only the key
##'   within \code{height}. (This means that keys keep the same proportions
##'   relative to the main plot regardless of positioning but that header and
##'   footer may exceed plot dimensions if \code{height} and/or \code{slots}
##'   are too large.
##'
##' \code{plot.style} a character vector of key plotting style instructions:
##'   Options currently include: 'paddle', 'ticks' and 'border'. 'paddle'
##'   applies the incremental paddle layout used by \code{winRose}. 'ticks'
##'   places ticks between the labels scale key. 'border' places a border about
##'   the scale key. Any combination of these may be used but if none set,
##'   scale key defaults to \code{c("ticks", "border")} for most plotting
##'   operations or \code{c("paddle")} for \code{windRose}.
##'

##' @param draw Option to return the key object or plot it directly.  The
##'   default, FALSE, should always be used within openair calls.
##' @param vp View port to be used when plotting key. The default, NULL, should
##'   always be used within openair calls.
##'
##' (Note: \code{drawOpenKey} is a crude modification of
##'   \code{lattice::draw.colorkey}, that provides labelling options for
##'   \code{openair} plot scale keys. Some aspects of the function are in
##'   development and may to subject to change. Therefore, it is recommended
##'   that you use parent \code{openair} function controls, e.g.
##'   \code{key.position}, \code{key.header}, \code{key.footer} options, where
##'   possible.  \code{drawOpenKey} may obviously be used in other plots but it
##'   is recommended that \code{draw.colorkey} itself be used wherever this
##'   type of additional scale labelling is not required.)
##' @export
##' @return The function is a modification of \code{lattice::draw.colorkey} and
##'   returns a scale key using a similar mechanism to that used in in the
##'   original function as developed by Deepayan Sarkar.
##' @note We gratefully acknoweldge the considerable help and advice of
##'   Deepayan Sarkar.
##' @author \code{draw.colorkey} is part of the \code{lattice} package,
##'   developed by Deepayan Sarkar.
##'
##' Additional modifications by Karl Ropkins.
##' @seealso Functions using \code{drawOpenKey} currently include
##'   \code{\link{windRose}}, \code{\link{pollutionRose}}.
##'
##' For details of the original function, see \code{\link{draw.colorkey}}
##' @references Deepayan Sarkar (2010). lattice: Lattice Graphics. R package
##'   version 0.18-5.  http://r-forge.r-project.org/projects/lattice/
##' @keywords methods
##' @examples
##'
##'
##' ##########
##' #example 1
##' ##########
##'
##' #paddle style scale key used by windRose
##'
##' windRose(mydata,)
##'
##' #adding text and changing style and position via key
##'
##' #note:
##' #some simple key control also possible directly
##' #For example, below does same as
##' #windRose(mydata, key.position="right")
##'
##' windRose(mydata,
##'    key =list(space="right")
##' )
##'
##' #however:
##' #more detailed control possible working with
##' #key and drawOpenKey. For example,
##'
##' windRose(mydata,
##'    key = list(header="Title", footer="wind speed",
##'               plot.style = c("ticks", "border"),
##'               fit = "all", height = 1,
##'               space = "top")
##' )
##'
##'
drawOpenKey <- function(key, draw = FALSE, vp = NULL) {
  
  ################
  # quick end if key obviously not right
  ################
  if (!is.list(key)) {
    stop(
      "In drawOpenKey(...) key must be a list",
      call. = FALSE
    )
  }
  
  ################
  # special case
  # windRose colour key
  ################
  if (is.null(key$at)) {
    if (is.null(key$labels)) {
      stop(
        "In drawOpenKey(...) neither 'at' nor 'labels' in key",
        "\n\tplease suppied at least one",
        call. = FALSE
      )
    } else {
      if (is.list(key$labels)) {
        if (is.null(key$labels$labels)) {
          stop(
            "In drawOpenKey(...) unable to recover missing 'at' in key",
            "\n\tplease check 'labels' structure or add 'at'",
            call. = FALSE
          )
        }
        key$at <- 0:length(key$labels$labels)
        if (is.null(key$labels$at)) {
          key$labels$at <- 1:length(key$labels$labels) - 0.5
        }
      } else {
        key$at <- 0:length(key$labels)
        key$labels <- list(
          labels = key$labels,
          at = 1:length(key$labels) - 0.5
        )
      }
    }
  }
  
  ################
  # process key
  # modification of sk
  ################
  process.key <- function(col = regions$col, alpha = regions$alpha,
                          at, tick.number = 7, width = 2, height = 1, space = "right",
                          plot.style = c("ticks", "border"),
                          ...) {
    regions <- trellis.par.get("regions")
    list(
      col = col, alpha = alpha, at = at, tick.number = tick.number,
      width = width, height = height, space = space,
      plot.style = plot.style,
      ...
    )
  }
  axis.line <- trellis.par.get("axis.line")
  axis.text <- trellis.par.get("axis.text")
  key <- do.call("process.key", key)
  
  ###############
  # test space
  # otherwise drops without creating key.gf
  # COULD default to one?
  ###############
  temp <- c("right", "left", "top", "bottom")
  if (!key$space %in% temp) {
    stop(
      " In drawOpenKey(...):", "\n\tkey.position (space) argument in key not recognised",
      "\n\tplease use one of:\n\t\"", paste(
        temp,
        sep = "", collapse = "\", \""
      ), "\"",
      call. = FALSE
    )
  }
  
  ###############
  # original sk key handling
  # with
  # modified error messaging
  ###############
  check.overlap <- TRUE
  key$at <- sort(key$at)
  numcol <- length(key$at) - 1
  key$col <- level.colors(x = seq_len(numcol) - 0.5, at = seq_len(numcol +
                                                                    1) - 1, col.regions = key$col, colors = TRUE)
  atrange <- range(key$at, finite = TRUE)
  scat <- as.numeric(key$at)
  reccentre <- (scat[-1] + scat[-length(scat)]) / 2
  recdim <- diff(scat)
  cex <- axis.text$cex
  col <- axis.text$col
  font <- axis.text$font
  fontfamily <- "serif"
  fontface <- axis.text$fontface
  rot <- 0
  if (is.null(key$lab)) {
    at <- pretty(atrange, key$tick.number)
    at <- at[at >= atrange[1] & at <= atrange[2]]
    labels <- format(at, trim = TRUE)
  } else if ((is.character(key$lab) | is.expression(key$lab) | is.numeric(key$lab))
             && length(key$lab) == length(key$at)) {
    check.overlap <- FALSE
    at <- key$at
    labels <- key$lab
  } else if (is.list(key$lab)) {
    at <- if (!is.null(key$lab$at)) {
      key$lab$at
    } else {
      pretty(atrange, key$tick.number)
    }
    at <- at[at >= atrange[1] & at <= atrange[2]]
    labels <- if (!is.null(key$lab$lab)) {
      check.overlap <- FALSE
      key$lab$lab
    } else {
      format(at, trim = TRUE)
    }
    if (!is.null(key$lab$cex)) {
      cex <- key$lab$cex
    }
    if (!is.null(key$lab$col)) {
      col <- key$lab$col
    }
    if (!is.null(key$lab$font)) {
      font <- key$lab$font
    }
    if (!is.null(key$lab$fontface)) {
      fontface <- key$lab$fontface
    }
    if (!is.null(key$lab$fontfamily)) {
      fontfamily <- key$lab$fontfamily
    }
    if (!is.null(key$lab$rot)) {
      rot <- key$lab$rot
    }
  } else {
    stop(
      "In drawOpenKey(...) unexpected labels structure in key",
      "\n\tplease check 'labels' structure",
      "\n\tor see 'labels' in ?drawOpenKey",
      call. = FALSE
    )
  }
  labscat <- at
  rot <- 0
  
  #############
  # header set up
  #############
  if (is.null(key$hea)) {
    key$hea <- list(header = "")
  }
  if (is.character(key$hea) | is.numeric(key$hea) | is.expression(key$hea)) {
    key$hea <- list(header = key$hea)
  }
  if (is.list(key$hea)) {
    h.text <- if (is.null(key$hea$hea)) "" else key$hea$hea
    h.tweaks <- if (is.null(key$hea$twe)) c("gap", "balance") else key$hea$twe
    h.auto.text <- if (is.null(key$hea$auto.text)) TRUE else key$hea$auto.text
    h.slot <- if (is.null(key$hea$slot)) 0.05 else key$hea$slot
  } else {
    stop(
      "In drawOpenKey(...) unexpected header structure in key",
      "\n\tplease check 'header' structure",
      "\n\tor see 'header' in ?drawOpenKey",
      call. = FALSE
    )
  }
  
  ############
  # footer setup
  ############
  if (is.null(key$foo)) {
    key$foo <- list(footer = "")
  }
  if (is.character(key$foo) | is.numeric(key$foo) | is.expression(key$foo)) {
    key$foo <- list(footer = key$foo)
  }
  if (is.list(key$foo)) {
    f.text <- if (is.null(key$foo$foo)) "" else key$foo$foo
    f.tweaks <- if (is.null(key$foo$twe)) c("gap", "balance") else key$foo$twe
    f.auto.text <- if (is.null(key$foo$auto.text)) TRUE else key$foo$auto.text
    f.slot <- if (is.null(key$foo$slot)) 0.05 else key$foo$slot
  } else {
    stop(
      "In drawOpenKey(...) unexpected footer structure in key",
      "\n\tplease check 'footer' structure",
      "\n\tor see 'footer' in ?drawOpenKey",
      call. = FALSE
    )
  }
  
  #################
  # higher level handling
  # auto.text, slot, tweak,
  #################
  if (!is.null(key$auto.text)) {
    if (is.logical(key$auto.text)) {
      h.auto.text <- key$auto.text
      f.auto.text <- key$auto.text
    }
  }
  if (!is.null(key$slot)) {
    if (is.numeric(key$slot)) {
      h.slot <- key$slot
      f.slot <- key$slot
    }
  }
  if (!is.null(key$twe)) {
    if (is.vector(key$twe)) {
      h.tweaks <- key$twe
      f.tweaks <- key$twe
    }
  }
  
  ###############
  # size text boxes, balance and gap
  # for
  # top and bottom only
  ###############
  h.text <- if (length(h.text) < 3) {
    c(rep("", 3 - length(h.text)), h.text)
  } else {
    h.text[1:3]
  }
  h.slots <- ifelse(as.character(h.text) != "", h.slot, 0)
  f.text <- c(f.text, rep("", 3))[1:3]
  f.slots <- ifelse(as.character(f.text) != "", f.slot, 0)
  if (sum(h.slots) > sum(f.slots) & "balance" %in% f.tweaks) {
    f.slots[3] <- f.slots[3] + sum(h.slots) - sum(f.slots)
  }
  if (sum(f.slots) > sum(h.slots) & "balance" %in% h.tweaks) {
    h.slots[1] <- h.slots[1] + sum(f.slots) - sum(h.slots)
  }
  g.slots <- c(
    if ("gap" %in% h.tweaks & sum(c(h.slots, f.slots)) > 0) h.slot else 0,
    if ("gap" %in% f.tweaks & sum(c(h.slots, f.slots)) > 0) f.slot else 0
  )
  
  #############
  # scale fit
  # scale, soft and all
  # default all
  #############
  s.slot <- 1 - sum(c(h.slots, f.slots, g.slots))
  s.offsets <- c(0, 0)
  if (!is.null(key$fit)) {
    if (is.character(key$fit)) {
      if (key$fit == "soft") {
        s.slot <- 1 - (sum(c(h.slots, f.slots, g.slots)) / 2)
      }
      if (key$fit == "scale") {
        s.slot <- 1
        s.offsets <- c(
          sum(c(h.slots, g.slots[1])),
          sum(c(f.slots, g.slots[2]))
        )
      }
    } else {
      stop(
        "In drawOpenKey(...) unexpected fit structure in key",
        "\n\tplease check 'fit' structure",
        "\n\tor see 'fit' in ?drawOpenKey",
        call. = FALSE
      )
    }
  }
  
  ############
  # paddle style
  # recwd rescaling
  #############
  recwd <- if ("paddle" %in% key$plot.style) {
    recwd <- seq(0.2, 1, length.out = length(key$at) - 1)
  } else {
    recwd <- rep(1, length(key$at) - 1)
  }
  
  #####################
  # right scale
  # size checks text see sac struff
  # positions
  # adds ticks and borders if requested
  #####################
  if (key$space == "right") {
    h.text <- if (is.character(h.text)) {
      lapply(h.text, function(x) quickText(x, h.auto.text))
    } else {
      list(h.text[1], h.text[2], h.text[3])
    }
    f.text <- if (is.character(f.text)) {
      lapply(f.text, function(x) quickText(x, h.auto.text))
    } else {
      list(f.text[1], f.text[2], f.text[3])
    }
    # sac stuff handles spacing needed for headers, scale and footers
    sac.text <- c(
      labels, f.text[[1]], f.text[[2]], f.text[[3]],
      h.text[[1]], h.text[[2]], h.text[[3]]
    )
    SacGrob <- textGrob(
      label = sac.text, x = rep(0, length(sac.text)),
      y = at, vp = viewport(yscale = atrange), default.units = "native",
      check.overlap = check.overlap, just = if (rot == -90) {
        c("center", "bottom")
      } else {
        c("left", "center")
      }, rot = rot, gp = gpar(
        col = col,
        cex = cex, fontfamily = fontfamily, fontface = chooseFace(
          fontface,
          font
        )
      )
    )
    heights.x <- c(
      ((1 - key$height) / 2) - (key$height * s.offsets[1]),
      key$height * h.slots[1], key$height * h.slots[2], key$height * h.slots[3],
      key$height * g.slots[1], (key$height * s.slot), key$height * g.slots[2],
      key$height * f.slots[1], key$height * f.slots[2], key$height * f.slots[3],
      ((1 - key$height) / 2) - (key$height * s.offsets[2])
    )
    heights.units <- rep("null", 11)
    temp <- if ("ticks" %in% key$plot.style) 0.6 else 0.3
    widths.x <- c(0.6 * key$width, temp, 1)
    widths.units <- c("lines", "lines", "grobwidth")
    widths.data <- list(NULL, NULL, SacGrob)
    key.layout <- grid.layout(nrow = 11, ncol = 3, heights = unit(
      heights.x,
      heights.units
    ), widths = unit(
      widths.x, widths.units,
      data = widths.data
    ), respect = TRUE)
    key.gf <- frameGrob(layout = key.layout, vp = vp)
    add.header.footer <- function(key.gf, text, key.row, key.col) {
      keyGrob <- textGrob(
        label = text, x = c(0),
        y = c(0.5), vp = viewport(yscale = c(0, 1)), default.units = "native",
        check.overlap = check.overlap, just = if (rot == -90) {
          c("center", "bottom")
        } else {
          c("left", "center")
        }, rot = rot, gp = gpar(
          col = col,
          cex = cex, fontfamily = fontfamily, fontface = chooseFace(
            fontface,
            font
          )
        )
      )
      placeGrob(key.gf, keyGrob, row = key.row, col = key.col)
    }
    key.gf <- add.header.footer(key.gf, h.text[[1]], 2, 3)
    key.gf <- add.header.footer(key.gf, h.text[[2]], 3, 3)
    key.gf <- add.header.footer(key.gf, h.text[[3]], 4, 3)
    key.gf <- add.header.footer(key.gf, f.text[[1]], 8, 3)
    key.gf <- add.header.footer(key.gf, f.text[[2]], 9, 3)
    key.gf <- add.header.footer(key.gf, f.text[[3]], 10, 3)
    key.gf <- placeGrob(key.gf, textGrob(
      label = labels, x = rep(0, length(at)),
      y = at, vp = viewport(yscale = atrange), default.units = "native",
      check.overlap = check.overlap, just = if (rot == -90) {
        c("center", "bottom")
      } else {
        c("left", "center")
      }, rot = rot, gp = gpar(
        col = col,
        cex = cex, fontfamily = fontfamily, fontface = chooseFace(
          fontface,
          font
        )
      )
    ), row = 6, col = 3)
    key.gf <- placeGrob(key.gf, rectGrob(
      x = rep(0.5, length(reccentre)),
      y = reccentre, default.units = "native", vp = viewport(yscale = atrange),
      height = recdim, width = recwd, gp = gpar(
        fill = key$col, col = "transparent",
        alpha = key$alpha
      )
    ), row = 6, col = 1)
    if ("border" %in% key$plot.style) {
      key.gf <- placeGrob(frame = key.gf, rectGrob(gp = gpar(
        col = axis.line$col,
        lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha,
        fill = "transparent"
      )), row = 6, col = 1)
    }
    if ("ticks" %in% key$plot.style) {
      key.gf <- placeGrob(frame = key.gf, segmentsGrob(
        x0 = rep(
          0,
          length(labscat)
        ), y0 = labscat, x1 = rep(0.4, length(labscat)),
        y1 = labscat, vp = viewport(yscale = atrange), default.units = "native",
        gp = gpar(
          col = axis.line$col, lty = axis.line$lty,
          lwd = axis.line$lwd
        )
      ), row = 6, col = 2)
    }
  }
  
  #####################
  # left scale
  # size checks text see sac struff
  # positions
  # adds ticks and borders if requested
  #####################
  else if (key$space == "left") {
    h.text <- if (is.character(h.text)) {
      lapply(h.text, function(x) quickText(x, h.auto.text))
    } else {
      list(h.text[1], h.text[2], h.text[3])
    }
    f.text <- if (is.character(f.text)) {
      lapply(f.text, function(x) quickText(x, h.auto.text))
    } else {
      list(f.text[1], f.text[2], f.text[3])
    }
    # sac stuff handles spacing needed for headers, scale and footers
    sac.text <- c(
      labels, f.text[[1]], f.text[[2]], f.text[[3]],
      h.text[[1]], h.text[[2]], h.text[[3]]
    )
    SacGrob <- textGrob(
      label = sac.text, x = rep(0, length(sac.text)),
      y = at, vp = viewport(yscale = atrange), default.units = "native",
      check.overlap = check.overlap, just = if (rot == 90) {
        c("center", "bottom")
      } else {
        c("right", "center")
      }, rot = rot, gp = gpar(
        col = col,
        cex = cex, fontfamily = fontfamily, fontface = chooseFace(
          fontface,
          font
        )
      )
    )
    heights.x <- c(
      ((1 - key$height) / 2) - (key$height * s.offsets[1]),
      key$height * h.slots[1], key$height * h.slots[2], key$height * h.slots[3],
      key$height * g.slots[1], (key$height * s.slot), key$height * g.slots[2],
      key$height * f.slots[1], key$height * f.slots[2], key$height * f.slots[3],
      ((1 - key$height) / 2) - (key$height * s.offsets[2])
    )
    heights.units <- rep("null", 11)
    temp <- if ("ticks" %in% key$plot.style) 0.6 else 0.3
    widths.x <- c(1, temp, 0.6 * key$width)
    widths.units <- c("grobwidth", "lines", "lines")
    widths.data <- list(SacGrob, NULL, NULL)
    key.layout <- grid.layout(nrow = 11, ncol = 3, heights = unit(
      heights.x,
      heights.units
    ), widths = unit(
      widths.x, widths.units,
      data = widths.data
    ), respect = TRUE)
    key.gf <- frameGrob(layout = key.layout, vp = vp)
    add.header.footer <- function(key.gf, text, key.row, key.col) {
      keyGrob <- textGrob(
        label = text, x = c(1),
        y = c(0.5), vp = viewport(yscale = c(0, 1)), default.units = "native",
        check.overlap = check.overlap, just = if (rot == 90) {
          c("center", "bottom")
        } else {
          c("right", "center")
        }, rot = rot, gp = gpar(
          col = col,
          cex = cex, fontfamily = fontfamily, fontface = chooseFace(
            fontface,
            font
          )
        )
      )
      placeGrob(key.gf, keyGrob, row = key.row, col = key.col)
    }
    key.gf <- add.header.footer(key.gf, h.text[[1]], 2, 1)
    key.gf <- add.header.footer(key.gf, h.text[[2]], 3, 1)
    key.gf <- add.header.footer(key.gf, h.text[[3]], 4, 1)
    key.gf <- add.header.footer(key.gf, f.text[[1]], 8, 1)
    key.gf <- add.header.footer(key.gf, f.text[[2]], 9, 1)
    key.gf <- add.header.footer(key.gf, f.text[[3]], 10, 1)
    key.gf <- placeGrob(
      key.gf,
      textGrob(
        label = labels, x = rep(1, length(at)),
        y = at, vp = viewport(yscale = atrange), default.units = "native",
        check.overlap = check.overlap, just = if (rot == 90) {
          c("center", "bottom")
        } else {
          c("right", "center")
        }, rot = rot, gp = gpar(
          col = col,
          cex = cex, fontfamily = fontfamily, fontface = chooseFace(
            fontface,
            font
          )
        )
      )
      ,
      row = 6, col = 1
    )
    key.gf <- placeGrob(key.gf, rectGrob(
      x = rep(0.5, length(reccentre)),
      y = reccentre, default.units = "native", vp = viewport(yscale = atrange),
      height = recdim, width = recwd, gp = gpar(
        fill = key$col, col = "transparent",
        alpha = key$alpha
      )
    ), row = 6, col = 3)
    if ("border" %in% key$plot.style) {
      key.gf <- placeGrob(frame = key.gf, rectGrob(gp = gpar(
        col = axis.line$col,
        lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha,
        fill = "transparent"
      )), row = 6, col = 3)
    }
    if ("ticks" %in% key$plot.style) {
      key.gf <- placeGrob(frame = key.gf, segmentsGrob(
        x0 = rep(
          0.5,
          length(labscat)
        ), y0 = labscat, x1 = rep(1, length(labscat)),
        y1 = labscat, vp = viewport(yscale = atrange), default.units = "native",
        gp = gpar(
          col = axis.line$col, lty = axis.line$lty,
          lwd = axis.line$lwd
        )
      ), row = 6, col = 2)
    }
  }
  
  #####################
  # top scale
  # positions
  # adds ticks and borders if requested
  #####################
  else if (key$space == "top") {
    f.text <- f.text[as.character(f.text) != ""]
    f.text <- if (is.character(f.text)) {
      quickText(paste(f.text, collapse = "  "), f.auto.text)
    } else {
      as.expression(parse(text = paste(f.text, collapse = "~~")))
    }
    h.text <- h.text[as.character(h.text) != ""]
    h.text <- if (is.character(h.text)) {
      quickText(paste(h.text, collapse = "  "), f.auto.text)
    } else {
      as.expression(parse(text = paste(h.text, collapse = "~~")))
    }
    labelsGrob <- textGrob(
      label = labels, y = rep(0, length(at)),
      x = at, vp = viewport(xscale = atrange), default.units = "native",
      check.overlap = check.overlap, just = if (rot == 0) {
        c("center", "bottom")
      } else {
        c("left", "center")
      }, rot = rot, gp = gpar(
        col = col,
        cex = cex, fontfamily = fontfamily, fontface = chooseFace(
          fontface,
          font
        )
      )
    )
    keyGrob <- textGrob(
      label = f.text, y = c(0),
      x = c(0.5), vp = viewport(xscale = c(0, 1)), default.units = "native",
      check.overlap = check.overlap, just = if (rot == 0) {
        c("center", "bottom")
      } else {
        c("left", "center")
      }, rot = rot, gp = gpar(
        col = col,
        cex = cex, fontfamily = fontfamily, fontface = chooseFace(
          fontface,
          font
        )
      )
    )
    keyGrob2 <- textGrob(
      label = h.text, y = c(0),
      x = c(0.5), vp = viewport(xscale = c(0, 1)), default.units = "native",
      check.overlap = check.overlap, just = if (rot == 0) {
        c("center", "bottom")
      } else {
        c("left", "center")
      }, rot = rot, gp = gpar(
        col = col,
        cex = cex, fontfamily = fontfamily, fontface = chooseFace(
          fontface,
          font
        )
      )
    )
    widths.x <- c((1 - key$height) / 2, key$height, (1 - key$height) / 2)
    widths.units <- rep("null", 3)
    temp <- c(0, 0, 0, 0.3)
    if (sum(f.slots[1:2]) > 0 & sum(h.slots[2:3]) == 0) {
      temp[1:3] <- c(0, 1, 1.5)
    }
    if (sum(f.slots[1:2]) == 0 & sum(h.slots[2:3]) > 0) {
      temp[1:3] <- c(1, 0, 1.5)
    }
    if (sum(f.slots[1:2]) > 0 & sum(h.slots[2:3]) > 0) {
      temp[1:3] <- c(1, 1.5, 1.5)
    }
    if (sum(f.slots[1:2]) == 0 & sum(h.slots[2:3]) == 0) {
      temp[1:3] <- c(1, 1, 1)
    }
    if ("ticks" %in% key$plot.style) {
      temp[4] <- 0.6
    }
    heights.x <- c(temp[1], temp[2], temp[3], temp[4], 0.6 * key$width)
    heights.units <- c("grobheight", "grobheight", "grobheight", "lines", "lines")
    heights.data <- list(keyGrob2, keyGrob, labelsGrob, NULL, NULL)
    key.layout <- grid.layout(nrow = 5, ncol = 3, heights = unit(
      heights.x,
      heights.units,
      data = heights.data
    ), widths = unit(
      widths.x,
      widths.units
    ), respect = TRUE)
    key.gf <- frameGrob(layout = key.layout, vp = vp)
    key.gf <- placeGrob(key.gf, rectGrob(
      y = rep(0.5, length(reccentre)),
      x = reccentre, default.units = "native", vp = viewport(xscale = atrange),
      width = recdim, height = recwd, gp = gpar(
        fill = key$col, col = "transparent",
        alpha = key$alpha
      )
    ), row = 5, col = 2)
    if ("border" %in% key$plot.style) {
      key.gf <- placeGrob(frame = key.gf, rectGrob(gp = gpar(
        col = axis.line$col,
        lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha,
        fill = "transparent"
      )), row = 5, col = 2)
    }
    if ("ticks" %in% key$plot.style) {
      key.gf <- placeGrob(frame = key.gf, segmentsGrob(
        y0 = rep(
          0,
          length(labscat)
        ), x0 = labscat, y1 = rep(0.4, length(labscat)),
        x1 = labscat, vp = viewport(xscale = atrange), default.units = "native",
        gp = gpar(
          col = axis.line$col, lty = axis.line$lty,
          lwd = axis.line$lwd
        )
      ), row = 4, col = 2)
    }
    key.gf <- placeGrob(key.gf, labelsGrob, row = 3, col = 2)
    key.gf <- placeGrob(key.gf, keyGrob, row = 2, col = 2)
    key.gf <- placeGrob(key.gf, keyGrob2, row = 1, col = 2)
  }
  
  #####################
  # bottom scale
  # positions
  # adds ticks and borders if requested
  #####################
  else if (key$space == "bottom") {
    f.text <- f.text[as.character(f.text) != ""]
    f.text <- if (is.character(f.text)) {
      quickText(paste(f.text, collapse = "  "), f.auto.text)
    } else {
      as.expression(parse(text = paste(f.text, collapse = "~~")))
    }
    h.text <- h.text[as.character(h.text) != ""]
    h.text <- if (is.character(h.text)) {
      quickText(paste(h.text, collapse = "  "), f.auto.text)
    } else {
      as.expression(parse(text = paste(h.text, collapse = "~~")))
    }
    temp <- c(0.3, 1, 0, 0)
    if (sum(f.slots[1:2]) > 0 & sum(h.slots[2:3]) == 0) {
      temp[2:4] <- c(1, 1, 1.5)
    }
    if (sum(f.slots[1:2]) == 0 & sum(h.slots[2:3]) > 0) {
      temp[2:4] <- c(1, 1.5, 1)
    }
    if (sum(f.slots[1:2]) > 0 & sum(h.slots[2:3]) > 0) {
      temp[2:4] <- c(1, 1.5, 1.5)
    }
    if (sum(f.slots[1:2]) == 0 & sum(h.slots[2:3]) == 0) {
      temp[2:4] <- c(1, 1, 1)
    }
    if ("ticks" %in% key$plot.style) {
      temp[1] <- 0.6
    }
    labelsGrob <- textGrob(
      label = labels, y = rep(0, length(at)),
      x = at, vp = viewport(xscale = atrange), default.units = "native",
      check.overlap = check.overlap, just = if (rot == 0) {
        c("center", "bottom")
      } else {
        c("left", "center")
      }, rot = rot, gp = gpar(
        col = col,
        cex = cex, fontfamily = fontfamily, fontface = chooseFace(
          fontface,
          font
        )
      )
    )
    keyGrob <- textGrob(
      label = h.text, y = c(0),
      x = c(0.5), vp = viewport(xscale = c(0, 1)), default.units = "native",
      check.overlap = check.overlap, just = if (rot == 0) {
        c("center", "bottom")
      } else {
        c("left", "center")
      }, rot = rot, gp = gpar(
        col = col,
        cex = cex, fontfamily = fontfamily, fontface = chooseFace(
          fontface,
          font
        )
      )
    )
    keyGrob2 <- textGrob(
      label = f.text, y = c(0),
      x = c(0.5), vp = viewport(xscale = c(0, 1)), default.units = "native",
      check.overlap = check.overlap, just = if (rot == 0) {
        c("center", "bottom")
      } else {
        c("left", "center")
      }, rot = rot, gp = gpar(
        col = col,
        cex = cex, fontfamily = fontfamily, fontface = chooseFace(
          fontface,
          font
        )
      )
    )
    widths.x <- c((1 - key$height) / 2, key$height, (1 - key$height) / 2)
    widths.units <- rep("null", 3)
    heights.x <- c(0.6 * key$width, temp[1], temp[2], temp[3], temp[4])
    heights.units <- c("lines", "lines", "grobheight", "grobheight", "grobheight")
    heights.data <- list(NULL, NULL, labelsGrob, keyGrob, keyGrob2)
    key.layout <- grid.layout(nrow = 5, ncol = 3, heights = unit(
      heights.x,
      heights.units,
      data = heights.data
    ), widths = unit(
      widths.x,
      widths.units
    ), respect = TRUE)
    key.gf <- frameGrob(layout = key.layout, vp = vp)
    key.gf <- placeGrob(key.gf, rectGrob(
      y = rep(0.5, length(reccentre)),
      x = reccentre, default.units = "native", vp = viewport(xscale = atrange),
      width = recdim, height = recwd, gp = gpar(
        fill = key$col, col = "transparent",
        alpha = key$alpha
      )
    ), row = 1, col = 2)
    if ("ticks" %in% key$plot.style) {
      key.gf <- placeGrob(frame = key.gf, segmentsGrob(
        y0 = rep(
          1,
          length(labscat)
        ), x0 = labscat, y1 = rep(0.6, length(labscat)),
        x1 = labscat, vp = viewport(xscale = atrange), default.units = "native",
        gp = gpar(
          col = axis.line$col, lty = axis.line$lty,
          lwd = axis.line$lwd
        )
      ), row = 2, col = 2)
    }
    if ("border" %in% key$plot.style) {
      key.gf <- placeGrob(frame = key.gf, rectGrob(gp = gpar(
        col = axis.line$col,
        lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha,
        fill = "transparent"
      )), row = 1, col = 2)
    }
    key.gf <- placeGrob(key.gf, labelsGrob, row = 3, col = 2)
    key.gf <- placeGrob(key.gf, keyGrob, row = 4, col = 2)
    key.gf <- placeGrob(key.gf, keyGrob2, row = 5, col = 2)
  }
  
  ##############
  # outputs
  ##############
  if (draw) {
    grid.draw(key.gf)
  }
  key.gf
}


#------------------------------------------------------------------------------


# original code taken from https://github.com/cran/openair/blob/master/R/windRose.R
windRose <- function(mydata, ws = "ws", wd = "wd", ws2 = NA, wd2 = NA,
                     ws.int = 2, angle = 30, type = "default", bias.corr = TRUE,
                     cols = "default", grid.line = NULL, width = 1, seg = NULL,
                     auto.text = TRUE, breaks = 4, offset = 10, normalise = FALSE,
                     max.freq = NULL, paddle = TRUE, key.header = NULL,
                     key.footer = "(m/s)", key.position = "bottom", calm,
                     key = TRUE, dig.lab = 5, statistic = "prop.count",
                     pollutant = NULL, annotate = TRUE, angle.scale = 315, border = NA,
                     ...) {
  library(lattice)
  ## these are pre-defined type that need a field "date"; used by cutData
  dateTypes <- c(
    "year", "hour", "month", "season", "weekday", "weekend",
    "monthyear", "gmtbst", "bstgmt", "dst", "daylight",
    "seasonyear", "yearseason"
  )
  # function to check variables are numeric, if not force with warning
  checkNum <- function(mydata, vars) {
    for (i in seq_along(vars)) {
      if (!is.numeric(mydata[[vars[i]]])) {
        mydata[[vars[i]]] <- as.numeric(as.character(mydata[[vars[i]]]))
        
        warning(
          paste(vars[i], "is not numeric, forcing to numeric..."),
          call. = FALSE
        )
      }
    }
    
    return(mydata)
  }

  if (is.null(seg)) seg <- 0.9
  
  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    ## other local colours
    calm.col <- "black"
  } else {
    calm.col <- "forestgreen"
  }
  
  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  
  ## reset graphic parameters
  on.exit(trellis.par.set(
    
    fontsize = current.font
  ))
  
  # make sure ws and wd and numeric
  mydata <- checkNum(mydata, vars = c(ws, wd))
  
  if (360 / angle != round(360 / angle)) {
    warning(
      "In windRose(...):\n  angle will produce some spoke overlap",
      "\n  suggest one of: 5, 6, 8, 9, 10, 12, 15, 30, 45, etc.",
      call. = FALSE
    )
  }
  if (angle < 3) {
    warning(
      "In windRose(...):\n  angle too small",
      "\n  enforcing 'angle = 3'",
      call. = FALSE
    )
    angle <- 3
  }
  
  ## extra args setup
  extra <- list(...)
  
  ## label controls
  extra$xlab <- if ("xlab" %in% names(extra)) {
    quickText(extra$xlab, auto.text)
  } else {
    quickText("", auto.text)
  }
  extra$ylab <- if ("ylab" %in% names(extra)) {
    quickText(extra$ylab, auto.text)
  } else {
    quickText("", auto.text)
  }
  extra$main <- if ("main" %in% names(extra)) {
    quickText(extra$main, auto.text)
  } else {
    quickText("", auto.text)
  }
  
  if ("fontsize" %in% names(extra)) {
    trellis.par.set(fontsize = list(text = extra$fontsize))
  }
  
  
  ## preset statitistics
  
  if (is.character(statistic)) {
    ## allowed cases
    ok.stat <- c("prop.count", "prop.mean", "abs.count", "frequency")
    
    if (!is.character(statistic) || !statistic[1] %in% ok.stat) {
      warning(
        "In windRose(...):\n  statistic unrecognised",
        "\n  enforcing statistic = 'prop.count'",
        call. = FALSE
      )
      statistic <- "prop.count"
    }
    
    if (statistic == "prop.count") {
      stat.fun <- length
      stat.unit <- "%"
      stat.scale <- "all"
      stat.lab <- "Frequency of counts by wind direction (%)"
      stat.fun2 <- function(x) format(mean(x, na.rm = TRUE), digits = 5)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }
    
    if (statistic == "prop.mean") {
      stat.fun <- function(x) sum(x, na.rm = TRUE)
      stat.unit <- "%"
      stat.scale <- "panel"
      stat.lab <- "Proportion contribution to the mean (%)"
      stat.fun2 <- function(x) format(mean(x, na.rm = TRUE), digits = 5)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }
    
    if (statistic == "abs.count" | statistic == "frequency") {
      stat.fun <- length
      stat.unit <- ""
      stat.scale <- "none"
      stat.lab <- "Count by wind direction"
      stat.fun2 <- function(x) round(length(x), 0)
      stat.lab2 <- "count"
      stat.labcalm <- function(x) round(x, 0)
    }
  }
  
  if (is.list(statistic)) {
    
    ## IN DEVELOPMENT
    
    ## this section has no testing/protection
    ## but allows users to supply a function
    ## scale it by total data or panel
    ## convert proportions to percentage
    ## label it
    
    stat.fun <- statistic$fun
    stat.unit <- statistic$unit
    stat.scale <- statistic$scale
    stat.lab <- statistic$lab
    stat.fun2 <- statistic$fun2
    stat.lab2 <- statistic$lab2
    stat.labcalm <- statistic$labcalm
  }
  
  ## variables we need
  vars <- c(wd, ws)
  
  diff <- FALSE ## i.e. not two sets of ws/wd
  rm.neg <- TRUE ## will remove negative ws in check.prep
  
  ## case where two met data sets are to be compared
  if (!is.na(ws2) & !is.na(wd2)) {
    vars <- c(vars, ws2, wd2)
    diff <- TRUE
    rm.neg <- FALSE
    mydata$ws <- mydata[[ws2]] - mydata[[ws]]
    mydata$wd <- mydata[[wd2]] - mydata[[wd]]
    
    ## fix negative wd
    id <- which(mydata$wd < 0)
    if (length(id) > 0) mydata$wd[id] <- mydata$wd[id] + 360
    
    pollutant <- "ws"
    key.footer <- "ws"
    wd <- "wd"
    ws <- "ws"
    vars <- c("ws", "wd")
    if (missing(angle)) angle <- 10
    if (missing(offset)) offset <- 20
    ## set the breaks to cover all the data
    if (is.na(breaks[1])) {
      max.br <- max(ceiling(abs(c(
        min(mydata$ws, na.rm = TRUE),
        max(mydata$ws, na.rm = TRUE)
      ))))
      breaks <- c(-1 * max.br, 0, max.br)
    }
    
    if (missing(cols)) cols <- c("lightskyblue", "tomato")
    seg <- 1
  }
  
  if (any(type %in% dateTypes)) vars <- c(vars, "date")
  
  if (!is.null(pollutant)) vars <- c(vars, pollutant)
  
  mydata <- cutData(mydata, type, ...)
  
  
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE, remove.neg = rm.neg)
  
  # original data to use later
  mydata_orig <- mydata
  
  # remove lines where ws is missing
  # wd can be NA and ws 0 (calm)
  id <- which(is.na(mydata[[ws]]))
  
  if (length(id) > 0) {
    mydata <- mydata[-id, ]
  }
  
  if (is.null(pollutant)) pollutant <- ws
  
  mydata$x <- mydata[[pollutant]]
  
  mydata[[wd]] <- angle * ceiling(mydata[[wd]] / angle - 0.5)
  mydata[[wd]][mydata[[wd]] == 0] <- 360
  
  ## flag calms as negatives
  mydata[[wd]][mydata[, ws] == 0] <- -999 ## set wd to flag where there are calms
  ## do after rounding or -999 changes
  
  if (length(breaks) == 1) breaks <- 0:(breaks - 1) * ws.int
  
  xmax <- max(mydata$x, na.rm = TRUE)
  if (max(breaks) < xmax) {
    breaks <- c(breaks, xmax)
  }
  
  if (min(breaks) > min(mydata$x, na.rm = TRUE)) {
    warning("Some values are below minimum break.")
  }
  
  breaks <- unique(breaks)
  mydata$x <- cut(
    mydata$x,
    breaks = breaks, include.lowest = FALSE,
    dig.lab = dig.lab
  )
  
  ## clean up cut intervals
  labs <- gsub("[(]|[)]|[[]|[]]", "", levels(mydata$x))
  labs <- gsub("[,]", " - ", labs)
  labs[grep(xmax, labs)] <- paste0(breaks[length(breaks) - 1], " +")
  
  
  ## statistic handling
  
  prepare.grid <- function(mydata) {
    
    ## these are all calms...
    if (all(is.na(mydata$x))) {
      weights <- data_frame(
        Interval1 = NA, wd = NA,
        calm = 100, panel.fun = NA, mean.wd = NA, freqs = NA
      )
    } else {
      levels(mydata$x) <- c(paste("Interval", 1:length(labs), sep = ""))
      
      all <- stat.fun(mydata[[wd]])
      #calm <- mydata[mydata[[wd]] == -999, ][[pollutant]]
      # use calm argument
      
      #calm <- stat.fun(calm)
      
      weights <- tapply(
        mydata[[pollutant]], list(mydata[[wd]], mydata$x),
        stat.fun
      )
      
      freqs <- tapply(mydata[[pollutant]], mydata[[wd]], length)
      
      ## scaling
      if (stat.scale == "all") {
        #calm <- calm / all
        weights <- weights / all
      }
      
      if (stat.scale == "panel") {
        temp <- stat.fun(stat.fun(weights)) + calm
        calm <- calm / temp
        weights <- weights / temp
      }
      
      weights[is.na(weights)] <- 0
      weights <- t(apply(weights, 1, cumsum))
      
      if (stat.scale == "all" | stat.scale == "panel") {
        weights <- weights * 100
        calm <- calm * 100
      }
      
      panel.fun <- stat.fun2(mydata[[pollutant]])
      
      ## calculate mean wd - useful for cases comparing two met data sets
      u <- mean(sin(2 * pi * mydata[[wd]] / 360), na.rm = TRUE)
      v <- mean(cos(2 * pi * mydata[[wd]] / 360), na.rm = TRUE)
      mean.wd <- atan2(u, v) * 360 / 2 / pi
      
      if (all(is.na(mean.wd))) {
        mean.wd <- NA
      } else {
        if (mean.wd < 0) mean.wd <- mean.wd + 360
        ## show as a negative (bias)
        if (mean.wd > 180) mean.wd <- mean.wd - 360
      }
      
      weights <- bind_cols(
        as_data_frame(weights),
        data_frame(
          wd = as.numeric(row.names(weights)),
          calm = calm, panel.fun = panel.fun,
          mean.wd = mean.wd, freqs = freqs
        )
      )
    }
    
    weights
  }
  
  if (paddle) {
    poly <- function(wd, len1, len2, width, colour, x.off = 0, y.off = 0) {
      theta <- wd * pi / 180
      len1 <- len1 + off.set
      len2 <- len2 + off.set
      x1 <- len1 * sin(theta) - width * cos(theta) + x.off
      x2 <- len1 * sin(theta) + width * cos(theta) + x.off
      x3 <- len2 * sin(theta) - width * cos(theta) + x.off
      x4 <- len2 * sin(theta) + width * cos(theta) + x.off
      y1 <- len1 * cos(theta) + width * sin(theta) + y.off
      y2 <- len1 * cos(theta) - width * sin(theta) + y.off
      y3 <- len2 * cos(theta) + width * sin(theta) + y.off
      y4 <- len2 * cos(theta) - width * sin(theta) + y.off
      lpolygon(
        c(x1, x2, x4, x3), c(y1, y2, y4, y3),
        col = colour,
        border = border
      )
    }
  } else {
    poly <- function(wd, len1, len2, width, colour, x.off = 0,
                     y.off = 0) {
      len1 <- len1 + off.set
      len2 <- len2 + off.set
      
      theta <- seq(
        (wd - seg * angle / 2), (wd + seg * angle / 2),
        length.out = (angle - 2) * 10
      )
      theta <- ifelse(theta < 1, 360 - theta, theta)
      theta <- theta * pi / 180
      x1 <- len1 * sin(theta) + x.off
      x2 <- rev(len2 * sin(theta) + x.off)
      y1 <- len1 * cos(theta) + x.off
      y2 <- rev(len2 * cos(theta) + x.off)
      lpolygon(c(x1, x2), c(y1, y2), col = colour, border = border)
    }
  }
  
  
  results <- group_by(mydata, UQS(syms(type))) %>%
    do(prepare.grid(.))
  
  ## format
  results$calm <- stat.labcalm(results$calm)
  results$mean.wd <- stat.labcalm(results$mean.wd)
  
  # function to correct bias
  corr_bias <- function(results) {
    
    # check to see if data for this type combination are rounded to 10 degrees
    wd_select <- inner_join(mydata_orig, results[1, type], by = type)
    if (!all(wd_select[[wd]] %% 10 == 0, na.rm = TRUE)) return(results)
    
    wds <- seq(10, 360, 10)
    tmp <- angle * ceiling(wds / angle - 0.5)
    id <- which(tmp == 0)
    if (length(id > 0)) tmp[id] <- 360
    tmp <- table(tmp) ## number of sectors spanned
    vars <- grep("Interval[1-9]", names(results)) ## the frequencies, without any calms
    results[results[["wd"]] != -999, vars] <-
      results[results[["wd"]] != -999, vars] * mean(tmp) / tmp
    return(results)
  }
  
  ## correction for bias when angle does not divide exactly into 360
  if (bias.corr) {
    results <- group_by(results, UQS(syms(type))) %>%
      do(corr_bias(.))
  }
  
  
  
  ## proper names of labelling###########################################
  strip.dat <- strip.fun(results, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]
  
  if (length(labs) < length(cols)) {
    col <- cols[1:length(labs)]
  } else {
    col <- openColours(cols, length(labs))
  }
  
  ## normalise by sector
  
  if (normalise) {
    vars <- grep("Interval[1-9]", names(results))
    
    ## original frequencies, so we can plot the wind frequency line
    results$freq <- results[[max(vars)]]
    
    results$freq <- ave(results$freq, results[type], FUN = function(x) x / sum(x))
    
    ## scale by maximum frequency
    results$norm <- results$freq / max(results$freq)
    
    ## normalise
    results[, vars] <- results[, vars] / results[[max(vars)]]
    
    stat.lab <- "Normalised by wind sector"
    stat.unit <- ""
  }
  
  if (is.null(max.freq)) {
    max.freq <- max(
      results[results$wd != -999, grep("Interval", names(results))],
      na.rm = TRUE
    )
  } else {
    max.freq <- max.freq
  }
  
  off.set <- max.freq * (offset / 100)
  box.widths <- seq(
    0.002 ^ 0.25, 0.016 ^ 0.25,
    length.out = length(labs)
  ) ^ 4
  box.widths <- box.widths * max.freq * angle / 5
  
  ## key, colorkey, legend
  legend <- list(
    col = col, space = key.position, auto.text = auto.text,
    labels = labs, footer = key.footer, header = key.header,
    height = 0.75, width = 1.5, fit = "scale",
    plot.style = if (paddle) "paddle" else "other"
  )
  
  #legend <- makeOpenKeyLegend(key, legend, "windRose")
  
  
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("Interval1 ~ wd | ", temp, sep = ""))
  
  mymax <- 2 * max.freq
  
  # check to see if grid.line is a list or not and set grid line properties
  grid.value <- NULL
  
  if (is.list(grid.line)) {
    if (is.null(grid.line[["value"]])) {
      grid.value <- NULL
    } else {
      grid.value <- grid.line[["value"]]
    }
    
    if (is.null(grid.line[["lty"]])) {
      grid.lty <- 1
    } else {
      grid.lty <- grid.line[["lty"]]
    }
    
    if (is.null(grid.line[["col"]])) {
      grid.col <- "grey85"
    } else {
      grid.col <- grid.line[["col"]]
    }
  } else {
    grid.value <- grid.line
    grid.lty <- 1
    grid.col <- "grey85"
  }
  
  myby <- if (is.null(grid.value)) pretty(c(0, mymax), 10)[2] else grid.value
  
  if (myby / mymax > 0.9) myby <- mymax * 0.9
  
  is_annotated <- !(annotate %in% c(FALSE, NA, NaN)) &&   !is.null(annotate)
  if (is_annotated) sub <- stat.lab else sub <- NULL
  
  xy.args <- list(
    x = myform,
    xlim = 1.03 * c(-max.freq - off.set, max.freq + off.set),
    ylim = 1.03 * c(-max.freq - off.set, max.freq + off.set),
    data = results,
    type = "n",
    #sub = sub,
    strip = strip,
    strip.left = strip.left,
    as.table = TRUE,
    aspect = 1,
    par.strip.text = list(cex = 0.8),
    scales = list(draw = FALSE),
    
    panel = function(x, y, subscripts, ...) {
      panel.xyplot(x, y, ...)
      angles <- seq(0, 2 * pi, length = 360)
      sapply(
        seq(off.set, mymax, by = myby),
        function(x) llines(
          x * sin(angles), x * cos(angles),
          col = grid.col, lwd = 1,
          lty = grid.lty
        )
      )
      
      dat <- results[subscripts, ] ## subset of data
      dat <- filter(dat, wd <= 360, wd >= 0)
      
      upper <- max.freq + off.set
      
      ## add axis lines
      lsegments(-upper, 0, upper, 0)
      lsegments(0, -upper, 0, upper)
      
      ltext(upper * -1 * 0.95, 0.07 * upper, "W", cex = 0.7, 
            fontfamily = "serif")
      ltext(0.07 * upper, upper * -1 * 0.95, "S", cex = 0.7,
            fontfamily = "serif")
      ltext(0.07 * upper, upper * 0.95, "N", cex = 0.7,
            fontfamily = "serif")
      ltext(upper * 0.95, 0.07 * upper, "E", cex = 0.7,
            fontfamily = "serif")
      
      if (nrow(dat) > 0) {
        dat$Interval0 <- 0 ## make a lower bound to refer to
        
        for (i in 1:nrow(dat)) { ## go through wind angles 30, 60, ...
          
          for (j in seq_along(labs)) { ## go through paddles x1, x2, ...
            
            tmp <- paste(
              "poly(dat$wd[i], dat$Interval", j - 1,
              "[i], dat$Interval", j, "[i], width * box.widths[",
              j, "], col[", j, "])",
              sep = ""
            )
            
            
            eval(parse(text = tmp))
          }
        }
      }
      
      if (normalise) {
        panel.wdprob(dat, seg, angle, off.set)
      }
      
      ltext(
        seq((myby + off.set), mymax, myby) * sin(pi * angle.scale / 180),
        seq((myby + off.set), mymax, myby) * cos(pi * angle.scale / 180),
        paste(seq(myby, mymax, by = myby), stat.unit, sep = ""),
        cex = 0.7,
        fontfamily = "serif"
      )
      
      ## annotations e.g. calms, means etc
      if (is_annotated) { ## don't add calms for prop.mean for now...
        if (annotate == "TRUE") {
          if (!diff) annotate <- c("statistic" , "calm")
          if (diff) annotate <- c("mean_ws" , "mean_wd")
        }
        
        annotations_to_place <- NULL
        for(annotate.index in annotate){
          annotations_to_place <- paste(
            annotations_to_place, 
            switch(
              annotate.index,
              statistic = "", #paste(stat.lab2, " = ", dat$panel.fun[1]),
              calm = paste("calm = ", dat$calm[1], stat.unit),
              mean_ws = paste("mean ws = ", round(as.numeric(dat$panel.fun[1]), 1)),
              mean_wd = paste("mean wd = ", round(dat$mean.wd[1], 1)),
              annotate.index
            ), sep = "\n"
          )
        }
        
        ltext(
          max.freq + off.set, -max.freq - off.set,
          label = annotations_to_place  ,
          adj = c(1, 0), cex = 0.75, col = calm.col,
          fontfamily = "serif"
        )        
        
      }
    }#, legend = legend
  )
  
  ## reset for extra
  xy.args <- listUpdate(xy.args, extra)
  
  ## plot
  plt <- do.call(xyplot, xy.args)
  
  
  ## output ################################################################################
  
  if (length(type) == 1) {
    plot(plt, mar = c(0, 0, 0, 0))
  } else {
    plt <- useOuterStrips(plt, strip = strip, strip.left = strip.left)
    plot(plt, mar = c(0, 0, 0, 0))
  }
  
  
  newdata <- results
  
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}

## adds a line showing probability wind direction is from a particular sector
## used when normalise = TRUE

panel.wdprob <- function(dat, seg, angle, off.set) {
  len1 <- off.set
  
  x.off <- 0
  y.off <- 0
  
  makeline <- function(i, dat) {
    theta <- seq(
      (dat$wd[i] - seg * angle / 2), (dat$wd[i] + seg * angle / 2),
      length.out = (angle - 2) * 10
    )
    theta <- ifelse(theta < 1, 360 - theta, theta)
    theta <- theta * pi / 180
    x1 <- len1 * sin(theta) + x.off
    x2 <- rev((dat$norm[i] + off.set) * sin(theta) + x.off)
    y1 <- len1 * cos(theta) + x.off
    y2 <- rev((dat$norm[i] + off.set) * cos(theta) + x.off)
    lpolygon(c(x1, x2), c(y1, y2), col = "transparent", border = "black", lwd = 2)
  }
  
  lapply(1:nrow(dat), makeline, dat)
}
