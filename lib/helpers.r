# Helper method for fetching packages.
pkgTest <- function(x) {
  if (!require(x,character.only = TRUE)) {
        install.packages(x,dep=TRUE)
  if(!require(x,character.only = TRUE)) stop("Package not found")
   
    }
}

# Plot a comparison with a simple moving average of time series
plot.sma <- function (data.set,order.interval) {

  sma.data.set <- SMA(data.set, n=order.interval)

  plot(data.set,type='l')

  par(new=T)

  plot(sma.data.set, col="red", type='l') 

}

# convert time series data to cumulative change over the time series
to.cumulative <- function (data.set) { 
  n <- nrow(data.set)
  ret <- data.set
  for (i in 2:n) {
    ret[i,] <- (ret[(i-1),] + as.vector(data.set[i,]))
  } 
  return(ret) 
}

plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd 
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid: mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd 
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors: 
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
