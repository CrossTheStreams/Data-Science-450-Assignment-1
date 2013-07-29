# convert time series data to additive change over the time series
to.additive <- function (data.set) { 
  n <- nrow(data.set)
  ret <- data.set
  for (i in 2:n) {
    ret[i,] <- (ret[(i-1),] + as.vector(data.set[i,]))
  } 
  return(ret) 
}

plotForecastErrors <- function(forecasterrors,label) {
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
  # make a red histogram of the forecast errors, with the normally distributed data overlaid: 
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins,main=label)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd 
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors: 
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

ensure.regularity <- function(ts) {
  return(as.ts(na.approx(as.ts(ts))))
}

res <- function(forecast,test) {
  return((as.vector(forecast))-(as.vector(test)))
}

mae <- function(forecast,test) {
  return(abs((as.vector(forecast))-(as.vector(test)))) 
}

