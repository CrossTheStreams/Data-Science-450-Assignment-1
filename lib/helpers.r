# Plot a comparison with a simple moving average of time series
plot.sma <- function (data.set,order.interval) {

  sma.data.set <- SMA(data.set, n=order.interval)

  plot(data.set,type='l')

  par(new=T)

  plot(sma.data.set, col="red", type='l') 

}

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

## Cross validation of:
 # Linear Regression
 # ARIMA
 # ETS 
 # Holts Winters

# Iterate models on training sub-sets of the ISE time series for cross-validation.
# Tests forecasts against test subsets and plots mean average error of horizons.
# Inspiration from: http://robjhyndman.com/hyndsight/tscvexample/

cross.validation <- function(time.series,k,test.size){

  n <- length(time.series)
  iterations <- as.integer((n-test.size)/k) 
  mae1 <- mae2 <- mae3 <- mae4 <- matrix(NA,test.size,iterations)
  lm.res <- arima.res <- ets.res <- hw.res <- c()

  for(i in 1:iterations) {

    end <- (k * (i))
    start <- (end - k)

    print(paste("Iteration for ",as.character(start),"..",as.character(end)))


    # ensure regularity 
    xshort <- as.ts(time.series[start:end])
    xnext <- as.ts(time.series[(end+1):(end+test.size)])
   
    lm.fit <- tslm(xshort ~ trend)
    lm.fcast <- forecast.lm(lm.fit, h=test.size)

    arima.fit <- auto.arima(xshort)
    arima.fcast <- forecast(arima.fit,h=test.size)

    ets.fit <- ets(xshort)
    ets.fcast <- forecast(ets.fit, h=test.size)

    hw.fit <- HoltWinters(xshort,beta=F,gamma=F)
    hw.fcast <- forecast.HoltWinters(hw.fit, h=test.size)

    lm.res <- c(lm.res,res(lm.fcast[['mean']],xnext))
    arima.res <- c(arima.res,arima.fcast[['mean']],xnext)
    ets.res <- c(ets.res,ets.fcast[['mean']],xnext)
    hw.res <- c(hw.res,hw.fcast[['mean']],xnext)
 
    mae1[,i] <- mae(lm.fcast[['mean']],xnext)
    mae2[,i] <- mae(arima.fcast[['mean']],xnext)
    mae3[,i] <- mae(ets.fcast[['mean']],xnext)
    mae4[,i] <- mae(hw.fcast[['mean']],xnext)

  }

  return(list("residuals" = list("lm.res"=lm.res,"arima.res"=arima.res,"ets.res"=ets.res,"hw.res"=hw.res),
              "mae" = list("lm.mae"=mae1,"arima.mae"=mae2,"ets.mae"=mae3,"hw.mae"=mae4)))

}


plot.cross.validation <- function (mae.list,exclude.mae.list.item="") {

  preserve <- mae.list
  
  test.size <- nrow(as.data.frame(mae.list[1][1]))

  if (length(exclude.mae.list.item) > 1) {
    mae.list[exclude.mae.list.item] <- NULL
  }
  mae.max <- max(as.vector(c(mae.list$lm.mae,mae.list$arima.mae,mae.list$ets.mae,mae.list$hw.mae)))

  plot(1:test.size, type="l", xlab="horizon", ylab="MAE", ylim=c(0,mae.max))
  if (typeof(mae.list$lm.mae) != "NULL") {
    lines(1:test.size, rowMeans(mae.list$lm.mae), type="l",col=2)
  }
  if (typeof(mae.list$arima.mae) != "NULL") {
    lines(1:test.size, rowMeans(mae.list$arima.mae), type="l",col=3)
  }
  if (typeof(mae.list$ets.mae) != "NULL") {
    lines(1:test.size, rowMeans(mae.list$ets.mae), type="l",col=4)
  }
  if (typeof(mae.list$hw.mae) != "NULL") {
    lines(1:test.size, rowMeans(mae.list$hw.mae), type="l",col=5)
  }

  legend("topleft",legend=c("LM","ARIMA","ETS","Holt Winters"),col=2:5,lty=1)
  
  mae.list <- preserve
}

slide.forecast.errors <- function(cv.list) { 
  par("ask"=T)
  plotForecastErrors(cv.list$residuals$lm.res,label="Residual Distribution for Linear Regression")
  plotForecastErrors(cv.list$residuals$arima.res,label="Residual Distribution for ARIMA")
  plotForecastErrors(cv.list$residuals$ets.res,label="Residual Distribution for ETS")
  plotForecastErrors(cv.list$residuals$hw.res,label="Residual Distribution for Holts Winters")
  par("ask"=F)
}

