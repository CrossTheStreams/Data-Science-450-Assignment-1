## Cross validation of:
 # Linear Regression
 # ARIMA
 # ETS 
 # Holts Winters

# Iterate models on training sub-sets of the ISE time series for cross-validation.
# Tests forecasts against test subsets and plots mean average error of horizons.
# Inspiration from: http://robjhyndman.com/hyndsight/tscvexample/

cross.validation <- function(time.series,k,horizon) {

  par("ask"=T)
  cv.list <- cv.iteration(reg.ise,k=k,test.size=30)
  plot.cross.validation(cv.list$mae)
  slide.forecast.errors(cv.list) 
  par("ask"=F)

}


cv.iteration <- function(time.series,k,test.size){

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


plot.cross.validation <- function (mae.list,exclude.mae.list.item=c(),y.max=0.05) {

  preserve <- mae.list
  
  test.size <- nrow(as.data.frame(mae.list[1][1]))

  if (length(exclude.mae.list.item) > 0) {
    mae.list[exclude.mae.list.item] <- NULL
  }
  plot(1:test.size, type="l", xlab="horizon", ylab="MAE", ylim=c(0,y.max))
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
  plotForecastErrors(cv.list$residuals$lm.res,label="Residual Distribution for Linear Regression")
  plotForecastErrors(cv.list$residuals$arima.res,label="Residual Distribution for ARIMA")
  plotForecastErrors(cv.list$residuals$ets.res,label="Residual Distribution for ETS")
  plotForecastErrors(cv.list$residuals$hw.res,label="Residual Distribution for Holts Winters")
}


