# Andrew Hautau
# Assignment 1 – Time Series Analysis 
# Deriving Knowledge From Data At Scale

# Building a predictive model of the ISE.
source("lib/helpers.r")

pkgTest('lubridate')
pkgTest('TTR')

# This library is especially apt at irregular time series data (e.g. financial data like this)
pkgTest('zoo')
# lattice graphics
pkgTest('lattice')
# forecast package
pkgTest('forecast')

# The data we're going to model is stock exchange daily changes in the 
# Istanbul Stock Exchange and other indexes between 1/5/2009 and 2/22/2011
data <- read.zoo('data/data_akbilgic_formatted.csv', sep=',', header=T, format='%m/%d/%y')

# We only care about dollar amounts
data <- data[,2:9]

cum.data <- to.cumulative(data)

# Daily changes of the Istanbul Stock Exchange, our 'stationary' time series
ise <- data[,1]
# Cumulative time series of the ISE
cum.ise <- cum.data[,1]

# Our time series is irregular, since it is only following business days.
# Whenever our methods need continuous data, let's ignore NAs, since interpolation seems to be an invalid strategy.
vector.ise <- as.vector(ise)
vector.cum.ise <- as.vector(cum.ise)

xyplot(ise)
xyplot(cum.ise)

train.vector.ise <- vector.ise[1:200]
plot(train.vector.ise,col='black',type='l')
forecast <- HoltWinters(train.vector.ise,beta=F,gamma=F)
train.ise.forecast <- forecast.HoltWinters(forecast, h=30)

plot.forecast(train.ise.forecast)
par(new=T)
plot(vector.ise[201:230],col='red',type='l')

cum.forecast <- HoltWinters(vector.cum.ise,beta=F,gamma=F)
cum.ise.forecast <- forecast.HoltWinters(cum.forecast, h=30)
plot.forecast(cum.ise.forecast)

plot.ts(cum.ise.forecast$residuals)

plotForecastErrors(cum.ise.forecast$residuals)


# We find no evidence of auto-correlation in the residuals here.
acf(cum.ise.forecast$residuals, lag.max=30)

cum.box.test <- Box.test(cum.ise.forecast$residuals, lag=30, type="Ljung-Box")    
print(cum.box.test)

# autocorrelation of the stationary time series
autocorrelation <- acf(vector.ise,lag.max=30,plot=F) 
print(autocorrelation)
# We have autocorrelation above zero up to lag 2
acf(vector.ise,lag.max=30)

# partial autocorrelation of the stationary time series
part.autocorrelation <- pacf(vector.ise,lag.max=30)
print(part.autocorrelation)
# We have partial autocorrelation above zero up to lag 1
pacf(vector.ise,lag.max=30)

arma.fit <- auto.arima(vector.ise)
forecast.arma.fit <- forecast(arma.fit,h=30)
plot(forecast.arma.fit)


# Attempt to fit a linear regression on cumulative series
cum.ise.df <- data.frame(index = as.integer(index(cum.ise)),cum.ise = cum.ise)  
lmfit <- lm(cum.ise ~ index, cum.ise.df) 
plot(lmfit)

sum(arma.fit$residuals)
sum(lmfit$residuals)

sum(lmfit$residuals)
plot(arma.fit$residuals, type='l')  
par(new=T) 
plot(lmfit$residuals, col='red', type='l')



