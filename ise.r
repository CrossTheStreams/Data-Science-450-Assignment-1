# Andrew Hautau
# Assignment 1 – Time Series Analysis 
# Deriving Knowledge From Data At Scale

# Ensure R packages.
source("lib/packages.r")
# Building a predictive model of the ISE.
source("lib/helpers.r")
source("lib/cross_validation.r")

# The data we're going to model is stock exchange daily changes in the 
# Istanbul Stock Exchange and other indexes between 1/5/2009 and 2/22/2011
data <- read.zoo('data/data_akbilgic_formatted.csv', sep=',', header=T, format='%m/%d/%y')

# We only care about dollar amounts.
data <- data[,2:9]
# 'Additive' time series data. Source is 'stationary'.
add.data <- to.additive(data)
# Daily changes of the Istanbul Stock Exchange, our 'stationary' time series
ise <- data[,1]
# 'Additive' time series of the ISE
add.ise <- add.data[,1]

xyplot(ise)
xyplot(add.ise)

# For when we need to avoid irregularity in the time series.
reg.ise <- ensure.regularity(ise)
reg.add.ise <- ensure.regularity(add.ise)

stat.kpss.test <- ur.kpss(reg.ise) 
# We have autocorrelation of some significance up to three points of lag, but this is fairly stationary data.
summary(stat.kpss.test)
plot(stat.kpss.test)
lag.plot(reg.ise)

# For contrast, the additive ISE time series.
add.stat.kpss.test <- ur.kpss(reg.add.ise) 
# Extreme autocorrelation!
plot(add.stat.kpss.test)
# Very non-stationary!
lag.plot(reg.add.ise)

# Cross validation of forecasting models for stationary ISE data...

# Residuals tend to follow normal distribution.
# At K=60, a winner is not at all clear. There is way too much being used in our predictions!
cross.validation(reg.ise,k=60,horizon=30)

cross.validation(reg.ise,k=20,horizon=30)

cross.validation(reg.ise,k=10,horizon=30)

cross.validation(reg.ise,k=5,horizon=30)

# However, as K value decreases, models other than Linear Regression greatly increase density at near or at zero residual value.
cross.validation(reg.ise,k=4,horizon=30)


# Cross Validation of forecasting models for additive ISE data...
cross.validation(add.reg.ise,k=60,horizon=30)

cross.validation(add.reg.ise,k=20,horizon=30)

cross.validation(add.reg.ise,k=10,horizon=30)

cross.validation(add.reg.ise,k=5,horizon=30)

# Residuals tend to follow normal distributions in higher k values for all models.
# Linear Regression tends to have a fairly normal distribution in residuals, though.
cross.validation(add.reg.ise,k=4,horizon=30)
