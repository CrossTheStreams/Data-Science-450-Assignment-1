# Helper method for fetching packages.
pkgTest <- function(x) {
  if (!require(x,character.only = TRUE)) {
        install.packages(x,dep=TRUE)
  if(!require(x,character.only = TRUE)) stop("Package not found")
   
    }
}

pkgTest('urca')
pkgTest('lubridate')
pkgTest('TTR')
# This library is especially apt at irregular time series data (e.g. financial data like this)
pkgTest('zoo')
pkgTest('tseries')
# lattice graphics
pkgTest('lattice')
# forecast package
pkgTest('forecast')

