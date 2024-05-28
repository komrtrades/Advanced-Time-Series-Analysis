rm(list = ls())
library(quantmod)
library(dplyr)
library(xts)
library(zoo)
library(tseries)
library(forecast)

yield_curves <- list()

for (i in 1:30) {
  symbol <- paste0("DGS", i) 
  tryCatch({
    getSymbols(symbol, src = "FRED")
    
    yield_curve <- get(symbol)
    
    yield_curve <- na.locf(yield_curve, na.rm = FALSE)
        
    yield_curves[[i]] <- yield_curve
    assign(paste0("y", i), yield_curve)

  }, error = function(e) {
    # message(paste("Error fetching data for symbol:", symbol))
  })
}

getSymbols("VIXCLS", src = "FRED")
getSymbols("FEDFUNDS", src = "FRED")
getSymbols("DEXUSEU", src = "FRED")

yc_matrix <- do.call(cbind, yield_curves)
yc_matrix <- cbind(yc_matrix, VIXCLS)
yc_matrix <- cbind(yc_matrix, FEDFUNDS)
yc_matrix <- cbind(yc_matrix, DEXUSEU)

yc_matrix <- na.locf(yc_matrix, na.rm = TRUE)
yc_matrix <- yc_matrix["2000-01-01/"]

yc_diff_matrix <- diff(yc_matrix)
yc_diff_matrix <- yc_diff_matrix[-1, ]

yc_logdiff_matrix <- diff(log(yc_matrix))
yc_logdiff_matrix <- yc_logdiff_matrix[-1, ]

yc_matrix <- yc_matrix[-1,]

rm(list = ls(pattern = "DGS"))
rm(list = ls(pattern = "y[0-9]+"))
rm(symbol, yield_curve, VIXCLS, FEDFUNDS, DEXUSEU)