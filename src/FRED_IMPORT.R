rm(list = ls())
library(quantmod)
library(dplyr)
library(xts)
library(zoo)
library(tseries)
library(forecast)
library(png)
library(grid)
library(gridExtra)

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
  })
}

yc_matrix <- do.call(cbind, yield_curves)

yc_matrix <- na.locf(yc_matrix, na.rm = TRUE)
yc_matrix <- yc_matrix["1978-01-01/2024-05-31"]


yc_diff_matrix <- diff(yc_matrix)
yc_diff_matrix <- yc_diff_matrix[-1, ]

yc_matrix <- yc_matrix[-1,]
y1 <- yc_matrix[,1]

rm(list = ls(pattern = "DGS"))
rm(list = ls(pattern = "y[0-9]+"))
rm(symbol, yield_curve)