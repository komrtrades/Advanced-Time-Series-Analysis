# Fetch all available US yield curves from 1 to 30 years
source("src/FRED_IMPORT.R")

# raw_data <- read.csv("data/DGS1.csv")
# raw_data$DATE <- as.Date(raw_data$DATE, format = "%Y-%m-%d")
# raw_data$DGS1 <- as.numeric(raw_data$DGS1)
# y1 <- xts(raw_data$DGS1, order.by = raw_data$DATE)
# y1 <- na.locf(y1)

y1 <- yc_matrix[,1]


# # unconditional moments of the series 
# y1 <- as.vector(y1)
# e <- rnorm(length(y1))
# my1 = mean(y1)
# vy1 = var(y1)
# sk_y1 = mean(((y1-my1)/(vy1^0.5))^3)
# k_y1 = mean((y1-my1)^4)/(vy1^2)

# qqplot(y1-my1/(vy1^0.5),e)

# sy1 = sqrt(var(y1))
# dy1 = density(y1) 

# plot(dy1)

# # put in a table all the moments
# moments <- data.frame(
#   Moment = c("Mean", "Variance", "Skewness", "Kurtosis"),
#   Value = c(my1, vy1, sk_y1, k_y1)
# )
# print(moments)
# y1 <- yc_matrix[,1]
# ###########################################


dev.new()
plot.xts(y1, main = "Daily 1-year Interest Rates", xlab = "Time", ylab = "Price")
par(mfrow = c(1,2))
acf(y1, lag.max = 100, main = "ACF of 1-year Interest Rates")
pacf(y1, lag.max = 100, main = "PACF of 1-year Interest Rates")
adf.test(y1, alternative = "stationary")
# unit root

# dy plots
dy <- diff(y1)
dy <- dy[-1]
par(mfrow = c(1,1))
plot.xts(dy, main = "Differenced 1-year Interest Rates", xlab = "Time", ylab = "Price")
par(mfrow = c(1,2))
acf(dy, lag.max = 100, main = "ACF of Differenced 1-year Interest Rates")
pacf(dy, lag.max = 100, main = "PACF of Differenced 1-year Interest Rates")
adf.test(dy, alternative = "stationary")
# stationary


# # logdy plots
# logdy <- log(y1)
# logdy <- logdy[-1]
# par(mfrow = c(1,1))
# plot.xts(logdy, main = "Log Differenced 1-year Interest Rates", xlab = "Time", ylab = "Price")
# par(mfrow = c(1,2))
# acf(logdy, lag.max = 100, main = "ACF of Log Differenced 1-year Interest Rates")
# pacf(logdy, lag.max = 100, main = "PACF of Log Differenced 1-year Interest Rates")
# adf.test(logdy, alternative = "stationary")



n <- length(y1)
train <- 1:floor(0.7 * n)
# workflow: fit model on train, forecast on test, check residuals
# oppure: fit model su train, check residuals, forecast on test


# LLM AS BASE MODEL??
par(mfrow = c(1, 1))
fit.llm <- StructTS(y, type = "level")
fit$coef
ts.plot(y, fitted(fit), col = c("blue", "red"), lty = 1:2)
legend("topleft", legend = c("Observed", "Fitted level"), col = c("black", "red"), lty = 1:2)
checkresiduals(fit)


# fit ar(1) model
fit.ar1 <- arima(y1[train], order = c(1, 1, 0))
summary(fit.ar1)
coef(fit.ar1)[1]
# y1[t] = c + 0.0759*y1[t-1] + e[t]


# plot observed vs fitted prices
par(mfrow = c(1,1))
ts.plot(y1[train], fitted(fit.ar1), col = c("red", "blue", "blue"), lty = c(1, 2, 2), main = "Observed vs Fitted Prices: AR(1) Model", xlab = "Time", ylab = "Price")
legend("topleft", legend = c("Observed", "Fitted"), col = c("red", "blue"), lty = c(1, 2), bty = "n")
plot(density(residuals(fit.ar1)), main = "Density of Residuals")
norm <- dnorm(seq(-1, 1, by = 0.01), mean = mean(residuals(fit.ar1)), sd = sd(residuals(fit.ar1)))
lines(seq(-1, 1, by = 0.01), norm, col = "red")
par(mfrow = c(1,2))
acf(residuals(fit.ar1), main = "ACF of Residuals: AR(1) Model")
pacf(residuals(fit.ar1), main = "PACF of Residuals: AR(1) Model")
par(mfrow = c(1,1))
# ts.plot(residuals(fit.ar1), main = "Residuals of the AR(1) Model", ylab = "Residuals")
par(mfrow = c(3,1))
ts.plot(residuals(fit.ar1)^2, main = "Squared Residuals of the AR(1) Model", ylab = "Residuals")
acf(residuals(fit.ar1)^2, main = "ACF of Squared Residuals: AR(1) Model")
pacf(residuals(fit.ar1)^2, main = "PACF of Squared Residuals: AR(1) Model")




# fittando un ar(10), l'errore mostra poca autocorrelation fino al 10° lag
fit.ar10 <- arima(y1[train], order = c(10, 1, 0))
summary(fit.ar10)
par(mfrow = c(1,2))
acf(residuals(fit.ar10), main = "ACF of Residuals: AR(10) Model")
pacf(residuals(fit.ar10), main = "PACF of Residuals: AR(10) Model")
acf(residuals(fit.ar10)^2, main = "ACF of Squared Residuals: AR(10) Model")
pacf(residuals(fit.ar10)^2, main = "PACF of Squared Residuals: AR(10) Model")
# l'acf dei residui al quadrato mostra sempre e comunque autocorrelation
# questo vuol dire che la magnitudine dell'errore è autocorrelata (?)
# mi fa pensare che un modello garch possa essere più appropriato, ma
# è corretto pensare che probabilmente un modello garch con una p più alta sia più adatto?


# fit ma(1) model
fit.ma1 <- arima(y1[train], order = c(0, 1, 1))
summary(fit.ma1)
# y_t = c + e_t + 0.0714*e_t-1

par(mfrow = c(1,1))
ts.plot(y1[train], fitted(fit.ma1), col = c("red", "blue", "blue"), lty = c(1, 2, 2), main = "Observed vs Fitted Prices: MA(1) Model", xlab = "Time", ylab = "Price")
legend("topleft", legend = c("Observed", "Fitted"), col = c("red", "blue"), lty = c(1, 2), bty = "n")
par(mfrow = c(1,1))
plot(density(residuals(fit.ma1)), main = "Density of Residuals")
norm <- dnorm(seq(-1, 1, by = 0.01), mean = mean(residuals(fit.ma1)), sd = sd(residuals(fit.ma1)))
lines(seq(-1, 1, by = 0.01), norm, col = "red")
par(mfrow = c(1,2))
acf(residuals(fit.ma1), main = "ACF of Residuals: MA(1) Model")
pacf(residuals(fit.ma1), main = "PACF of Residuals: MA(1) Model")


# Fit arima(1,1,1)
fit.arima111 <- arima(y1[train], order = c(1, 1, 1))
summary(fit.arima111)
# y_t = c + 0.9320*y_t-1 -0.8950*e_t-1 + e_t

par(mfrow = c(1,1))
ts.plot(y1[train], fitted(fit.arima111), col = c("red", "blue", "blue"), lty = c(1, 2, 2), main = "Observed vs Fitted Prices: ARIMA(1,1,1) Model", xlab = "Time", ylab = "Price")
legend("topleft", legend = c("Observed", "Fitted"), col = c("red", "blue"), lty = c(1, 2), bty = "n")
par(mfrow = c(2,1))
ts.plot(residuals(fit.arima111), main = "Residuals of the ARIMA(1,1,1) Model", ylab = "Residuals")
plot(density(residuals(fit.arima111)), main = "Density of Residuals")
norm <- dnorm(seq(-1, 1, by = 0.01), mean = mean(residuals(fit.arima111)), sd = sd(residuals(fit.arima111)))
lines(seq(-1, 1, by = 0.01), norm, col = "red")
par(mfrow = c(2,2))
acf(residuals(fit.arima111), main = "ACF of Residuals: ARIMA(1,1,1) Model")
pacf(residuals(fit.arima111), main = "PACF of Residuals: ARIMA(1,1,1) Model")
acf(residuals(fit.arima111)^2, main = "ACF of Squared Residuals: ARIMA(1,1,1) Model")
pacf(residuals(fit.arima111)^2, main = "PACF of Squared Residuals: ARIMA(1,1,1) Model")

# I modelli sono molto simili

# AIC E BIC
info_criteria <- data.frame(
  Model = c("AR(1)", "AR(10)", "MA(1)", "ARIMA(1,1,1)"),
  AIC = c(AIC(fit.ar1), AIC(fit.ar10), AIC(fit.ma1), AIC(fit.arima111)),
  BIC = c(BIC(fit.ar1), BIC(fit.ar10), BIC(fit.ma1), BIC(fit.arima111))
)

print(info_criteria)

# MA(1) è il modello migliore in base ai criteri AIC e BIC



# # # # CHIEDERE # #
# # Fit the GARCH(1,1) model as an ARMA(1,1) model on the squared log returns
logdy <- diff(log(y1))
logdy <- logdy[-1]
par(mfrow = c(1,1))
ts.plot(logdy)
fit.garch11 <- arima(logdy^2, order = c(1, 0, 1))
summary(fit.garch11)
coef(fit.garch11)
# Plotting
ts.plot(residuals(fit.garch11), main = "Residuals of the GARCH(1,1) Model", ylab = "Residuals")
ts.plot(logdy, (logdy-residuals(fit.garch11)), col = c("red", "blue"), lty = c(1, 2), main = "Observed vs Fitted Prices: GARCH(1,1) Model", xlab = "Time", ylab = "Price")
legend("topleft", legend = c("Observed", "Fitted"), col = c("red", "blue"), lty = c(1, 2), bty = "n")
acf(residuals(fit.garch11), main = "ACF of Residuals: GARCH(1,1) Model")
acf(residuals(fit.garch11)^2, main = "ACF of Squared Residuals: GARCH(1,1) Model")



# garch(1,1)_2
fit.garch11_2 <- garch(x = logdy[train], order = c(1,1))

summary(fit.garch11_2)
coef(fit.garch11_2)
# coefficienti completamente diversi da prima

par(mfrow = c(2,1))
ts.plot(residuals(fit.garch11_2), main = "Residuals of the GARCH(1,1) Model", ylab = "Residuals")
acf(residuals(fit.garch11_2), main = "ACF of Residuals: GARCH(1,1) Model", na.action = na.pass)
# Unico modello dove i residuals sembrano quasi white noise (comunque c'è autocorrelation ai lag più bassi)
par(mfrow=c(1,1))
acf(residuals(fit.garch11_2)^2, main = "ACF of Squared Residuals: GARCH(1,1) Model", na.action = na.pass)
# Non c'è autocorrelation nei residuals squared

par(mfrow = c(1,1))
cond_var <- fitted.values(fit.garch11_2)

ts.plot(logdy[train])
lines(cond_var[,1], col = "red")
lines(cond_var[,2], col = "red")

ts.plot(logdy[train]^2)
lines(cond_var[,1]^2, col = "red")


# garch(1,1)_3
library(rugarch)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(1, 1)),
                    distribution.model = "norm")

# Fit the model
fit_garch11_3 <- ugarchfit(spec = spec, data = logdy[train])

# Display the summary
coef(fit_garch11_3)
summary(fit_garch11_3)

# save residuals
residuals_garch11_3 <- residuals(fit_garch11_3)
ts.plot(residuals_garch11_3, main = "Residuals of the GARCH(1,1) Model", ylab = "Residuals")
acf(residuals_garch11_3, main = "ACF of Residuals: GARCH(1,1) Model")
acf(residuals_garch11_3^2, main = "ACF of Squared Residuals: GARCH(1,1) Model")


info_criteria <- data.frame(
  Model = c("GARCH(1,1)", "GARCH(1,1)_2", "GARCH(1,1)_3"),
  AIC = c(AIC(fit.garch11), AIC(fit.garch11_2), infocriteria(fit_garch11_3)[1]),
  BIC = c(BIC(fit.garch11), BIC(fit.garch11_2), infocriteria(fit_garch11_3)[2])
)

print(info_criteria)

# Risultati su scale diverse?

# Questi modelli GARCH possono essere tranquillamente usati anche su dy, 
# a patto che la serie sia stazionaria e zero-mean?

# ccf
yld1 <- yc_logdiff_matrix[,1]
vixld <- yc_logdiff_matrix[,ncol(yc_logdiff_matrix)]
ccf(yld1, vixld, main = "Cross-Correlation Function: 1-Year Yield vs VIX", ylab = "Correlation", xlab = "Lag")
ts.plot(vixld)
ts.plot(yld1)

ccf(yld1, vixld, main = "Cross-Correlation Function: 1-Year Yield vs VIX", ylab = "Correlation", xlab = "Lag")



# ARMAX model
yc_matrix[1:11,]
yc_diff_matrix[1:11,]
fit.armax <- arima(yc_diff_matrix[, 1], order = c(1, 0, 1), xreg = yc_diff_matrix[, -1])
fit.armax$coef

# Residual analysis to check the adequacy of the model
ts.plot(resid(fit.armax), main = "Residuals of ARMAX Model")
acf(resid(fit.armax), main = "ACF of Residuals", na.action = na.pass)
pacf(resid(fit.armax), main = "PACF of Residuals", na.action = na.pass)
acf(resid(fit.armax)^2, main = "ACF of Squared Residuals", na.action = na.pass)

# Model Selection
fit.autoarima <- auto.arima(yc_diff_matrix[, 1], xreg = yc_diff_matrix[, -1])

# Display the summary
summary(fit.autoarima)

# Forecasting
forecast.autoarima <- forecast(fit.autoarima, xreg = yc_diff_matrix[, -1], h = 1)

# Display the forecast
forecast.autoarima

# Plot the forecast
plot(forecast.autoarima)
