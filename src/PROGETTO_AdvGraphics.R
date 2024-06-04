library(png)
library(grid)
library(gridExtra)
library(xts)

# Fetch all available US yield curves from 1 to 30 years
source("src/FRED_IMPORT.R")
# load("data/workspace.RData")
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

# # workflow: 
# * check stationarity
# * fit model on train
# * forecast on test & select best model
# * check residuals of the best models
# * either stop or go on with more complex models

dev.new()
par(mfrow = c(1,1))
# plot the series in p1
plot.xts(y1, main = "1-year Treasury Yield - Daily", xlab = "Date", ylab = "Yield")
# plot the acf and pacf in p2
par(mfrow = c(2,1))
acf(y1, lag.max = 100, main = "ACF of Y_t")
pacf(y1, lag.max = 100, main = "PACF Y_t")
adf.test(y1, k = 0)
# unit root

## fig1 ###
png("p1.png", width = 800, height = 600)
plot.xts(y1, main = "1-year Treasury Yield - Daily", xlab = "Date", ylab = "Yield")
dev.off()

png("p2.png", width = 800, height = 600)
par(mfrow = c(2, 1))
acf(y1, lag.max = 100, main = "ACF of Y_t")
pacf(y1, lag.max = 100, main = "PACF Y_t")
dev.off()

p1 <- readPNG("p1.png")
p2 <- readPNG("p2.png")

png("out/fig1.png", width = 1600, height = 600)
layout(matrix(c(1, 2), nrow = 1, ncol = 2))
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(p1, 0, 0, 1, 1)
plot(NA, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(p2, 0, 0, 1, 1)
dev.off()
file.remove("p1.png")
file.remove("p2.png")
rm(p1, p2)
##


# dy plots
dy <- diff(y1)
dy <- dy[-1]
par(mfrow = c(1,1))
plot.xts(dy, main = "Differenced 1-year Treasury Yield", xlab = "Time", ylab = "Price")
par(mfrow = c(2,1))
acf(dy, lag.max = 100, main = "ACF of dY_t")
pacf(dy, lag.max = 100, main = "PACF of dY_t")
adf.test(dy, k = 0)
# no unit root

### fig2 ###
png("p1.png", width = 800, height = 600)
plot.xts(dy, main = "Differenced 1-year Treasury Yield", xlab = "Time", ylab = "Price")
dev.off()

png("p2.png", width = 800, height = 600)
par(mfrow = c(2, 1))
acf(dy, lag.max = 100, main = "ACF of dY_t")
pacf(dy, lag.max = 100, main = "PACF of dY_t")
dev.off()

p1 <- readPNG("p1.png")
p2 <- readPNG("p2.png")

png("out/fig2.png", width = 1600, height = 600)
layout(matrix(c(1, 2), nrow = 1, ncol = 2))
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(p1, 0, 0, 1, 1)
plot(NA, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(p2, 0, 0, 1, 1)
dev.off()
file.remove("p1.png")
file.remove("p2.png")
rm(p1, p2)
###

n <- length(y1)
train <- 1:floor(0.7 * n)
set.seed(1234)

grid <- expand.grid(p = 0:7, d = 1:2, q = 0:7)

# ARIMA
fit.arima <- lapply(1:nrow(grid), function(i) {
  Arima(y1[train], order = c(grid$p[i], grid$d[i], grid$q[i]), method = "ML")
})

fit.arima.test <- lapply(1:nrow(grid), function(i) {
  Arima(y1[-train], order = c(grid$p[i], grid$d[i], grid$q[i]), model = fit.arima[[i]])
})


# table with information criteria
info_criteria_arima <- data.frame(
  p = grid$p,
  d = grid$d,
  q = grid$q,
  AIC = sapply(fit.arima.test, function (x) summary(x)$aic),
  AICc = sapply(fit.arima.test, function(x) summary(x)$aicc),
  BIC = sapply(fit.arima.test, function(x) summary(x)$bic),
  Likelihood = sapply(fit.arima.test, logLik)
)

best.spec.arima.idx <- which.min(info_criteria_arima$BIC)
best.spec.arima <- info_criteria_arima[best.spec.arima.idx,]
best.spec.arima

summary(fit.arima[[best.spec.arima.idx]])
summary(fit.arima.test[[best.spec.arima.idx]])
# save res and est for later
res.arima <- residuals(fit.arima[[best.spec.arima.idx]])
res.arima.test <- residuals(fit.arima.test[[best.spec.arima.idx]])
est.arima <- fitted(fit.arima[[best.spec.arima.idx]])
est.arima.test <- fitted(fit.arima.test[[best.spec.arima.idx]])


### fig5 ###
png("out/fig5.png", width = 800, height = 800)
par(mfrow = c(2, 2))
ts.plot(residuals(fit.arima[[best.spec.arima.idx]]), main = "Residuals of the Best ARIMA Model", ylab = "Residuals")
plot(density(residuals(fit.arima[[best.spec.arima.idx]])), main = "Density of Residuals")
norm_dist <- rnorm(nrow(y1[train]), mean = mean(residuals(fit.arima[[best.spec.arima.idx]])), sd = sd(residuals(fit.arima[[best.spec.arima.idx]])))
lines(density(norm_dist), col = "red")
acf(residuals(fit.arima[[best.spec.arima.idx]]), main = "ACF of Residuals: Best ARIMA Model")
acf(residuals(fit.arima[[best.spec.arima.idx]])^2, main = "ACF of Squared Residuals: Best ARIMA Model")
dev.off()



# Forecasting
par(mfrow = c(2,1))
ts.plot(residuals(fit.arima.test[[best.spec.arima.idx]]), main = "Residuals of the Best ARIMA Model", ylab = "Residuals")
plot(density(residuals(fit.arima.test[[best.spec.arima.idx]])), main = "Density of Residuals")
norm_dist <- rnorm(nrow(y1[-train]), mean = mean(residuals(fit.arima.test[[best.spec.arima.idx]])), sd = sd(residuals(fit.arima.test[[best.spec.arima.idx]])))
lines(density(norm_dist), col = "red")
acf(residuals(fit.arima.test[[best.spec.arima.idx]]), main = "ACF of Residuals: Best ARIMA Model")
acf(residuals(fit.arima.test[[best.spec.arima.idx]])^2, main = "ACF of Squared Residuals: Best ARIMA Model")
colnames(yc_diff_matrix) <- paste0("ΔY",c(1,2,3,5,7,10,20,30),"_t")

# ccf
dev.new()
par(mfrow = c(2,2))
  ccf(as.vector(yc_diff_matrix[,1]), as.vector(yc_diff_matrix[,1]), main = "ACF of ΔY1_t")
for (i in 2:ncol(yc_diff_matrix)) {
  ccf(as.vector(yc_diff_matrix[,1]), as.vector(yc_diff_matrix[,i]), main = paste0("CCF of ΔY1_t and ",colnames(yc_diff_matrix)[i]))
}

### adv graphics ###
png("p1.png", width = 800, height = 800)
par(mfrow = c(2, 2))
ccf(as.vector(yc_diff_matrix[,1]), as.vector(yc_diff_matrix[,1]), main = "ACF of ΔY1_t")
for (i in 2:4) {
  ccf(as.vector(yc_diff_matrix[,1]), as.vector(yc_diff_matrix[,i]), main = paste0("CCF of ΔY1_t and ", colnames(yc_diff_matrix)[i]))
}
dev.off()

png("p2.png", width = 800, height = 800)
par(mfrow = c(2, 2))
for (i in 5:ncol(yc_diff_matrix)) {
  ccf(as.vector(yc_diff_matrix[,1]), as.vector(yc_diff_matrix[,i]), main = paste0("CCF of ΔY1_t and ", colnames(yc_diff_matrix)[i]))
}
dev.off()

p1 <- readPNG("p1.png")
p2 <- readPNG("p2.png")

png("out/fig3.png", width = 1600, height = 800)
layout(matrix(c(1, 2), nrow = 1, ncol = 2))
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(p1, 0, 0, 1, 1)
plot(NA, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
rasterImage(p2, 0, 0, 1, 1)
dev.off()

file.remove("p1.png")
file.remove("p2.png")
rm(p1, p2)
###



# Full ARMAX model
grid <- expand.grid(p = 0:7, d = 1:2, q = 0:7)

fit.armax1 <- lapply(1:nrow(grid), function(i) {
  Arima(yc_diff_matrix[train, 1], order = c(grid$p[i], grid$d[i], grid$q[i]), xreg = yc_diff_matrix[train, -1])
})

fit.armax2 <- lapply(1:nrow(grid), function(i) {
  Arima(yc_matrix[train, 1], order = c(grid$p[i], grid$d[i], grid$q[i]), xreg = yc_diff_matrix[train, -1])
})

fit.armax.test <- lapply(1:nrow(grid), function(i) {
  Arima(yc_diff_matrix[-train, 1], order = c(grid$p[i], grid$d[i], grid$q[i]), xreg = yc_diff_matrix[-train, -1], model = fit.armax2[[i]])
})


# table with information criteria
info_criteria_armax <- data.frame(
  p = grid$p,
  d = grid$d,
  q = grid$q,
  AIC = sapply(fit.armax.test, function (x) summary(x)$aic),
  AICc = sapply(fit.armax.test, function(x) summary(x)$aicc),
  BIC = sapply(fit.armax.test, function(x) summary(x)$bic),
  Likelihood = sapply(fit.armax.test, logLik)
)

best.spec.armax.idx <- which.min(info_criteria_armax$BIC)
best.spec.armax <- info_criteria_armax[best.spec.armax.idx,]
best.spec.armax

best.armax <- fit.armax2[[best.spec.armax.idx]]
summary(best.armax)
best.armax.test <- fit.armax.test[[best.spec.armax.idx]]
summary(best.armax.test)
res.armax <- residuals(best.armax)

test.armax <- Arima(yc_matrix[train, 1], order = c(1,1,1), xreg = yc_diff_matrix[train, -1])
summary(test.armax)
checkresiduals(test.armax)
test.armax.test <- Arima(yc_matrix[-train, 1], order = c(1,1,1), xreg = yc_diff_matrix[-train, -1], model = test.armax)
summary(test.armax.test)
checkresiduals(test.armax.test)

# Residual analysis
par(mfrow = c(3,1))
ts.plot(resid(fit.armax1[[best.spec.armax.idx]]), main = "Residuals of ARMAX Model")
acf(resid(fit.armax1[[best.spec.armax.idx]]), main = "ACF of Residuals", na.action = na.pass, lag.max = 100)
acf(resid(fit.armax1[[best.spec.armax.idx]])^2, main = "ACF of Squared Residuals", na.action = na.pass, lag.max = 100)

### adv graphics ###
png("out/fig4.png", width = 800, height = 800)
par(mfrow = c(3,1))
ts.plot(resid(fit.armax1[[best.spec.armax.idx]]), main = "Residuals of ARMAX Model")
plot(density(resid(fit.armax1[[best.spec.armax.idx]])), main = "Density of Residuals")
norm_dist <- rnorm(nrow(yc_diff_matrix[train, 1]), mean = mean(resid(fit.armax1[[best.spec.armax.idx]])), sd = sd(resid(fit.armax1[[best.spec.armax.idx]])))
lines(density(norm_dist), col = "red")
pacf(resid(fit.armax1[[best.spec.armax.idx]]), main = "PACF of Squared Residuals", na.action = na.pass, lag.max = 100)
dev.off()

# Forecasting
par(mfrow = c(1,1))
ts.plot(yc_diff_matrix[-train,1])
lines(fitted(fit.armax.test[[best.spec.armax.idx]]), col = "red")
par(mfrow = c(3,1))
ts.plot(residuals(fit.armax.test[[best.spec.armax.idx]]), main = "Residuals of the ARMAX Model", ylab = "Residuals")
acf(residuals(fit.armax.test[[best.spec.armax.idx]]), main = "ACF of Residuals: ARMAX Model", na.action = na.pass, lag.max = 100)
acf(residuals(fit.armax.test[[best.spec.armax.idx]])^2, main = "ACF of Squared Residuals: ARMAX Model", na.action = na.pass, lag.max = 100)
checkresiduals(fit.armax1[[best.spec.armax.idx]])
checkresiduals(fit.arima[[best.spec.arima.idx]])


# GARCH(1,1) model on residuals
par(mfrow = c(2,1))
fit.garch11 <- garch(x = res.arima, order = c(1,1))
res.garch <- residuals(fit.garch11)
res.garch <- na.omit(res.garch)
ts.plot(res.garch)
plot(density(na.omit(res.garch)))
norm_dist <- rnorm(nrow(y1[train]), mean = mean(res.garch), sd = sd(res.garch))
lines(density(norm_dist), col = "red")
acf(res.garch, lag.max = 100, na.action = na.pass)
acf(res.garch^2, lag.max = 100, na.action = na.pass)
coef(fit.garch11)

### adv graphics ###
png("out/fig6.png", width = 800, height = 800)
par(mfrow = c(2,1))
ts.plot(res.garch)
plot(density(res.garch))
norm_dist <- rnorm(nrow(y1[train]), mean = mean(res.garch), sd = sd(res.garch))
lines(density(norm_dist), col = "red")
dev.off()

png("out/fig7.png", width = 800, height = 800)
par(mfrow = c(2,1))
acf(res.garch, lag.max = 100, na.action = na.pass, main = "ACF of Residuals: GARCH(1,1) Model")
acf(res.garch^2, lag.max = 100, na.action = na.pass, main = "ACF of Squared Residuals: GARCH(1,1) Model")
dev.off()
###

res.arima.full <- append(res.arima, res.arima.test)
garch11.full <- garch(x = res.arima.full, order = c(1,1), model = fit.garch11)
cond_var_full <- fitted.values(garch11.full)

par(mfrow = c(1,1))
ts.plot(res.arima.full, main = "ARIMA residuals and estimated sigma_t", ylab = "Residuals", xlab = "Index")
lines(cond_var_full[,1], col = "red")
lines(cond_var_full[,2], col = "red")
lines(1.96*cond_var_full[,1], col = "orange", lty = 2)
lines(1.96*cond_var_full[,2], col = "orange", lty = 2)
abline(v = length(res.arima), col = "blue", lty = 2)
legend("topright", legend = c("ARIMA residuals", "Square root of estimated conditional variance", "95% level Confidence Interval", "Train-test split point"), col = c("black", "red","orange", "blue"), lty = c(1,1,2,2))

# percent of residuals outside the confidence interval
sum(abs(res.arima.test) > 1.96*cond_var_full[-train,1])/length(res.arima.test)


### adv graphics ###
png("out/fig8.png", width = 800, height = 600)
ts.plot(res.arima.full, main = "ARIMA residuals and estimated sigma_t", ylab = "Residuals", xlab = "Index")
lines(cond_var_full[,1], col = "red")
lines(cond_var_full[,2], col = "red")
lines(1.96*cond_var_full[,1], col = "orange", lty = 2)
lines(1.96*cond_var_full[,2], col = "orange", lty = 2)
abline(v = length(res.arima), col = "blue", lty = 2)
legend("topright", legend = c("ARIMA residuals", "Square root of estimated conditional variance", "95% level Confidence Interval", "Train-test split point"), col = c("black", "red","orange", "blue"), lty = c(1,1,2,2))
dev.off()
# # manual computation of residuals
# z_t <- res.arima / (cond_var[,1])
# ts.plot(z_t)
# acf(z_t, lag.max = 100, na.action = na.pass)
# acf(z_t^2, lag.max = 100, na.action = na.pass)


est.arima.full <- append(est.arima, est.arima.test)
par(mfrow = c(1,1))
ts.plot(y1, main = "1-year Treasury Yield - Daily", xlab = "Date", ylab = "Yield")
lines(est.arima.full, col = "red")
lines(est.arima.full+1.96*cond_var_full[,1], col = "orange", lty = 2)
lines(est.arima.full+1.96*cond_var_full[,2], col = "orange", lty = 2)


y.forecast <- forecast(fit.arima.test[[best.spec.arima.idx]], h = 1)
y.forecast$mean[1]
y.forecast$upper[2]
y.forecast$lower[2]

alpha0 <- coef(fit.garch11)[1]
alpha1 <- coef(fit.garch11)[2]
beta1 <- coef(fit.garch11)[3]
e_t1 <- tail(res.arima.test, 1)
s_t1 <- tail(cond_var_full[,1]^2, 1)
s_t <- alpha0 + alpha1*e_t1^2 + beta1*s_t1
s_t
conf_int_garch <- c(y.forecast$mean[1] - 1.96*sqrt(s_t), y.forecast$mean[1] + 1.96*sqrt(s_t))
conf_int_arima <- c(y.forecast$lower[2], y.forecast$upper[2])

# Plot Last 10 days
png("out/fig9.png", width = 800, height = 600)
par(mfrow = c(1,1))
ts.plot(tail(y1,20), main = "One-step-ahead forecast of Y_t and Confidence Intervals at alpha = 0.05", xlab = "Date", ylab = "Yield", ylim = c(4.95, 5.4))
# Add conf_int_garch for tomorrow forecast
lines(rep(y.forecast$mean[1], 20), col = "red")
lines(rep(conf_int_garch[1], 20), col = "orange", lty = 2)
lines(rep(conf_int_garch[2], 20), col = "orange", lty = 2)
lines(rep(conf_int_arima[1], 20), col = "blue", lty = 2)
lines(rep(conf_int_arima[2], 20), col = "blue", lty = 2)
legend("topright", legend = c("Observed Data", "1-day forecast ARIMA", "95% Confidence Interval ARIMA", "95% Confidence Interval GARCH"), col = c("black", "red", "blue", "orange"), lty = c(1,1,2,2))
dev.off()
