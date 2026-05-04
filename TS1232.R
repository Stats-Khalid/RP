#7Best model MA or AR
data = c(8.35 ,9.06, 7.77, 8.49, 9.19, 10.20, 11.68, 12.86, 12.69, 12.41,11.67, 9.44, 6.99, 6.51, 6.20, 6.96, 8.94, 10.04, 11.04, 12.17, 12.88, 14.40, 13.74, 11.77, 9.72, 6.44, 3.93, 4.47, 7.47, 9.81, 11.39, 12.19, 13.01, 12.06, 10.39, 7.17, 6.81, 7.71, 9.76, 10.82, 12.01, 12.04, 11.14, 10.85, 10.21, 8.54, 8.49, 10.13, 11.32, 12.86, 12.80, 12.87, 12.80, 12.95, 12.58, 12.83, 11.88, 11.95, 10.70, 9.43, 7.28, 7.27, 8.38, 9.90, 10.09, 11.45, 11.45, 7.97, 7.18 ,5.79, 8.60, 9.44, 8.65, 9.56, 9.98, 10.36, 11.92, 13.49, 13.34, 12.03, 10.20, 8.30, 8.16 ,7.73, 7.77, 8.54, 9.70, 12.59, 13.89, 14.42, 11.97, 10.82, 12.05, 13.23, 12.92 ,13.56, 12.87, 10.27, 6.67,6.20)
mat <- matrix(rep(0,10), nrow = 2, dimnames = list(c("AR", "MA"), 1:5))
mat
for(i in 1:5){
  ar_est = arima(data, order=c(i,0,0))
  mat[1,i] = ar_est$aic
  ma_est = arima(data, order=c(0,0,i))
  mat[2,i] = ma_est$aic
}
mat
best <- arima(data, order = c(2,0,0))
best
#residual
res <- residuals(best)
res
#normality of residuals
qqnorm(res, main = "Q-Q Plot of Residuals")
qqline(res)
#Shapirow wilk test
shapiro.test(res)
#Time series plot along with fitted time series
y_fit <- data - res
ts.plot(data, main = "Time series plot of given data")
points(y_fit, type = "l",col = 2, lty = 2, lwd = 2)


#8 BJsales
library(tseries)
yt <- BJsales
ts.plot(yt, main = "Time series plot of yt")
acf(yt, main = "ACF of yt")
adf.test(yt)
# Now first differencing.
dyt <- diff(yt)
ts.plot(dyt, main = "Time series plot of dyt")
acf(dyt, main = "ACF of dyt")
adf.test(dyt)
# Now second differencing.
d2yt <- diff(dyt)
ts.plot(d2yt, "Time series plot of d2yt")
acf(d2yt, main = "ACF of d2yt")
adf.test(d2yt)

#9 BJsales ARIMA model
library(tseries)
yt <- BJsales
for (i in 0:2){
  for (j in 0:2){
    est_i <- arima(yt, order = c(i,2,j))
    print(paste("AIC for ARIMA(",i, ",2,", j,") is", round(est_i$aic, 4)))
  }
}

#best model is ARIMA(1,2,2)
best <- arima(yt, order=c(1,2,2))
best

#normality of Residual
res <- residuals(best)
qqnorm(res, main = "Q-Q Plot of Residual of the best model")
qqline(res)


#shapiro wilk test
shapiro.test(res)

#fitted values
y_fit <- yt - res

#forecast next 30 obsevations
predict(best,30)$pred

#plot time series plot
ts.plot(yt, lwd=2, xlim = c(0,200), ylim = c(200,275), main = "Time series plot")
points(y_fit, type = 'l', col = '2', lty = 2, lwd = 2)
points(predict(best,30)$pred, type = 'l', lty = 3, lwd = 3, col = 4)
