#1time series 1
install.packages("tseries")
library(tseries)
set.seed(123)
n <- 500
eps <- rnorm(n)
x <- cumsum(eps)
acf(x, lag.max = 20, plot= FALSE)
plot(x, type = "l", xlab = 'time', ylab = 'y', main = 'time series plot')
acf(x, lag.max = 50, main = 'ACF plot of Random walk')
adf.test(x)


#2with drift a=0.1
n<- 500
eps <- rnorm(n,0,1)
x<- cumsum(eps+0.1)
plot(x, type = 'l', xlab = "time", ylab = 'x', main = 'time series plot with drift')
c = acf(x, lag.max = 30)
adf.test(x)


#3Random walk model y_t=0.3y_t+e_t
n <- 500
eps <- rnorm(n)
yt <- numeric(n)
for(i in 2:n){
  yt[i] = 0.3*yt[i-1] + eps[i]
}
ts.plot(yt, main = 'time series 1')
acf(yt)
pacf(yt)
adf.test(yt)


#4 AR(2) 
n <- 1000
eps <- rnorm(n)
yt <- numeric(n)
for (i in 3:n){
  yt[i] = 0.4*yt[i-1]+0.5*yt[i-2]+ eps[i]
}
ts.plot(yt, main = 'time series plot of AR(2) series', lwd = 1.5)
acf(yt)
pacf(yt)
adf.test(yt)


#5 MA(1) and MA(2) yt=e_t+e_t-1, yt= et+ 0.4e_t-1 - 0.22e_t-2
y1 <- arima.sim(n=1000,model = list(order = c(0,0,1), ma = 1))
ts.plot(y1)
acf(y1)
pacf(y1)
adf.test(y1)
#second part MA(2)
y2 <- arima.sim(n=1000, model = list(order = c(0,0,2), ma = c(0.4, -0.22)))
ts.plot(y2)
acf(y2)
pacf(y2)
adf.test(y2)

#6ARMA
library(tseries)
y = arima.sim(n=1000, model = list(order = c(2,0,2), ar = c(0.5, -0.2), ma = c(0.7, 0.3)))
par(mfrow=c(2,2))
ts.plot(y)
acf(y)
pacf(y)
adf.test(y)
