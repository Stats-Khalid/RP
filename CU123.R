x <- c(9.45, 7.99, 9.29, 11.66, 12.16, 10.18, 8.04, 11.46, 9.2, 10.34,
       9.03, 11.47, 10.51, 9.40, 10.08, 9.37, 10.62, 10.31, 8.52, 10.84,
       10.90, 9.33, 12.29, 11.50, 10.60, 11.08, 10.38, 11.62, 11.31, 10.52)
#parameters
mu0=10
mu1= 11
h=5
k=0.5
n=length(x)
cplus=numeric(n)
cminus =numeric(n)
#compute CCUSUM
for (i in 2:n){
  cplus[i]=max(0,cplus[i-1]+(x[i]-mu0-k))
  cminus[i]=min(0,cminus[i-1]+(x[i]-mu0+k))
}
#plot
plot(cplus, type="l", col="blue", ylim = c(min(cminus),max(cplus)), ylab= "CUSUM", xlab= "observation", main= "CUSUM CHART")
lines(cminus, col= "green")
abline(h=h, col="red")
abline(h=-h, col="red")
legend("bottomright", legend=c("c+","c-"), col=c("blue", "green"), lty=1)
#Detect out of control points
out_pos= which(cplus>h)
out_neg= which(cminus< -h)
out_pos
out_neg





#Hetroscedastisity and Autpcorrelation
sales= c(3083,3149,3218,3295,3374)
ex= c(75,78,80,82,84)
model= lm(ex~sales)
library(car)
durbinWatsonTest(model)
residual= resid(model)
#durbin watson in manual via a fuction
dw= function(res){
  r1= res[2:20]
  r2 = res[1:19]
  d= sum((r1-r2)^2)/sum(res^2)
  d
}
dw(residual)
library(lmtest)
bptest(model)