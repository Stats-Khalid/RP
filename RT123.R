#10Type 2
ftime <- c(40,60,90,120,195,260,350,420,501,620,650,730,815,910,980)
r <- 15 ; t <- 500 ; N <- 100
#Type 2 censoring.
#Case 1 :WOR
lambda1 <- r/(sum(ftime)+(N-r)*max(ftime))
mst1 <- 1/lambda1
rt1 <- exp(-t*lambda1)
#Case 2: WR
lambda2 <- r/(N*max(ftime))
mst2 <- 1/lambda2
rt2 <- exp(-t*lambda2)
result <- data.frame(case = c("WOR", "WR"), MST = c(round(mst1,3), round(mst2,3)), reliability = c(rt1, rt2))
result


#11Type 1 censoring
ftime <- c(50, 75, 120, 160, 210, 260, 300, 340, 360, 390)
N <- 80 ; T <- 400 ;t <- 300; r <- 10
#CASE 1: WOR
lambda1 <- r/(sum(ftime)+(N-r)*T)
mst1 <- 1/lambda1
rt1 <- exp(-(t*lambda1))
#CASE 2: WR
lambda2 <- r/(N*T)
mst2 <- 1/lambda2
rt2 <- exp(-(t*lambda2))
result <- data.frame(case = c("WOR", "WR"), MST = c(round(mst1,3), round(mst2,3)), reliability = c(rt1, rt2))
result


#12 MLE of Reliability, Hazard Function and MTSF
xt <- c(10.99, 15.79, 24.14, 34.43, 43.72, 51.72, 56.12, 68.27, 77.2, 88.47, 91.07, 117.58, 130.4, 133.12, 152.9, 159, 193.62, 208.71, 308.82, 316.07)
t <= 100 ; n <- 20; m1 <- 20
mu1 <- min(xt) ; mu1
sigma1 <- (sum(xt-mu1)/n)
sigma1
# MLE of R(t) at t = 100
rt1 <- exp(-(t-mu1)/sigma1)
rt1
#MLE of h(t) at t = 100
ht1 <- 1/sigma1
ht1
#MLE of MTSF at t = 100
MTSF1 <- mu1 +sigma1
MTSF1
# CASE 2: Based on 15 observations
m2 <- 15
xt1 <- xt[1:15]
xt1
mu2 <- min(xt1)
mu2
sigma2 <- (sum(xt1 - mu2) + (n-m2)*(max(xt1)-mu2))/m2
sigma2
# MLE of reliability based on 15 obsn at t = 100
rt2 <- exp(-(t - mu2)/sigma2)
rt2
# MLE of hazard rate based on 15 obsn at t = 100
ht2 <- 1/sigma2
ht2
# MLE of MTSF based on 15 obsn at t = 100
MTSF2 <- mu2 + sigma2
MTSF2
