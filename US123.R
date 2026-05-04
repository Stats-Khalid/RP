# Hypothesis testing using hotellings t2
height=c(69,74,68,70,72,67,66,70,76,68,72,79,74,67,66,71,74,75,75,76)
weight=c(153,175,155,135,172,150,115,137,200,130,140,265,15,112,140,150,165,185,210,220)
data <- cbind(height, weight)
data

mu0 <- c(70,170)
sigma <- matrix(c(20,100,100,1000), nrow=2, byrow = TRUE)
n <- nrow(data)
xbar <- colMeans(data)
xbar

t2 <- n*t(xbar-mu0)%*%solve(sigma)%*%(xbar-mu0) ; t2
p<- ncol(data) ; p
Fstats <- (n-p)/(p*(n-1))*t2
pval <- 1-pf(Fstats,p,n-p) ; pval
print(paste("Hotelling's T^2 = ", t2))
print(paste("F-statistic = ",Fstats))
print(paste("p-value = ", pval))


#var-cov and corr matrix
y1=c(35,35,40,10,6,20,35,35,35,30)
y2=c(3.5,4.9,30,2.8,2.7,2.8,4.6,10.9,8,1.6)
y3=c(2.8,2.7,4.38,3.21,2.73,2.81,2.88,2.9,3.28,3.2)
data <- cbind(y1,y2,y3)
var_cov_mat <- cov(data) ; var_cov_mat
cor_mat <- cor(data) ; cor_mat

#hierarchical clustring
data("USArrest")
df <- USArrests
df
df_scaled <- scale(df)

#HC using sigle linkage
dist_matrix <- dist(df_scaled)
hc_single <- hclust(dist_matrix, method = "single")
plot(hc_single, main = "Single Linkage Clustring", xlab = "", sub = "")
rect.hclust(hc_single, k=3)
cluster_single <- cutree(hc_single,k=3)
table(cluster_single)

#HC using complete linkage
dist_matrix <- dist(df_scaled)
hc_complete <- hclust(dist_matrix, method = "complete")
plot(hc_complete, main = "complete Linkage Clustring", xlab = "", sub = "")
rect.hclust(hc_complete, k=3)
cluster_complete <- cutree(hc_single,k=3)
table(cluster_complete)

#HC using average linkage
dist_matrix <- dist(df_scaled)
hc_average <- hclust(dist_matrix, method = "average")
plot(hc_average, main = "Average Linkage Clustring", xlab = "", sub = "")
rect.hclust(hc_average, k=3)
cluster_average <- cutree(hc_average,k=3)
table(cluster_average)

#k means clustring on USArrest dataset
set.seed(55)
km3 <- kmeans(df_scaled, centers = 3, nstart = 25)
km3$cluster
table(km3$cluster)
split(rownames(df), km3$cluster)

#PCA on USArrest dataset
pca <- prcomp(df_scaled)
summary(pca)
plot(pca, type = "l", main = "Scree Plot of principle components")
biplot(pca, scale = 0, main = "PCA Biplot of USArrests", cex = 0.5)

