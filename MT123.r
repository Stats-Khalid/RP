data(mtcars)
dim(mtcars)
# Function to draw boxplots
plot_boxplot = function(df){
  par(mfrow = c(2,6))
  for(i in 1:ncol(df)){
    boxplot(df[,i], main = colnames(df)[i])
  }
}
# Boxplots before treating outliers
par(oma = c(0,0,4,0), mfrow = c(2,6))
plot_boxplot(mtcars)
mtext("Boxplots", outer = TRUE, cex = 1.5)
# Treat outliers in selected variables
for(i in c("hp", "wt", "qsec", "carb")){
  out = boxplot.stats(mtcars[, i])$out
  index = which(mtcars[, i] %in% out)
  mtcars[index, i] = max(mtcars[-index, i])
}
# Boxplots after treating outliers
plot_boxplot(mtcars)
mtext("Boxplots after treating outliers", outer = TRUE, cex = 1.5)



# Fit multiple regression model
model = lm(mpg ~ ., data = mtcars)
summary(model)
# Correlation matrix
corr_mat = cor(mtcars)
print(corr_mat)


# Check multicollinearity
library(car)
vif(model)
# Remove variables with high VIF one by one
mtcars.new = mtcars
mtcars.new$wt = NULL
model1 = lm(mpg ~ ., data = mtcars.new)
vif(model1)
mtcars.new$cyl = NULL
model2 = lm(mpg ~ ., data = mtcars.new)
vif(model2)
mtcars.new$hp = NULL
model3 = lm(mpg ~ ., data = mtcars.new)
vif(model3)
# Final model summary
summary(model3)



# Parsimonious Model Selection
# Full model using variables after removing multicollinearity
full_model = lm(mpg ~ ., data = mtcars.new)
# Null model means model with intercept only
null_model = lm(mpg ~ 1, data = mtcars.new)
# 1. Forward Selection
forward_model = step( null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
summary(forward_model)
# 2. Backward Selection
backward_model = step( full_model, direction = "backward")
summary(backward_model)
# 3. Stepwise Selection
stepwise_model = step( null_model, scope = list(lower = null_model, upper = full_model), direction = "both")
summary(stepwise_model)

# Compare models using AIC
AIC(forward_model, backward_model, stepwise_model)
best_model = stepwise_model

# Get residuals and fitted values
res = residuals(best_model)
fit = fitted(best_model)
# 1. Linearity check: Residuals vs Fitted plot
par(mfrow=c(1,2))
plot(fit, res, main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Residuals", pch = 19)
abline(h = 0, col = "red", lty = 2)
# 2. Normality of errors: Q-Q plot
qqnorm(res, main = "Normal Q-Q Plot of Residuals")
qqline(res, col = "red", lty = 2)
# 3. Shapiro-Wilk test for normality
shapiro.test(res)


# 4. Durbin-Watson test for autocorrelation
library(car)
durbinWatsonTest(best_model)
# 5. Breusch-Pagan test for heteroscedasticity
library(lmtest)
bptest(best_model)