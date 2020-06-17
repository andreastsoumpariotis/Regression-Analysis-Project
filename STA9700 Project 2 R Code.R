############################# Chapter 1 ##########################################
basketball = read.csv("Documents/Baruch College/2019-20/Fall/STA 9700 Modern Regression Analysis/Project 2/2012_nba_draft_combine.csv")
basketball
View(basketball)

############################# Chapter 2 ########################################

# x3, x7, x2, x1, x8

# Question 1
y = basketball$Height..No.Shoes.
x1 = basketball$Wingspan
x2 = basketball$Weight
x3 = basketball$Standing.reach
x4 = basketball$Vertical..Max.
x5 = basketball$Vertical..Max.Reach.
x6 = basketball$Vertical..No.Step.
x7 = basketball$Vertical..No.Step.Reach.
x8 = basketball$Body.Fat
x9 = basketball$Hand..Length.
x10 = basketball$Hand..Width.
x11 = basketball$Bench
x12 = basketball$Agility
x13 = basketball$Sprint

# Height (no shoes) (y) vs. Wingspan (x1)
plot1 = plot(x = x1, y = y, main = "Height (no shoes) vs. Wingspan", 
             xlab ="Wingspan", ylab ="Height", pch=16, cex=.7)

# Height (no shoes) (y) vs. Weight (x2)
plot2 = plot(x = x2, y = y, main = "Height (no shoes) vs. Weight", 
             xlab ="Weight", ylab ="Height", pch=16, cex=.7)

# Question 3

# Question 3 part a
# Wingspan (x1) vs. Weight (x2)
plot2 = plot(x = x2, y = x1, main = "Wingspan (x1) vs. Weight (x2)", 
             xlab ="Weight", ylab ="Wingspan", pch=16, cex=.7)

# Question 3 part b
# There is no evidence of collinearity as the points do not seem to lie on a 
# straight line.

# Question 5
# Two-Regressor Fitted Model
fit = lm(y ~ x1 + x2, data=basketball)
summary(fit)

# Question 6

# Question 6 part b
summary(fit)
# p-value: 2.2e-16

# Question 6 part c
coefficients(fit)

# Question 7

# Question 7 part b
# Regression for y on x1
fit2 = lm(y ~ x1, data=basketball)
summary(fit2)
coefficients(fit2)
# coefficient = .65000916

# Question 7 part c
# y* = y on x1
residual_1 = lm(y ~ x1, data=basketball)
residual_y = residuals(residual_1)
# x2* = x2 on x1
residual_2 = lm(x2 ~ x1, data=basketball)
residual_x2 = residuals(residual_2)
# y* on x2*
summary(lm(residual_y~residual_x2))
summary(lm(y~x1+x2))

# Question 8

# Question 8 part a and b
fit = lm(y ~ x1 + x2, data=basketball)
# R-Square
summary(fit)$r.squared
# R-Square: 0.7217068

# Question 9

# Question 9 part a and b
fit = lm(y ~ x1 + x2, data=basketball)
# Adjusted R-Square
summary(fit)$adj.r.squared
# Adjusted R-Square: 0.7121105

############################# Chapter 3 ########################################
library(dplyr)
library(matlib)

# Question 1

# Question 1 part a
# X Matrix
basketball_8_rows = basketball[1:8,]
basketball_8_rows
X_matrix = basketball_8_rows %>% select(Wingspan)
X_matrix

# Question 1 part b
# Y Vector
Y_Vector = basketball_8_rows %>% select( Height..No.Shoes.)
Y_Vector

# Question 1 part c
# Finding the b-vector [(X'X)^-1(X')(y->)]
X = matrix( c(1, 1, 1, 1, 1, 1, 1, 1, 
              90.25, 88, 89.50,
              86.25, 79.25, 87,
              80, 79.75), nrow=8, ncol = 2, byrow=FALSE)
Y = matrix(c(81.75, 80.50, 81.25, 81.50, 75.50, 80.75, 75.25, 73.75), 
           nrow=8, ncol=1, byrow=FALSE)
X
Y
transpose_X = t(X)
transpose_X
C = transpose_X %*% X
C
IC = solve(C)
IC
S = IC %*% transpose_X #S = (X'X)^-1(X')
b_vector = S %*% Y
b_vector

# Finding the y-hat vector [X*(b vector)]
yhat_vector = X %*% b_vector
yhat_vector

# Finding the Hat Matrix [X*(S)]
H = X %*% S

# CHECK: yhat_vector = H %*% Y
yhat_vector = H %*% Y
yhat_vector

# Question 1 part d
# Y on X
fit = lm(Y ~ X, data = basketball_8_rows)
summary(fit)

# Question 1 part e
# Mean
mean_x = mean(X[,2])
mean_x
# SSx
SSx = sum((X[,2] - mean_x)^2)
SSx

# Question 2

# Question 2 part a
new_X = matrix( c(1, 1, 1, 1, 1, 1, 1, 1, 
                  90.25, 88, 89.50,
                  86.25, 79.25, 87,
                  80, 79.75, 279, 234, 222, 
                  233, 203, 230, 202, 189), nrow=8, ncol = 3, byrow=FALSE)
new_X

# Question 2 part b
# Y on new_X 
fit = lm(Y ~ new_X, data = basketball)
summary(fit)

# Question 2 part c and d
# Finding the new b vector = (X'X)^-1(X')(y->)
Xmat = cbind(rep(1,8), basketball_8_rows[,7], basketball_8_rows[,13])
Xmat
Bhat = (solve(t(Xmat)%*%Xmat, tol=1e-17))%*%t(Xmat)%*%Y
Bhat
Xmat%*%Bhat
HatMat = Xmat%*%(solve(t(Xmat)%*%Xmat, tol=1e-17))%*%t(Xmat)
HatMat%*%Y

############################# Chapter 4 Part 1 ##################################

# Question 1

# Question 1 part a
# Add (x1)^2 to dataset
matrix1 = cbind(basketball %>% select(Height..No.Shoes., Wingspan))
matrix1
basketball$Wingspan_Squared = (matrix1[,2])^2
basketball$Wingspan_Squared

# Regress y on x1
fit1 = lm(y ~  x1, data=basketball)
summary(fit1)
coefficients(fit1)

# Regress y on (x1)^2
fit2 = lm(y ~ basketball$Wingspan_Squared, data=basketball)
fit2
summary(fit2)
coefficients(fit2)

# Question 1 part b
# Table output
poly_table = lm(y ~ x1 + basketball$Wingspan_Squared, data=basketball)
poly_table
summary(poly_table)

# Diagnostics Output
library(ggfortify)
autoplot(poly_table)
which.max(poly_table)

# Question 1 part e
# F-Test
var.test(x1, y)
# t-tests
t.test(x1, y)

# Question 1 part f
library(ggplot2)
poly_table = lm(y ~ x1 + basketball$Wingspan_Squared + basketball$Wingspan_Cubed, data=basketball)

temp_var = predict(poly_table, interval="prediction")
new_df = cbind(poly_table, temp_var)

ggplot(temp_var, aes(x = x1, y = y)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Height vs. Wingspan") +
  labs(y= "Height", x = "Wingspan") +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")

# Question 2
# Question 2 part a
print(basketball$Weight)
dummy = as.numeric(basketball$Weight >= 215)
basketball$dummy = as.numeric(basketball$Weight >= 215)
basketball

# Question 2 part b
# Used x1 (Wingspan) and x12 (Agility) for interaction
interaction = lm(y ~ x1 + x12 + x1:x12, data = basketball)
interaction

# Question 2 part c
autoplot(interaction)

############################# Chapter 4 Part 2 ##################################

# Question 1

# Question 1 part a
# Matrix Scatterplot of Original Data
pairs(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13,data=basketball, 
      main="Matrix Scatterplot of Original Data",
      cex.labels = 2, pch = 20, cex = 0.8)

# Question 1 part b
# Right off the bat, it doesn’t appear that my scatterplots have any problems 
# associated with curvature, heteroscedasticity, outstanding outliers, etc.
# The graphs for the x9 variable, which regresses height against player 
# hand length, seem a little funky. I believe this is due to the fact that 
# multiple players have similar hand lengths, causing the oddly shaped plots.

# Question 1 part c
library(leaps)
library(olsrr)
library(MASS)
subset = regsubsets(y ~ x1 + x2 + x8 + x11 + x12 + x13,
                  data = basketball)
model = lm(y ~ x1 + x2 + x8 + x11 + x12 + x13,
           data = basketball)
# Summary Table
k = ols_step_all_possible(model)
k
View(k)
# Criteria Plot
plot(k)
write.table(k)

# Question 1 part d
# I would use the x1 and x2 predictors as they have R-Square values close 
# to 1.0 and have low cp values as well.

# Question 1 part e
library(ggfortify)
autoplot(model)

# Question 2

# Question 2 part a and b
model_2 = lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13, 
             data = basketball)
k2 = ols_step_forward_p(model_2)
k2
# The R-Square and C(p) values are different compared to the best subsets 
# method.

# Question 3

# Question 3 part a
# Essentially, variance inflation refers to the regression of a variable 
# on something almost identical to itself. It measures the extent of 
# multicollinearity within a regression’s predictor variable. Such an 
# instance will also show inflated standard error values of the slopes 
# as well and makes model selection more difficult to conduct.

# Question 3 part b
library(car)
library(psych)
library(plyr)
vif(model)

# Question 4

# Question 4 part a
# Cook’s D is an “aggregate distance measure” that measures the affect 
# deleting an observation has on the dataset. To compute it, we take the 
# sum of squares of the residuals and divide that by p multiplied with 
# the MSE.

# Question 4 part b
library(base)
View(cooks.distance(model))
ols_plot_cooksd_chart(model)
# No, there are no outliers as the standardized residual values have 
# absolute values that are less than 2.0.

# Chapter 6 (Cross-Validation)
library(caret)
library(tidyverse)
library(dplyr)

set.seed(123)
training.samples = basketball$Height..No.Shoes. %>%
  createDataPartition(p = .8, list = FALSE)
train.data = basketball[training.samples, ]
test.data = basketball[-training.samples, ]
model = lm(Height..No.Shoes. ~ ., data = train.data)
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Height..No.Shoes.),
            RMSE = RMSE(predictions, test.data$Height..No.Shoes.),
            MAE = MAE(predictions, test.data$Height..No.Shoes.))

##     R2 RMSE  MAE
## 1 0.29882 .61556 .599981

RMSE(predictions, test.data$Height..No.Shoes.)/mean(test.data$Height..No.Shoes.)


