library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)

#read
set.seed(21)
df = read.csv("cleaned_datasetIndCountries.csv")
df2 = read.csv("cleaned_datasetGroupCountries.csv")


str(df)
str(df2)

#Split
library(caTools)
split = sample.split(df$management, 0.75)
Train = subset(df, split == TRUE)
Test = subset(df, split == FALSE)
nrow(Train)
nrow(Test)



#linear regression raw
mod1 = lm(management~., data = Train)
summary(mod1)
mean(mod1$residuals^2)

pred1 = predict.lm(mod1, newdata = Test)
mean((Test$management - pred1) ^ 2, na.rm = TRUE)


#linear regression with only significant variables
mod2 = lm(management~ lemp_firm + ownership + country  + pppgdp +degree_m +degree_nm +lb_employindex , data = Train)
summary(mod2)
mean(mod2$residuals^2)

pred2 = predict.lm(mod2, newdata = Test)
mean((Test$management - pred2) ^ 2, na.rm = TRUE)





####Data for ridge and lasso
library(glmnet)
c = colnames(df)
dummies = dummyVars(management~ ., data = df[,c])
train_dummies = predict(dummies, newdata = Train[,c])
test_dummies = predict(dummies, newdata = Test[,c])
print(dim(train_dummies)); print(dim(test_dummies))

xTrain = as.matrix(train_dummies)
yTrain = Train$management
xTest = as.matrix(test_dummies)
yTest = Test$management



#Optimal lambda for ridge
lambdas = 10^seq(2, -3, by = -.1)
ridge_cv = cv.glmnet(xTrain, yTrain, alpha = 0)
optimal_ridge = ridge_cv$lambda.min
optimal_ridge #o.oo1
plot(ridge_cv)


#ridge regression 
mod3 = glmnet(xTrain, yTrain, nlambda = 25, alpha = 0, family = 'gaussian', lambda = optimal_ridge)
summary(mod3)

pred3 = predict(mod3, s = optimal_ridge, newx = xTest)
ridgeR = 1 - (sum((pred3 - yTest)^2))/ sum((yTest - mean(yTest))^2) #R-square = 0.68
ridgeMSE = (sum((pred3 - yTest)^2))/798 #
ridgeR #0.6826
ridgeMSE #0.02783



#Optimal lambda for lasso
lasso_cv = cv.glmnet(xTrain, yTrain, alpha = 0)
optimal_lasso = lasso_cv$lambda.min 
optimal_lasso #o.oo1



#lasso regression
mod4 = glmnet(xTrain, yTrain, alpha = 1, lambda = optimal_lasso, standardize = TRUE)
summary(mod4)

pred4 = predict(mod4, s = optimal_lasso, newx = xTest)
lassoR = 1 - (sum((pred4 - yTest)^2))/ sum((yTest - mean(yTest))^2) #R-square = 0.68
lassoMSE = (sum((pred4 - yTest)^2))/798 #MSE = 0.027
lassoR  #0.6634
lassoMSE  #0.02951
