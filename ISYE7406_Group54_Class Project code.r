install.packages("ISLR")
install.packages("tree")
#install.packages("olsrr")
#library(olsrr)
rm(list = ls())


## -------------------------------------------------------------------------------------------------------------
if (!require(Ecdat)) install.packages("Ecdat")
library(Ecdat)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if (!require(ISLR)) install.packages("ISLR")
library(ISLR)

if (!require(GGally)) install.packages("GGally")
if (!require(car)) install.packages("car")
library(car)

library(MASS)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(leaps)
library(rpart)
library(mgcv)
library(glmnet)
library(boot)
library(rpart.plot)
library(ggpubr)
library(glmnet)
library(lares)
library(caret)
library(MASS)
library(e1071)
library(nnet)
library(class)
library(randomForest)
library(ISLR)
library(tree)
library(pls)
library(DataExplorer)
library(gbm)
library(stargazer)
set.seed(201)
df <- read.table(file = "C://Temp/insurance.csv", sep=",", header=TRUE);
n = dim(df)[1]; ### total number of observations

##EDA
#Getting Summary
summary(df)
str(df)

#Histogram of Continuous Variables
par(mfrow=c(3, 1))
ggplot(data=df, aes(charges)) + 
  geom_histogram(breaks =seq (1000, 70000, by =1),
                 col="red",
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Insurance Charges") +
  labs(x="Insurance Charges", y="Count")

ggplot(data=df, aes(age)) + 
  geom_histogram(breaks=seq(10, 70, by =1), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count")
ggplot(data=df, aes(bmi)) + 
  geom_histogram(breaks=seq(10, 70, by =1), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for BMI") +
  labs(x="BMI", y="Count")

ggplot(data=df, aes(charges)) + 
  geom_histogram(breaks=seq(500, 70000, by =2000),
                 col="red",
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Insurance Charges") +
  labs(x="Insurance Charges", y="Count")


log_charges <- log(df$charges)
ggplot(data=df, aes(log_charges)) + 
  geom_histogram(breaks=seq(5,15, by =0.5),
                 col="red",
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Log of Insurance Charges") +
  labs(x="Log of Insurance Charges", y="Count")

#Boxplots
par(mfrow=c(2,2))
boxplot(charges~sex,
        main="",
        xlab="Male/Female",
        ylab="Insurance Charges",
        col=blues9,
        data=df)
boxplot(charges~smoker,
        main="",
        xlab="Smoker",
        ylab="Insurance Charges",
        col=blues9,
        data=df)

boxplot(charges~children,
        main="",
        xlab="Number of Children",
        ylab="Insurance Charges",
        col=blues9,
        data=df)
boxplot(charges~region,
        main="",
        xlab="Region",
        ylab="Insurance Charges",
        col=blues9,
        data=df)

par(mfrow=c(1,2))
ggplot(df, aes(x=bmi, y=charges)) + geom_point() +
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm,   
              se=FALSE,    
              fullrange=TRUE) 
ggplot(df, aes(x=age, y=charges)) + geom_point() +
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm,   
              se=FALSE,    
              fullrange=TRUE)

df_cat <-  df %>% 
  mutate(Male = ifelse(sex== "male",1,0))%>%
  mutate(Female = ifelse(sex== "female",1,0))%>%
  mutate(Smoker = ifelse(smoker == "yes",1,0))%>%
  mutate(Non_Smoker = ifelse(smoker == "no",1,0))%>%
  mutate(Region_Northeast = ifelse(region == "northeast",1,0))%>%
  mutate(Region_Northwest = ifelse(region == "northwest",1,0))%>%
  mutate(Region_Southeast = ifelse(region == "southeast",1,0))%>%
  mutate(Region_Southwest = ifelse(region == "southwest",1,0))

#df_cor <- subset(df_cat, select = c(age, bmi,children,Male,Female,Smoker,Non_Smoker,Region_Northeast,Region_Northwest,Region_Southeast,Region_Southwest,charges))
df_cor <- subset(df_cat, select = c(age, bmi,children,Male,Female,Smoker,Non_Smoker,charges))
corrplot(cor(df_cor), method = "number")

cor(df_cor)
#charges in different regions
ggplot(df, aes(x = region, y = charges, color = region)) +
  geom_boxplot() +
  labs(title = "Medical Costs by Region",y="Insurance Charges")

# Convert the numerical categorical variables to predictors
df$sex <-  as.factor(df$sex)
df$smoker <-  as.factor(df$smoker)
df$region <-  as.factor(df$region)
#df$logcharges<-log(df$charges)
str(df)


#data spilitting
set.seed(19930419)
n1 <- 0.80*n 
set.seed(19930419)
i= sort(sample(1:n, n1))
df_train <- df[i, ]
df_test <- df[-i, ]
dim(df_train)
dim(df_test)
Train_err <- NULL;
Test_err <- NULL;

##Before transformation using charges as it is
model0 <- lm(charges~.,data = df_train)
summary(model0)
ytrue <- df_test[,7]
MSEmod0train <- mean( (resid(model0) )^2);
MSEmod0train
set.seed(3458)
pred0 <- predict(model0, df_test[,1:6])
MSEmod0test <-mean((pred0 - ytrue)^2)
MSEmod0test

#Model1: full model after log transformation
model1<-lm(log(charges)~.,data=df_train)
summary(model1)

# Cook’s Distance-checking for outliers
cook = cooks.distance(model1)
plot(cook,
     type="h",
     lwd=3,
     ylab = "Cook's Distance",
     main="Cook's Distance")

vif =vif(model1)
vif
stargazer(model1, type = "text")
par(mfrow=c(2,2))
plot(model1)
hist(resid(model1),main="Histogram of Residuals")
#par(mfrow=c(2,1))
plot(model1$residuals)
plot(model0$residuals)
ytrue<-log(df_train$charges)
pred1_train<-predict(model1,df_train[,-7])
Train_err <- c(Train_err, mean((pred1_train-log(df_train$charges))^2)); 
Train_err; 

pred1_test<-predict(model1,newdata=df_test[,-7])
Test_err <- c(Test_err, mean((pred1_test-log(df_test$charges))^2)); 
Test_err;

#Variable selection using stepwise regression
nullmodel <- lm(charges~ 1, data = df_train)
fullmodel <- lm(charges ~ ., data = df_train)

#forward selection
model_fw<- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward")
summary(model_fw)

#Backward selection
model_back <- step(fullmodel, direction = "backward")
summary(model_back)
#stepwise selection
model_both <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "both")
summary(model_both)

# All the stepwise regression method shows that the sex variable is not significant. 
model2<-lm(formula = log(charges) ~ smoker + age + bmi + children + region, data = df_train)
summary(model2)
#mn<-ols_step_all_possible(model2)
#plot(mn)
pred2_train<-predict(model2,newdata=df_train)
Train_err <- c(Train_err, mean((pred2_train-log(df_train$charges))^2)); 
Train_err

pred2_test<-predict(model2,newdata=df_test)
Test_err <- c(Test_err, mean((pred2_test-log(df_test$charges))^2)); 
Test_err;

##Checking for normality and histogram of residuals
resids = rstandard(model2)
fit = model2$fitted
hist(resids,nclass=20,col=blues9,main="Histogram of residuals")
qqnorm(resids)

##Ridge regression & Lasso Regression
#Converting into matrix for glmnet and cvglmnet
x_train=model.matrix(log(charges)~.,df_train)[,-1]
y_train=log(df_train$charges)
x_test=model.matrix(log(charges)~.,df_test)[,-1]
y_test=log(df_test$charges)

#First we will fit a ridge regression model. This is achieved by calling 'cv.glmnet' with aplha=0
#Using cross validation to select lambda
cv_ridge=cv.glmnet(x_train,y_train,alpha=0)
#plot(cv_ridge) ## 2 lines, one at the min and another at 1std. error
bestlam_ridge = cv_ridge$lambda.min
bestlam_ridge
i <- which(cv_ridge$lambda == cv_ridge$lambda.min)
mse_min_ridge <- cv_ridge$cvm[i]
mse_min_ridge

#Plot for optimal lambda value
ggplot(mapping = aes(x=log(cv_ridge$lambda), y=cv_ridge$cvm)) +
  geom_point() +
  geom_point(aes(x=log(bestlam_ridge), y=mse_min_ridge, color="blue", size = 2), show.legend = FALSE, color="blue") +
  geom_errorbar(aes(ymin=cv_ridge$cvm-cv_ridge$cvsd, ymax=cv_ridge$cvm+cv_ridge$cvsd), color="gray") +
  xlab("Log(lambda)") +
  ylab("Mean Squared Error") +
  labs(title = "Optimal Lambda for Ridge Regression", subtitle = paste("Best Lambda: ", bestlam_ridge)) +
  theme_classic()

# Ridge model with optimal lambda
model3 = glmnet(x_train,y_train, alpha=0, lambda=bestlam_ridge)
summary(model3)
pred3_train<-predict(model3,newx = x_train)
#Train error
Train_err <- c(Train_err, mean((pred3_train-y_train)^2)); 
Train_err; 
#Test error
pred3_test<-predict(model3,newx = x_test)
Test_err <- c(Test_err, mean((pred3_test-y_test)^2)); 
Test_err;

SSE <- sum((pred3_train - y_train)^2)
SST <- sum((y_train - mean(y_train))^2)
R_square_ridge <- 1 - SSE / SST
R_square_ridge
coef(model3)


ridge<-glmnet(x_train,y_train, alpha=0)
#Plot Ridge regression coefficients
plot(ridge,xvar="lambda",label=T,main="Ridge regression coefficients")
abline(v=cv_ridge$lambda.min,col="red",lty=2)


#First we will fit a Lasso regression model. This is achieved by calling 'cv.glmnet' with aplha=1
#Using cross validation to select lambda
cv_lasso=cv.glmnet(x_train,y_train,alpha=1)
plot(cv_lasso,xvar='lambda') ## 2 lines, one at the min and another at 1std. error
bestlam_lasso = cv_lasso$lambda.min
bestlam_lasso
i <- which(cv_lasso$lambda == cv_lasso$lambda.min)
mse_min_lasso <- cv_lasso$cvm[i]
mse_min_lasso
# Lasso model with optimal lambda
ggplot(mapping = aes(x=log(cv_lasso$lambda), y=cv_lasso$cvm)) +
  geom_point() +
  geom_point(aes(x=log(bestlam_lasso), y=mse_min_lasso, color="blue", size = 2), show.legend = FALSE, color="green") +
  geom_errorbar(aes(ymin=cv_lasso$cvm-cv_lasso$cvsd, ymax=cv_lasso$cvm+cv_lasso$cvsd), color="gray") +
  xlab("Log(lambda)") +
  ylab("Mean Squared Error") +
  labs(title = "Optimal Lambda for Lasso Regression", subtitle = paste("Best Lambda: ", bestlam_lasso)) +
  theme_classic()
#Lasso model with optimal lambda
model4 = glmnet(x_train,y_train, alpha=1, lambda=bestlam_lasso)
summary(model4)
pred4_train<-predict(model4,newx = x_train)
#Train error
Train_err <- c(Train_err, mean((pred4_train-y_train)^2)); 
Train_err; 
#Test error
pred4_test<-predict(model4,newx = x_test)
Test_err <- c(Test_err, mean((pred4_test-y_test)^2)); 
Test_err;

SSE <- sum((pred4_train - y_train)^2)
SST <- sum((y_train - mean(y_train))^2)
R_square_ridge <- 1 - SSE / SST
R_square_ridge


lasso<-glmnet(x_train,y_train, alpha=0)
#Plot Lasso regression coefficients
plot(lasso,xvar="lambda",label=T,main="Lasso regression coefficients")
abline(v=cv_lasso$lambda.min,col="red",lty=2)
coef(model4)
#plot(model4)
##Bagging: special case of random forest, where m=p
model5<-randomForest(log(charges)~.,data=df_train,mtry=6,importance=TRUE)
model5
plot(model5)
summary(model5)
importance(model5)
# Variable Importance Plot for Bagging Model
varImpPlot(model5,main="Variable Importance Plot for Bagging Model")
#train error
pred5_train<-predict(model5,newdata=df_train)
Train_err <- c(Train_err, mean((pred5_train-log(df_train$charges))^2)); 
Train_err
#Test error
pred5_test<-predict(model5,newdata=df_test)
Test_err <- c(Test_err, mean((pred5_test-log(df_test$charges))^2)); 
Test_err;

##Random forest
rf1 <-randomForest(log(charges)~.,data=df_train)
rf1
plot(rf1,main="Plot for Random Forest Model (No tuning)")
varImpPlot(rf1,main="Variable Importance Plot for Random Forest Model (No tuning)")
bestmtry<-tuneRF(x = df_train[,-7],y =log(df_train$charges),ntreeTry = 500,mtryStart = 1)
title("OOB Error vs mtry")
print(bestmtry)

#Random forest model with optimized values
model6 <-randomForest(log(charges)~.,data=df_train,mtry=4,ntree=500)
model6
plot(model6,main="Plot for Random Forest Model (tuned parameters)")
importance(model6)
#Variable Importance Plot for Random Forest model- after parameter tuning
varImpPlot(model6,main="Variable Importance Plot for Random Forest model(tuned parameters)")

#Train error
pred6_train<-predict(model6,newdata=df_train)
Train_err <- c(Train_err, mean((pred6_train-log(df_train$charges))^2)); 
Train_err
#Test error
pred6_test<-predict(model6,newdata=df_test)
Test_err <- c(Test_err, mean((pred6_test-log(df_test$charges))^2)); 
Test_err;

## Boosting 
# Tuned n.trees parameter for the values of 500, 1000, 1500 and 2000, and chose 2000 as it had the lowest training error.
# The code took a long time to run for these 4 n.trees values, hence they are not coded in a loop here.
#Fitting the model
Model_Boosting <- gbm(log(charges) ~ .,data=df_train, distribution = 'gaussian', n.trees = 2000, shrinkage = 0.01, interaction.depth = 3, cv.folds = 10)
summary(Model_Boosting)

#Checking performance of the model for optimal number of iterations
Boosting_Performance = gbm.perf(Model_Boosting, method="cv") 
Boosting_Performance #Gives number of iterations = 761

#Generating predictions on training data and training error using optimal number of iterations
predict_boosting_train = predict(Model_Boosting, newdata = df_train, n.trees=Boosting_Performance, type="response")
Train_err <- c(Train_err, mean((predict_boosting_train-log(df_train$charges))^2)); 
Train_err

#Generating predictions on testing data and test error using optimal number of iterations
predict_boosting_test = predict(Model_Boosting, newdata = df_test, n.trees=Boosting_Performance, type="response")
Test_err <- c(Test_err, mean((predict_boosting_test-log(df_test$charges))^2)); 
Test_err;

## Principal Component Regression
#Fitting PCR model
Model_PCR <- pcr(log(charges)~., data=df_train, validation="CV");  
head(df_train)
#Plotting RMSEP and displaying summary of the model
validationplot(Model_PCR);
summary(Model_PCR); 

#Optimal number of components id 6, which is full model with all variables
pcr_components <- which.min(Model_PCR$validation$adj);
pcr_components

#Training error
predict_pcr_train = predict(Model_PCR, ncomp = pcr_components, newdata = df_train[,1:6]); 
Train_err <- c(Train_err, mean((predict_pcr_train-log(df_train$charges))^2)); 
Train_err

#Test Error
predict_pcr_test = predict(Model_PCR, ncomp = pcr_components, newdata = df_test[,1:6]); 
Test_err <- c(Test_err, mean((predict_pcr_test-log(df_test$charges))^2)); 
Test_err;

#### Monte Carlo Cross Validation
###Now, lets do Cross Validation for B = 100 iterations
### Initialize the Test Error values for all models in all $B=100$ loops
TEALL = NULL; ### Final Training Error values
TEST_ALL = NULL; ### Final Testing Error values

set.seed(7406);
B= 100; ### number of loops
for (b in 1:B){
  TE_b = NULL; ### Training Error values for iteration b
  TEST_b = NULL; ### Test Error values for iteration b
  ### randomly select n1 observations as a new training subset in each loop
  flag <- sort(sample(1:n, n1));
  df_train <- df[-flag,];
  df_test <- df[flag,];
  ytrue <- log(df_test$charges)
  
  # Linear Regression:All Variables
  model1_allvar<-lm(log(charges)~.,data=df_train)
  #Training error
  predict_allvar_train = predict(model1_allvar, newdata = df_train)
  MSE_model_allvar_train =  mean( (predict_allvar_train - log(df_train$charges))^2)
  TE_b <- cbind( TE_b, MSE_model_allvar_train);
  
  #Test error
  predict_allvar_test = predict(model1_allvar, newdata = df_test);
  MSE_model_allvar_test = mean((predict_allvar_test - ytrue)^2);
  TEST_b <- cbind( TEST_b, MSE_model_allvar_test);  
  
  # Stepwise regression 
  #Fitting the model
  model2_stepwise<-lm(formula = log(charges) ~ smoker + age + bmi + children + region, data = df_train)
  
  ##Training error
  predict_stepwise_train = predict(model2_stepwise, newdata = df_train)
  MSE_Model_stepwise_train = mean( (predict_stepwise_train - log(df_train$charges))^2)
  TE_b <- cbind( TE_b, MSE_Model_stepwise_train);
  
  #Test error 
  predict_stepwise_test = predict(model2_stepwise, newdata = df_test)
  MSE_Model_stepwise_test = mean( (predict_stepwise_test - log(df_test$charges))^2)
  TEST_b <- cbind( TEST_b,MSE_Model_stepwise_test);
  
  # Ridge Regression
  model3_ridge = glmnet(x_train,y_train, alpha=0, lambda=bestlam_ridge)
  #Training error
  predict_ridge_train<- predict(model3_ridge,newx = x_train)
  MSE_Model_ridge_train = mean( (predict_ridge_train - y_train)^2); 
  TE_b <- cbind( TE_b, MSE_Model_ridge_train);
  
  #Test Error
  predict_ridge_test<- predict(model3_ridge,newx = x_test)
  MSE_Model_ridge_test = mean( (predict_ridge_test - y_test)^2);
  TEST_b <- cbind( TEST_b, MSE_Model_ridge_test);
  
  # Lasso Regression
  model4_lasso = glmnet(x_train,y_train, alpha=1, lambda=bestlam_lasso)
  #Training error
  predict_lasso_train<- predict(model4_lasso,newx = x_train)
  MSE_Model_lasso_train = mean( (predict_lasso_train - y_train)^2); 
  TE_b <- cbind( TE_b, MSE_Model_lasso_train);
  
  #Test Error
  predict_lasso_test<- predict(model4_lasso,newx = x_test)
  MSE_Model_lasso_test = mean( (predict_lasso_test - y_test)^2);
  TEST_b <- cbind( TEST_b, MSE_Model_lasso_test);
  
  # Bagging: Special case of Random Forest Regression
  model5_bag =randomForest(log(charges)~.,data=df_train,mtry=6,importance=TRUE)
  #Training error
  predict_bag_train = predict( model5_bag, newdata = df_train)
  MSE_Model_bag_train = mean( (predict_bag_train - log(df_train$charges))^2)
  TE_b <- cbind( TE_b, MSE_Model_bag_train);
  
  #Test Error
  predict_bag_test = predict(model5_bag, newdata = df_test)
  MSE_Model_bag_test = mean( (predict_bag_test - log(df_test$charges))^2)
  TEST_b <- cbind( TEST_b,MSE_Model_bag_test);
 
  # Random Forest Regression
  model6_rf =randomForest(log(charges)~.,data=df_train,mtry=4,ntree=500)
  #Training error
  predict_rf_train = predict( model6_rf, newdata = df_train)
  MSE_Model_rf_train = mean( (predict_rf_train - log(df_train$charges))^2)
  TE_b <- cbind( TE_b, MSE_Model_rf_train);
  
  #Test Error
  predict_rf_test = predict(model6_rf, newdata = df_test)
  MSE_Model_rf_test = mean( (predict_rf_test - log(df_test$charges))^2)
  TEST_b <- cbind( TEST_b,MSE_Model_rf_test);
  
  TEALL <- rbind( TEALL, TE_b); 
  TEST_ALL <- rbind( TEST_ALL, TEST_b); 
  
}


## You can report the sample mean/variances of the training and testing errors so as to compare these models
df_mean_train<-apply(TEALL, 2, mean)
df_mean_test<-apply(TEST_ALL, 2, mean)

df_mean_train
df_mean_test
