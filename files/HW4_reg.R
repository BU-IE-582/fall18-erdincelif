require(penalized)
library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)

data=read.csv(file="C:/Users/09172/Desktop/parkinsons_updrs.csv", header = TRUE, sep = ",")
data= as.data.frame(data)

data[,3] = as.factor(data[,3])

# data=data[data$V1>=2005,]

smp_size = floor(0.70*nrow(data))

set.seed(123)
train_ind=sample(seq_len(nrow(data)), size = smp_size)

train= data[train_ind,]
test= data[-train_ind,]

trainclass=train[,21]
traindata=train[,2:20]
testclass=test[,21]
testdata=test[,2:20]


#install.packages('caret', dependencies=T)
library(caret)

d =cbind(trainclass,traindata)

# LASSO

# fit1 = optL1(trainclass, traindata, lambda2=0, fold=10)
# pen=penalized(trainclass~., data = traindata, lambda1=0.3911655,
#             lambda2=0, model = "linear", standardize = FALSE, trace = TRUE)
# # 
# train_pred = predict(pen, traindata)
# MSE_train= mean((as.data.frame(train_pred)$mu - trainclass)^2)
# 
# test_pred= predict(pen, testdata)
# MSE_test = mean((as.data.frame(test_pred)$mu - testclass)^2)


#fit <- profL1(trainclass, traindata, fold=10, plot=TRUE)
# 
# list=seq(0, 2, by = 0.5)
# for (i in list){
# pen=penalized(trainclass~., data = traindata, lambda1=i,
#               lambda2=0, model = "poisson", standardize = FALSE, trace = TRUE)
# y[i] = sum(pen@residuals^2)/nrow(train)
# }
# plot(list,y)
# list
# y
# 
# # ###
# grid = 10^seq(10, -2, length = 100)
# lasso_mod = glmnet(as.matrix(traindata), trainclass, alpha = 1, lambda = grid) # Fit lasso model on training data
# 
# plot(lasso_mod)
# 
# cv.out = cv.glmnet(as.matrix(traindata), trainclass, alpha = 1) # Fit lasso model on training data
# plot(cv.out) # Draw plot of training MSE as a function of lambda
# bestlam = cv.out$lambda.min # Select lamda that minimizes training MSE
# train_MSE = min(cv.out$cvm)
# 
# lasso_pred = predict(lasso_mod, s = bestlam, newx = as.matrix(testdata)) # Use best lambda to predict test data
# lasso_pred = round(lasso_pred)
# test_MSE = mean((lasso_pred - testclass)^2) # Calculate test MSE

model_pen <- train(trainclass~., data =d,  method = "rqlasso",
                  trControl = trainControl("cv", number = 10))

model_pen$bestTune
predictions_las_test = predict(model_pen,testdata)
MSE_test_las = mean(predictions_las_test - testclass)^2


predictions_las_train = predict(model_pen,traindata)
MSE_train_las = mean(predictions_las_train - trainclass)^2


# SVM

# install.packages("e1071")
# library("e1071")

# tuned_parameters <- tune.svm(as.matrix(trainclass)~as.matrix(traindata), gamma = 10^(-5:-1), cost = 10^(-3:1))
# 
# summary(tuned_parameters)
# svm_model = svm(as.matrix(trainclass)~as.matrix(traindata),  kernel = "polynomial", gamma = 1e-05, cost = 0.001)
# 
# my_prediction <- predict(svm_model, testdata)
# MSE_test_svm = mean((as.data.frame(test_pred)$mu - testclass)^2)


# svm_tune <- tune(svm, train.x=traindata, train.y=trainclass, 
#                  kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
# 
# print(svm_tune)
# 
# svm_model_after_tune <- svm(as.matrix(trainclass)~as.matrix(traindata),  kernel="radial", cost=10, gamma=0.5)
# summary(svm_model_after_tune)
# my_prediction <- predict(svm_model_after_tune, testdata)
# MSE_train_svm = mean((my_prediction - trainclass)^2)
# 
# svm_model_test <- svm(as.matrix(testclass)~as.matrix(testdata),  kernel="radial", cost=10, gamma=0.5)
# summary(svm_model_test)
# my_prediction_test <- predict(svm_model_test, testdata)
# MSE_test_svm = mean((my_prediction_test - testclass)^2)
# 
# 
# svm_tune_2 <- tune(svm, train.x=traindata, train.y=trainclass, 
#                  kernel="polynomial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))


# Radial 
model_svmR <- train(trainclass~., data =d,  method = "svmRadial",
                   trControl = trainControl("cv", number = 10))

model_svmR$bestTune
predictions_svmR_test = predict(model_svmR,testdata)
MSE_test_svmR = mean(predictions_svmR_test - testclass)^2

predictions_svmR_train = predict(model_svmR,traindata)
MSE_train_svmR = mean(predictions_svmR_train - trainclass)^2

# Polynomial
model_svmP <- train(trainclass~., data =d,  method = "svmPoly",
                    trControl = trainControl("cv", number = 10))

model_svmP$bestTune
predictions_svmP_test = predict(model_svmP,testdata)
MSE_test_svmP = mean(predictions_svmP_test - testclass)^2

predictions_svmP_train = predict(model_svmP,traindata)
MSE_train_svmP = mean(predictions_svmP_train - trainclass)^2

# DECISION TREE

# library(rpart)
# 
# fit <- rpart(as.matrix(trainclass)~as.matrix(traindata), 
#              method="anova", data=cu.summary, control)
# 
# dt_tune= tune.rpart(as.matrix(trainclass)~as.matrix(traindata), data= traindata, minbucket = c(5,10,15,20),
#            cp = c(0.005, 0.01,0.02,0.04, 0.05, 0.1))
# plot(dt_tune)
# 
# dt_model = rpart(as.matrix(trainclass)~as.matrix(traindata), data= traindata, control = rpart.control(cp = 0.005, minbucket = 5))
# 
# summary(dt_model)
# 
# predictions_dt = predict(dt_model, traindata)
# MSE_train_dt = mean((predictions_dt - trainclass)^2)
# 
# dt_model_test = rpart(as.matrix(testclass)~as.matrix(testdata), data= testdata, control = rpart.control(cp = 0.005, minbucket = 5))
# MSE_test_dt = mean((predict(dt_model_test, testdata) - testclass)^2)


model_dt <- train(trainclass~., data =d,  method = "rpart",
                    trControl = trainControl("cv", number = 10))

model_dt$bestTune
predictions_dt_test = predict(model_dt,testdata)
MSE_test_dt = mean(predictions_dt_test - testclass)^2

predictions_dt_train = predict(model_dt,traindata)
MSE_train_dt = mean(predictions_dt_train - trainclass)^2
# 
# printcp(fit) # display the results 
# plot(fit) # visualize cross-validation results 
# summary(fit) # detailed summary of splits

#RANDOM FOREST

d =cbind(trainclass,traindata)
model_rf <- train(trainclass~., data =d,  method = "rf",
               trControl = trainControl("cv", number = 10))

model_rf$bestTune
predictions_rf_test = predict(model_rf,testdata)
MSE_test_rf = mean(predictions_rf_test - testclass)^2

predictions_rf_train = predict(model_rf,traindata)
MSE_train_rf = mean(predictions_rf_train - trainclass)^2


# XGBOOST
# install.packages("caret", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com",dependencies=TRUE))
# require(caret)


model <- train(trainclass~., data =d,  method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
model$bestTune
predictions_xg = predict(model,testdata)
MSE_test_xg = mean(predictions_xg - testclass)^2

predictions_xg_train = predict(model,traindata)
MSE_train_xg = mean(predictions_xg_train - trainclass)^2
