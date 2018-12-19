
data=read.csv(file="C:/Users/09172/Desktop/HW4/mice.csv", header = TRUE, sep = ",")
data= as.data.frame(data)
#data =data[complete.cases(data), ]
data[,1]=NULL
data=data[,colSums(is.na(data)) == 0] 
data[,c(29,30,31)]=NULL

# data[,32] = as.factor(data[,32])
# data[,31] = as.factor(data[,31])
# data[,30] = as.factor(data[,30])
data[,29] = as.factor(data[,29])

smp_size = floor(0.70*nrow(data))

set.seed(123)
train_ind=sample(seq_len(nrow(data)), size = smp_size)

train= data[train_ind,]
test= data[-train_ind,]

trainclass=train[,29]
traindata=train[,1:28]
testclass=test[,29]
testdata=test[,1:28]

#install.packages('caret', dependencies=T)
require(caret)

d =cbind(trainclass,traindata)

# LASSO

require(mda)
model_pen <- train(trainclass~., data =d,  method = "pda", metric = "Accuracy", maximize = TRUE,  
                   trControl = trainControl("cv", number = 10))

model_pen$bestTune
predictions_las_test = predict(model_pen,testdata)
#MSE_test_las = mean(predictions_las_test - testclass)^2

tbl_las_1=table(predictions_las_test, testclass)
acc_las_test=sum(diag(tbl_las_1))/length(testclass)

predictions_las_train = predict(model_pen,traindata)
#MSE_train_las = mean(predictions_las_train - trainclass)^2

tbl_las_2=table(predictions_las_train, trainclass)
acc_las_train=sum(diag(tbl_las_2))/length(trainclass)


# SVM

# Radial 
model_svmR <- train(trainclass~., data =d,  method = "svmRadial",
                    trControl = trainControl("cv", number = 10))

model_svmR$bestTune
predictions_svmR_test = predict(model_svmR,testdata)


tbl_svmR_1=table(predictions_svmR_test, testclass)
acc_svmR_test=sum(diag(tbl_svmR_1))/length(testclass)
#MSE_test_svmR = mean(predictions_svmR_test - testclass)^2

predictions_svmR_train = predict(model_svmR,traindata)
#MSE_train_svmR = mean(predictions_svmR_train - trainclass)^2
tbl_svmR_2=table(predictions_svmR_train, trainclass)
acc_svmR_train=sum(diag(tbl_svmR_2))/length(trainclass)

# Polynomial
model_svmP <- train(trainclass~., data =d,  method = "svmPoly",
                    trControl = trainControl("cv", number = 10))

model_svmP$bestTune
predictions_svmP_test = predict(model_svmP,testdata)

tbl_svmP_1=table(predictions_svmP_test, testclass)
acc_svmP_test=sum(diag(tbl_svmP_1))/length(testclass)

#MSE_test_svmP = mean(predictions_svmP_test - testclass)^2

predictions_svmP_train = predict(model_svmP,traindata)
#MSE_train_svmP = mean(predictions_svmP_train - trainclass)^2
tbl_svmP_2=table(predictions_svmP_train, trainclass)
acc_svmP_train=sum(diag(tbl_svmP_2))/length(trainclass)

# DECISION TREE

model_dt <- train(trainclass~., data =d,  method = "rpart",
                  trControl = trainControl("cv", number = 10))

model_dt$bestTune
predictions_dt_test = predict(model_dt,testdata)
#MSE_test_dt = mean(predictions_dt_test - testclass)^2
tbl_dt_1=table(predictions_dt_test, testclass)
acc_dt_test=sum(diag(tbl_dt_1))/length(testclass)

predictions_dt_train = predict(model_dt,traindata)
#MSE_train_dt = mean(predictions_dt_train - trainclass)^2
tbl_dt_2=table(predictions_dt_train, trainclass)
acc_dt_train=sum(diag(tbl_dt_2))/length(trainclass)

#RANDOM FOREST

# d =cbind(trainclass,traindata)
model_rf <- train(trainclass~., data =d,  method = "rf",
                  trControl = trainControl("cv", number = 10))

model_rf$bestTune
predictions_rf_test = predict(model_rf,testdata)
#MSE_test_rf = mean(predictions_rf_test - testclass)^2
tbl_rf_1=table(predictions_rf_test, testclass)
acc_rf_test=sum(diag(tbl_rf_1))/length(testclass)

predictions_rf_train = predict(model_rf,traindata)
#MSE_train_rf = mean(predictions_rf_train - trainclass)^2
tbl_rf_2=table(predictions_rf_train, trainclass)
acc_rf_train=sum(diag(tbl_rf_2))/length(trainclass)

# XGBOOST

model_xg <- train(trainclass~., data =d,  method = "xgbTree",
               trControl = trainControl("cv", number = 10)
)
model_xg$bestTune
predictions_xg_test = predict(model_xg,testdata)
#MSE_test_xg = mean(predictions_xg - testclass)^2
tbl_xg_1=table(predictions_xg_test, testclass)
acc_xg_test=sum(diag(tbl_xg_1))/length(testclass)

predictions_xg_train = predict(model_xg,traindata)
#MSE_train_xg = mean(predictions_xg_train - trainclass)^2
tbl_xg_2=table(predictions_xg_train, trainclass)
acc_xg_train=sum(diag(tbl_xg_2))/length(trainclass)
