require(penalized)
install.packages("caret", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com",dependencies=TRUE))
require(caret)

train=read.table('C:/Users/09172/Desktop/ecgTRAIN')
test=read.table('C:/Users/09172/Desktop/ecgTEST')

trainclass=train[,1] # takes -1 and 1
trainclass[trainclass==-1]=0

testclass=test[,1] # takes -1 and 1
testclass[testclass==-1]=0

fit1 = optL1(trainclass, train[,2:ncol(train)],lambda2=0, fold=10)
#L2 = optL2(trainclass, train[,2:ncol(train)], fold=10)

pen=penalized(trainclass, penalized= train[,2:ncol(train)],lambda1=1.860641  ,
           lambda2=0, positive = FALSE, data=train, fusedl=TRUE,
           model = "logistic",
           standardize = FALSE, trace = TRUE)
str(pen)
pen@fitted
pen@penalized
pen@converged
pen@residuals
pen@formula$unpenalized

test_pred= predict(pen, test[,2:ncol(test)])
test_pred2=round(test_pred)

require(e1071)
r=round(pen@fitted)
unique(r)
all.equal(as.factor(trainclass),as.factor(r))

confusionMatrix(as.factor(test_pred2), as.factor(testclass), dnn = c("Prediction", "Reference"))


#PART C
data.frame(train)
train_new=train[,3:97]
for(i in 3:ncol(train)) {
  train_new[,i-2]=train[,i]-train[,i-1]
}

test_new=test[,1:95]
for(i in 3:ncol(test)) {
  test_new[,i-2]=test[,i]-test[,i-1]
}

fit2 = optL1(trainclass, train_new,lambda2=0, fold=10)
#L2 = optL2(trainclass, train[,2:ncol(train)], fold=10)

pen2=penalized(trainclass, penalized= train_new,lambda1=1.171004 ,
              lambda2=0, positive = FALSE, data=train, fusedl=TRUE,
              model = "logistic",
              standardize = FALSE, trace = TRUE)

test_pred_new= predict(pen2, test_new)
test_pred_new2=round(test_pred_new)

confusionMatrix(as.factor(test_pred_new2), as.factor(testclass), dnn = c("Prediction", "Reference"))

pen2@penalized
