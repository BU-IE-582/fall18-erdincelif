require(scatterplot3d)
require(plot3D)
require(distances)
require(FNN)
require(glmnet)
require(TunePareto)
require(class)
require(knnGarden)
# require(caret)
require(tictoc)
require(data.table)
# require(KODAMA)
# require(bigmemory)
#require(readtext)
require(matrixStats)

#install.packages("caret", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com",dependencies=TRUE))
#install.packages("knnflex", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com",dependencies=TRUE))

xtrain=read.table('C:/Users/09172/Desktop/UWave/uWaveGestureLibrary_X_TRAIN')
ytrain=read.table('C:/Users/09172/Desktop/UWave/uWaveGestureLibrary_Y_TRAIN')
ztrain=read.table('C:/Users/09172/Desktop/UWave/uWaveGestureLibrary_Z_TRAIN')

xtest=read.table('C:/Users/09172/Desktop/UWave/uWaveGestureLibrary_X_TEST')
ytest=read.table('C:/Users/09172/Desktop/UWave/uWaveGestureLibrary_Y_TEST')
ztest=read.table('C:/Users/09172/Desktop/UWave/uWaveGestureLibrary_Z_TEST')

# PART A
# VISUALIZATION OF THE FIRTS INSTANCE
colors= 2:ncol(xtrain)
xtrain[1,1]
scatter3D(as.numeric(xtrain[1,2:316]),as.numeric(ytrain[1,2:316]),as.numeric(ztrain[1,2:316]), colvar = colors, main="Row #1 Class 6")

# VISUALIZATION OF INSTANCES FROM EACH CLASS
class=xtrain[,1]
classtest=xtest[,1]
summary(class)
unique_class=unique(class)
par(mfrow=c(1,1))
for(i in unique_class){
	ind=which(class==i)
	selected=sample(ind,1)
	x=cumsum(xtrain[selected,2:ncol(xtrain)])
	y=cumsum(ytrain[selected,2:ncol(ytrain)])
	z=cumsum(ztrain[selected,2:ncol(ztrain)])
	scatter3D(as.numeric(x),as.numeric(y),as.numeric(z), colvar=colors, main=paste("Row #",selected,"Class",i))
}

# PART B

# Combination of 3 coordinates
training_combined=cbind(xtrain[,1:316],ytrain[,2:316],ztrain[,2:316])
test_combined=cbind(xtest[,1:316],ytest[,2:316],ztest[,2:316])
combined=rbind(training_combined,test_combined)

#distMatrix=distances(training_combined[,2:ncol(training_combined)])
# print(str(distMatrix))
# distMatrix=as.matrix(distMatrix)
# neighborhood=order(distMatrix[1,]) 

#x=nearest_neighbor_search(distMatrix, 10)

traindata = as.matrix(training_combined)
traindata=training_combined[,2:ncol(training_combined)]
trainclass=as.factor(training_combined[,1])
testclass=as.factor(test_combined[,1])

k_levels=c(1,3,5,10,20,30,50)
thresHold=0.5
nofReplications=1
nFolds=10
indices=generateCVRuns(trainclass,nofReplications,nFolds,stratified=TRUE)

cvresult=data.table()
for(i in 1:nofReplications) {
  thisReplication=indices[[i]]
  for(j in 1:nFolds){
    testindices=thisReplication[[j]]
    
    cvtrain=traindata[-testindices,]        
    cvtest=traindata[testindices,]
    
    cvtrain=data.frame(Class=trainclass[-testindices],cvtrain)
    logreg=glm(Class~.,cvtrain,family='binomial')
    pred_logreg=predict(logreg,data.frame(cvtest),type='response')
    
    if(nrow(cvresult)==0){
      cvresult=data.table(Replication=i,Fold=j,Method='Logistic',Klev=NA,TestId=testindices,
                          Predictions=as.numeric(pred_logreg>thresHold),Real=trainclass[testindices])
    } else {
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='Logistic',Klev=NA,TestId=testindices,
                                         Predictions=as.numeric(pred_logreg>thresHold),Real=trainclass[testindices]))
    }
    cvtrain=traindata[-testindices,]        
    cvtest=traindata[testindices,]
    
    for(y in 1:length(k_levels)){
      param_k=k_levels[y]
      predict_knn=knn(cvtrain, cvtest,trainclass[-testindices], k = param_k)
      cvresult=rbind(cvresult,data.table(Replication=i,Fold=j,Method='knn',Klev=param_k,TestId=testindices,
                                         Predictions=as.numeric(as.character(predict_knn)),Real=trainclass[testindices]))
    }   
  }    
}


cvresult[,list(Accu=mean(Predictions==Real)),by=list(Method,Klev)]


# Alternative k-value evaluation

accuracy <- rep(0, 10)
k <- 1:10
for(x in k){
  prediction <- knn(training_combined[,2:ncol(training_combined)], test_combined[,2:ncol(test_combined)],
                    training_combined[,1], k = x)
  accuracy[x] <- mean(prediction == test_combined[,1])
}

plot(k, accuracy, type = 'b')


# # DENEME
# k_dist_euc=knn.dist(training_combined[,2:ncol(training_combined)], k=10)
# 
# preds = knn.predict(training_combined[,2:ncol(training_combined)], test_combined[,2:ncol(test_combined)], trainclass ,k_dist_euc, k=1, agg.meth="majority")
# 
# 
# #DENEME 2
# 
# require(ecodist)
# 
# knn_def <- function(xmat, k,method){
#   n <- nrow(xmat)
#   if (n <= k) stop("k can not be more than n-1")
#   neigh <- matrix(0, nrow = n, ncol = k)
#   for(i in 1:n) {
#     ddist<- distance(xmat, method)  
#     neigh[i, ] <- order(ddist)[2:(k + 1)]
#   }
#   return(neigh)
# }
# wdbc_nn =knn_def(as.matrix(training_combined[,2:ncol(training_combined)]),k=1,method="euclidean")
# 
# #
# 
# PART C
# Using Euclidean Distances
require(kknn)
tic()
predictions_euc= kknn(formula = class~., training_combined[,2:ncol(training_combined)], test_combined[,2:ncol(test_combined)], k = 3, distance = 2)
euc=rowMedians(predictions_euc$CL)
tbl_euc=table(euc,combined[897:4478,1])
sum_euc=sum(diag(tbl_euc))
acc_euc=sum_euc/length(testclass)
toc()

# Using Manhattan Distances
tic()
predictions_man= kknn(formula = class~., training_combined[,2:ncol(training_combined)], test_combined[,2:ncol(test_combined)], k = 3, distance = 1)
man=rowMedians(predictions_man$CL)
tbl_man=table(man,combined[897:4478,1])
sum_man=sum(diag(tbl_man))
acc_man=sum_man/length(testclass)
toc()

# trainclass=xtrain[,1]
# data.table(trainclass)
# bir=trainclass[trainclass==1]
# iki=trainclass[trainclass==2]
# uc=trainclass[trainclass==3]
# dort=trainclass[trainclass==4]
# bes=trainclass[trainclass==5]
# alti=trainclass[trainclass==6]
# yedi=trainclass[trainclass==7]
# sekiz=trainclass[trainclass==8]
# 
# cl=factor(c(rep("1",122), rep("2",108), rep("3",106), rep("4",110), rep("5",127), rep("6",111), rep("7",112), rep("8",100)))
# 
# tic()
# pred=knn(combined[1:896,],combined[897:4478,],combined[1:896,1],k=21)
# #pred=knn(training_combined,test_combined,cl,k=21,prob=TRUE)
# table(pred,combined[897:4478,1])
# toc()
# 
# pred2=knn(combined[1:896,],combined[897:4478,],combined[1:896,1],k=10)
# #pred=knn(training_combined,test_combined,cl,k=21,prob=TRUE)
# table(pred2,combined[897:4478,1])
# 
# pred3=knn(combined[1:896,],combined[897:4478,],combined[1:896,1],k=3)
# #pred=knn(training_combined,test_combined,cl,k=21,prob=TRUE)
# table(pred3,combined[897:4478,1])
# 
# # testclass=xtest[,1]
# 
# all.equal(pred,as.factor(testclass))
# 
# source("https://bioconductor.org/biocLite.R")
# biocLite("Biobase")
# 
# trControl <- trainControl(method  = "cv",
#                           number  = 10)
# 
# fit <- train(Species ~ .,
#              method     = "knn",
#              tuneGrid   = expand.grid(k = 1:10),
#              trControl  = trControl,
#              metric     = "Accuracy",
#              data       = combined[1:896,])
# 
# library(DAAG)
# cv.lm(data=combined, form.lm=mod1, m= 10, plotit = F) 

