train_xgboost <- function(train_features, test_features,not_included_feature_indices=c(1:5),nFolds=10,trace=T){
  
  set.seed(1)
  
  # glmnet works with complete data
  xg_features=train_features[complete.cases(train_features)]
  xg_train_class<-xg_features$Match_Result
  xg_features <-as.data.table(xg_features)
  xg_train_data=xg_features[,-not_included_feature_indices,with=F]
  xg_test_data=test_features[,-not_included_feature_indices,with=F]
  
  library(caret)
  
  d<-cbind(xg_train_class,xg_train_data)
  model<-train(xg_train_class~.,data=d,method="xgbTree",
               trControl=trainControl("cv",number=nFolds))
  predicted_probabilities=predict(model, as.matrix(xg_test_data), type = "prob")

  k<-predicted_probabilities[,2]
  l<-predicted_probabilities[,3]
  m<-predicted_probabilities[,1]
  predicted_probabilities<-cbind(k,l,m)
  final_result=data.table(test_features[,list(matchId,Match_Result)],predicted_probabilities)
  return(list(predictions=final_result,varImp=varImp(model)))
}
