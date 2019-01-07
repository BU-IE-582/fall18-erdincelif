train_rfboost <- function(train_features, test_features,not_included_feature_indices=c(1:5),nFolds=10,trace=T){
  
  set.seed(1)
  
  # glmnet works with complete data
  rf_features=train_features[complete.cases(train_features)]
  train_class=rf_features$Match_Result
  rf_features <-as.data.table(rf_features)
  rf_train_data=rf_features[,-not_included_feature_indices,with=F]
  rf_test_data=test_features[,-not_included_feature_indices,with=F]
  
  library(caret)
  
  d<-cbind(train_class,rf_train_data)
  model<-train(train_class~.,data=d,method="rf",
               trControl=trainControl("cv",number=nFolds), importance=TRUE)
  
  predicted_probabilities=predict(model, as.matrix(rf_test_data), type = "prob")
  
  k<-predicted_probabilities[,2]
  l<-predicted_probabilities[,3]
  m<-predicted_probabilities[,1]
  predicted_probabilities<-cbind(k,l,m)
  final_result=data.table(test_features[,list(matchId,Match_Result)],predicted_probabilities)
  
  return(list(predictions=final_result,varImp=varImp(model)))
}
