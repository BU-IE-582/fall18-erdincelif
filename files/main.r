require(data.table)
require(TunePareto)

setwd('C:\\Users\\10025\\Desktop\\IE582\\proje\\')

testStart=as.Date('2018-11-29')
trainStart=as.Date('2013-08-10')
rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold

source('data_preprocessing.r')
source('feature_extraction.r')
source('performance_metrics.r')
source('train_models.r')
source('xgfunc.r')
source("rffunc.r")

matches_data_path='C:\\Users\\10025\\Desktop\\IE582\\proje/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_data_path='C:\\Users\\10025\\Desktop\\IE582\\proje/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

# read data
matches_raw=readRDS(matches_data_path)
odd_details_raw=readRDS(odd_details_data_path)

# preprocess matches
matches=matches_data_preprocessing(matches_raw)

# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches,which_bets=c('1x2'),remove_bookmaker=c('BetfairExchange','PaddyPower'),removeOlderThan=30)

# extract open and close odd type features from multiple bookmakers
features=extract_features.openclose(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)

####gjghj####
matches$Home <- gsub('manchester-united', 'manchester-utd', matches$Home)
matches$Away <- gsub('manchester-united', 'manchester-utd', matches$Away)

matches$Home <- gsub('crystal palace', 'crystal-palace', matches$Home)
matches$Away <- gsub('crystal palace', 'crystal-palace', matches$Away)

matches$Home <- gsub('manchester city', 'manchester-city', matches$Home)
matches$Away <- gsub('manchester city', 'manchester-city', matches$Away)

matches$Home <- gsub('manchester united', 'manchester-utd', matches$Home)
matches$Away <- gsub('manchester united', 'manchester-utd', matches$Away)

matches$Home <- gsub('newcastle utd', 'newcastle', matches$Home)
matches$Away <- gsub('newcastle utd', 'newcastle', matches$Away)

matches$Home <- gsub('stoke city', 'stoke', matches$Home)
matches$Away <- gsub('stoke city', 'stoke', matches$Away)

matches$Home <- gsub('west ham', 'west-ham', matches$Home)
matches$Away <- gsub('west ham', 'west-ham', matches$Away)

library(dplyr)
library(zoo)
homeaway<-matches[,c(2, 3, 4)]

feat<-merge(features,homeaway,by="matchId")

feat<-feat%>%arrange(desc(Match_Date))
t1<-feat[,c(1:3)]
t2<-merge(t1,matches,by="matchId")
t2<-t2%>%arrange(desc(Match_Date.x))

averageofhowmanygames<-5


t2 = t2 %>%
  group_by(Home) %>%
  arrange(Match_Date.x) %>%
  mutate(HHome_Score.lag = lag(Home_Score, n = 1),
         HResult_Home.lag = lag(Result_Home, n = 1),
         HResult_Tie.lag = lag(Result_Tie, n = 1),
         HResult_Away.lag = lag(Result_Away, n = 1),
         HAway_Score.lag = lag(Away_Score, n = 1))
t2<-t2%>%arrange(desc(Match_Date.x))
t2 = t2 %>%
  group_by(Away) %>%
  arrange(Match_Date.x) %>%
  mutate(AHome_Score.lag = lag(Home_Score, n = 1),
         AResult_Home.lag = lag(Result_Home, n = 1),
         AResult_Tie.lag = lag(Result_Tie, n = 1),
         AResult_Away.lag = lag(Result_Away, n = 1),
         AAway_Score.lag = lag(Away_Score, n = 1))
t2<-t2%>%arrange(desc(Match_Date.x))
df2 = t2 %>%
  group_by(Home) %>%
  arrange(Match_Date.x) %>%
  mutate(temp.1 = rollsum(x = HHome_Score.lag, averageofhowmanygames, align = "right", fill = NA),
         temp.2 = rollsum(x = HResult_Home.lag, averageofhowmanygames, align = "right", fill = NA),
         temp.3 = rollsum(x = HResult_Tie.lag, averageofhowmanygames, align = "right", fill = NA),
         temp.4 = rollsum(x = HResult_Away.lag, averageofhowmanygames, align = "right", fill = NA),
         temp.5 = rollsum(x = HAway_Score.lag, averageofhowmanygames, align = "right", fill = NA))
df2<-df2%>%arrange(desc(Match_Date.x))
df3 = df2 %>%
  group_by(Home) %>%
  arrange(Match_Date.x) %>%
  mutate(temp.6 = temp.2*3+temp.3) #should be 5 matches
df3<-df3%>%arrange(desc(Match_Date.x))
df4 = df3 %>%
  group_by(Away) %>%
  arrange(Match_Date.x) %>%
  mutate(temp.7 = rollsum(x = AAway_Score.lag, averageofhowmanygames, align = "right", fill = NA),
         temp.8 = rollsum(x = AResult_Away.lag, averageofhowmanygames, align = "right", fill = NA),
         temp.9 = rollsum(x = AResult_Tie.lag, averageofhowmanygames, align = "right", fill = NA),
         temp.10 = rollsum(x = AResult_Home.lag, averageofhowmanygames, align = "right", fill = NA),
         temp.11 = rollsum(x = AHome_Score.lag, averageofhowmanygames, align = "right", fill = NA))
df4<-df4%>%arrange(desc(Match_Date.x))
df5 = df4 %>%
  group_by(Away) %>%
  arrange(Match_Date.x) %>%
  mutate(temp.12 = temp.8*3+temp.9)
df5<-df5%>%arrange(desc(Match_Date.x))
df5<-df5[,-c(2,3,17:26)]

colnames(df5)<-c("matchId","LeaugeId","Home","Away","Match_Date","type","Match_Hour","Home_Score",
                 "Away_Score","Match_Result","Total_Score","Result_Home","Result_Tie","Result_Away",
                 "HomeScore","AwayScore","HomeWin","HomeTie","HomeLost","AwayLost","AwayTie","AwayWin",
                 "HomeConceded","AwayConceded","Last5Home","Last5Away")

df6<-df5[,c(1,15:26)]

feat<-merge(feat,df6,by="matchId")
feat<-as.data.frame(feat)
#features <- feat[,-c(ncol(feat),ncol(feat)-1)]

features <- feat[,-c(ncol(feat)-13,ncol(feat)-12)]
features<- as.data.table(features)

# divide data based on the provided dates 
train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
test_features=features[Match_Date>=testStart] 
test_features<-test_features[!is.na(test_features$HomeWin),]
test_features<-test_features[!is.na(test_features$AwayWin),]

library(glmnet) #Lasso and Elastic-Net Regularized Generalized Linear Models
a<-list()

# run glmnet on train data with tuning lambda parameter based on RPS and return predictions based on lambda with minimum RPS
predictions=train_glmnet(train_features, test_features,not_included_feature_indices=c(1:5), alpha=0.5,nlambda=50, tune_lambda=TRUE,nofReplications=2,nFolds=10,trace=T)
# subset of matches that are finished and in the test set
a[[1]]<-subset(predictions$predictions, !is.na(predictions$predictions$Match_Result))

# run xgboost from caret package
predictions_xg=train_xgboost(train_features, test_features,not_included_feature_indices=c(1:5),nFolds=10,trace=T)
# subset of matches that are finished and in the test set
a[[2]]<-subset(predictions_xg$predictions, !is.na(predictions_xg$predictions$Match_Result))

# run randomforest from caret package
predictions_rf=train_rfboost(train_features, test_features,not_included_feature_indices=c(1:5),nFolds=10,trace=T)
# subset of matches that are finished and in the test set
a[[3]]<-subset(predictions_rf$predictions, !is.na(predictions_rf$predictions$Match_Result))

predictions_xg$varImp
predictions_rf$varImp

probs<-list()
outcomes<-list()
RPSsingle<-list()
RPSmatrix<-list()
for (j in 1:length(a)) {
  
  probs[[j]] <- a[[j]][,c(3,4,5)]
  probs[[j]]<-as.matrix(probs[[j]])
  
  outcome <- matrix(0,nrow = nrow(a[[j]]),ncol = 3)
  for (i in 1 : nrow(a[[j]])) {
    raw_outcomes <- a[[j]][,c(2)]
    outcome[i,1] <- ifelse(raw_outcomes[i]=="Home",1,0)
    outcome[i,2] <- ifelse(raw_outcomes[i]=="Tie",1,0)
    outcome[i,3] <- ifelse(raw_outcomes[i]=="Away",1,0)
    outcomes[[j]]<-outcome
  }
  RPSsingle[[j]]=RPS_single(probs[[j]],outcomes[[j]])
  RPSmatrix[[j]]=RPS_matrix(probs[[j]],outcomes[[j]])
}

#add change names if any changes
RPSasMatrix<-bind_cols(meth=RPSmatrix[[1]],xgb=RPSmatrix[[2]],rf=RPSmatrix[[3]])
RPSasSingle<-bind_cols(glmnet=RPSsingle[[1]],xgb=RPSsingle[[2]],rf=RPSsingle[[3]])


#RPS
RPSasMatrix
RPSasSingle




# For plot and overalls
realall<-data.frame()
realall[1,1]<-mean(subset(df2, !is.na(df2$Match_Result))$Result_Home)
realall[1,2]<-mean(subset(df2, !is.na(df2$Match_Result))$Result_Tie)
realall[1,3]<-mean(subset(df2, !is.na(df2$Match_Result))$Result_Away)
colnames(realall)<-c("Home","Tie","Away")
realall


overallmeansofpreds<-data.frame()
subsettedmeansofpreds<-data.frame()
subsettedpreds<-data.frame()

for (i in 1 : length(a)) {
  colnames(a[[i]])<-c("matchId","Match_Result","Home","Tie","Away")
overallmeansofpreds[i,1]<-mean(a[[i]]$Home)
overallmeansofpreds[i,2]<-mean(a[[i]]$Tie)
overallmeansofpreds[i,3]<-mean(a[[i]]$Away)

subsettedmeansofpreds[i,1]<-mean(subset(a[[i]],a[[i]]$Match_Result=='Home')$Home)
subsettedmeansofpreds[i,2]<-mean(subset(a[[i]],a[[i]]$Match_Result=='Tie')$Tie)
subsettedmeansofpreds[i,3]<-mean(subset(a[[i]],a[[i]]$Match_Result=='Away')$Away)

}


for (i in 1:length(a)) {
  for (j in 1:nrow(RPSasMatrix)) {
    subsettedpreds[j,i]<-0
    subsettedpreds[j,i]<-ifelse(a[[i]][j,2]=='Home',a[[i]][j,3],subsettedpreds[j,i])
    subsettedpreds[j,i]<-ifelse(a[[i]][j,2]=='Tie',a[[i]][j,4],subsettedpreds[j,i])
    subsettedpreds[j,i]<-ifelse(a[[i]][j,2]=='Away',a[[i]][j,5],subsettedpreds[j,i])
  }
}



rownames(overallmeansofpreds)<-c("glmnet","XGBcaret","RFcaret")
colnames(overallmeansofpreds)<-c("Home","Tie","Away")
overallmeansofpreds

rownames(subsettedmeansofpreds)<-c("glmnet","XGBcaret","RFcaret")
colnames(subsettedmeansofpreds)<-c("Home","Tie","Away")
subsettedmeansofpreds

subsettedpreds<-cbind(subsettedpreds,a[[1]]$Match_Result)
colnames(subsettedpreds)<-c("glmnet","XGBcaret","RFcaret","Match_Result")
k<-seq(from=1, to=nrow(RPSasMatrix),by=1)

library(ggplot2)

ggplot(subsettedpreds, aes(k)) + 
  geom_line(aes(y = glmnet, colour = "glmnet",size=0.4)) + geom_point(aes(y = glmnet, colour = Match_Result,size=0.5)) +
  geom_line(aes(y = XGBcaret, colour = "XGBcaret",size=0.4)) + geom_point(aes(y = XGBcaret, colour = Match_Result,size=0.5)) +
  geom_line(aes(y = RFcaret, colour = "RFcaret",size=0.4)) +geom_point(aes(y = RFcaret, colour = Match_Result,size=0.5)) 

ggplot(subsettedpreds, aes(k)) + 
  geom_line(aes(y = glmnet, colour = "glmnet",size=1,alpha=0.5)) + 
  geom_line(aes(y = XGBcaret, colour = "XGBcaret",size=1,alpha=0.5)) +
  geom_line(aes(y = RFcaret, colour = "RFcaret",size=1,alpha=0.5)) 



#For conf matrix
predsout<-list()
predsout[[1]]<-data.frame(matrix(0,ncol=1,nrow=10))
predsout[[2]]<-data.frame(matrix(0,ncol=1,nrow=10))
predsout[[3]]<-data.frame(matrix(0,ncol=1,nrow=10))
b<-list()
for (i in 1:length(a)) {
  for (j in 1:nrow(RPSasMatrix)) {
      b[[i]]<-a[[i]][,c(3,4,5)]
      predsout[[i]][j,1]<-ifelse(max(b[[i]][j,])==a[[i]][j,3],"Home",predsout[[i]][j,1])
      predsout[[i]][j,1]<-ifelse(max(b[[i]][j,])==a[[i]][j,4],"Tie",predsout[[i]][j,1])
      predsout[[i]][j,1]<-ifelse(max(b[[i]][j,])==a[[i]][j,5],"Away",predsout[[i]][j,1])     
  }
}

library(caret)
glmnetconf<-confusionMatrix(as.factor(predsout[[1]][,1]), as.factor(a[[1]]$Match_Result))
xgboostconf<-confusionMatrix(as.factor(predsout[[2]][,1]), as.factor(a[[1]]$Match_Result))
rfconf<-confusionMatrix(as.factor(predsout[[3]][,1]), as.factor(a[[1]]$Match_Result))

glmnetconf$table
glmnetconf$overall
xgboostconf$table
rfconf$table

