require(data.table)
require(anytime)
#require(dplyr)
#require(plotly)

#read data 
matches = readRDS("C:/Users/09172/Desktop/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
odd_details = readRDS("C:/Users/09172/Desktop/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds")

data.table(matches)
data.table(odd_details)

matches=unique(matches)
odd_details =unique(odd_details)

matches[,type:=NULL] #deleted since all are soccer

#filter na scores
matches[is.na(score)]

#omit the rows with score = NA & divide the score into 2
matches_played = na.omit(matches,cols = "score") 
matches_played[,c("score1","score2"):=tstrsplit(score,':')]
matches$score1=as.numeric(matches$score1)
matches$score2=as.numeric(matches$score2)


#MATCH OUTCOMES
matches_played[,Outcome:=0]
matches_played[score1>score2,Outcome:=1] #HOME
matches_played[score1==score2,Outcome:=2] #TIE
matches_played[score1<score2,Outcome:=3] #AWAY
matches_outcome = matches_played[,c("leagueId","home", "away","score","date","score1","score2"):=NULL]


## PINNACLE
data_1 =odd_details[bookmaker=='Pinnacle']
# data_x =odd_details[bookmaker=='Pinnacle'& betType=='dc']

#order data in ascending date
odds_1=data_1[order(matchId,oddtype,bookmaker,date)]
# ou bets are evaluated separately
odds_1=odds_1[betType!='ou']
# only total handicap == 2.5 is taken into account
odds_2.5_1 =data_1[totalhandicap=='2.5']

#final odds were taken into account

odds_final_1=odds_1[,list(final_odd=odd[.N]),
                          by=list(matchId,oddtype,bookmaker)]
wide_odds_final_1=dcast(odds_final_1,
                        matchId~oddtype+bookmaker,
                        value.var='final_odd')

odds_2.5_final_1=odds_2.5_1[,list(final_odd=odd[.N]),
                    by=list(matchId,oddtype,bookmaker)]

wide_odds_2.5_final_1=dcast(odds_2.5_final_1,
                        matchId~oddtype+bookmaker,
                        value.var='final_odd')

merged_1=merge(wide_odds_final_1,wide_odds_2.5_final_1,by='matchId')
merged_1=merged_1[complete.cases(merged_1)]

merged_all_1=merge(merged_1,matches_outcome,by='matchId')


#PCA ANALYSIS
pca1=princomp(merged_all_1[,2:8],cor = TRUE)
plot(pca1, main = 'Pinnacle odds - PCA analysis')

str(pca1)
summary(pca1,loadings=T)

biplot(pca1)
plot(pca1$scores[,1],pca1$scores[,2],col=merged_all_1$Outcome,pch=".",cex=5, main = 'Pinnacle odds - PCA analysis by Match Outcomes')
legend(-7, 8, legend=c("HOME", "TIE", "AWAY"),
       col=c(1, 2,3), pch=".", cex=0.8, pt.cex=5)


require(rgl)
plot3d(pca1$scores[,1:3], col=merged_all_1$Outcome)

summary(pca1$scores)
cor(pca1$scores)

summary(merged_1[,2:8])
cor(merged_1[,2:8])


## BET365
data_2 =odd_details[bookmaker=='bet365']
# data_x =odd_details[bookmaker=='Pinnacle'& betType=='dc']

#order data in ascending date
odds_2=data_2[order(matchId,oddtype,bookmaker,date)]
# ou bets are evaluated separately
odds_2=odds_2[betType!='ou']
# only total handicap == 2.5 is taken into account
odds_2.5_2 =data_2[totalhandicap=='2.5']

#final odds were taken into account

odds_final_2=odds_2[,list(final_odd=odd[.N]),
                    by=list(matchId,oddtype,bookmaker)]
wide_odds_final_2=dcast(odds_final_2,
                        matchId~oddtype+bookmaker,
                        value.var='final_odd')

odds_2.5_final_2=odds_2.5_2[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]

wide_odds_2.5_final_2=dcast(odds_2.5_final_2,
                            matchId~oddtype+bookmaker,
                            value.var='final_odd')

merged_2=merge(wide_odds_final_2,wide_odds_2.5_final_2,by='matchId')
merged_2=merged_2[complete.cases(merged_2)]

merged_all_2=merge(merged_2,matches_outcome,by='matchId')


#PCA ANALYSIS
pca2=princomp(merged_all_2[,2:13],cor = TRUE)
plot(pca2, main = 'Bet365 odds - PCA analysis')

str(pca2)
summary(pca2,loadings=T)


biplot(pca2)
plot(pca2$scores[,1],pca2$scores[,2],col=merged_all_2$Outcome,pch=".",cex=5, main = 'Bet365 odds - PCA analysis by Match Outcomes')
legend(-12, 12, legend=c("HOME", "TIE", "AWAY"),
       col=c(1, 2,3), pch=".", cex=0.8, pt.cex=5)


require(rgl)
plot3d(pca2$scores[,1:3], col=merged_all_2$Outcome)

summary(pca2$scores)
cor(pca2$scores)

summary(merged_2[,2:13])
cor(merged_2[,2:13])

## BWIN
data_3 =odd_details[bookmaker=='bwin']
# data_x =odd_details[bookmaker=='Pinnacle'& betType=='dc']

#order data in ascending date
odds_3=data_3[order(matchId,oddtype,bookmaker,date)]
# ou bets are evaluated separately
odds_3=odds_3[betType!='ou']
# only total handicap == 2.5 is taken into account
odds_2.5_3 =data_3[totalhandicap=='2.5']

#final odds were taken into account

odds_final_3=odds_3[,list(final_odd=odd[.N]),
                    by=list(matchId,oddtype,bookmaker)]
wide_odds_final_3=dcast(odds_final_3,
                        matchId~oddtype+bookmaker,
                        value.var='final_odd')

odds_2.5_final_3=odds_2.5_3[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]

wide_odds_2.5_final_3=dcast(odds_2.5_final_3,
                            matchId~oddtype+bookmaker,
                            value.var='final_odd')

merged_3=merge(wide_odds_final_3,wide_odds_2.5_final_3,by='matchId')
merged_3=merged_3[complete.cases(merged_3)]

merged_all_3=merge(merged_3,matches_outcome,by='matchId')


#PCA ANALYSIS
pca3=princomp(merged_all_3[,2:13],cor = TRUE)
plot(pca3, main = 'Bwin odds - PCA analysis')

str(pca3)
summary(pca3,loadings=T)

biplot(pca3)
plot(pca3$scores[,1],pca3$scores[,2],col=merged_all_3$Outcome,pch=".",cex=5, main = 'Bwin odds - PCA analysis by Match Outcomes')
legend(-12, 12, legend=c("HOME", "TIE", "AWAY"),
       col=c(1, 2, 3), pch=".", cex=0.8, pt.cex=5)

require(rgl)
plot3d(pca3$scores[,1:3], col=merged_all_3$Outcome)

## YOUWIN
data_4 =odd_details[bookmaker=='youwin']
# data_x =odd_details[bookmaker=='Pinnacle'& betType=='dc']

#order data in ascending date
odds_4=data_4[order(matchId,oddtype,bookmaker,date)]
# ou bets are evaluated separately
odds_4=odds_4[betType!='ou']
# only total handicap == 2.5 is taken into account
odds_2.5_4 =data_4[totalhandicap=='2.5']

#final odds were taken into account

odds_final_4=odds_4[,list(final_odd=odd[.N]),
                    by=list(matchId,oddtype,bookmaker)]
wide_odds_final_4=dcast(odds_final_4,
                        matchId~oddtype+bookmaker,
                        value.var='final_odd')

odds_2.5_final_4=odds_2.5_4[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]

wide_odds_2.5_final_4=dcast(odds_2.5_final_4,
                            matchId~oddtype+bookmaker,
                            value.var='final_odd')

merged_4=merge(wide_odds_final_4,wide_odds_2.5_final_4,by='matchId')
merged_4=merged_4[complete.cases(merged_4)]

merged_all_4=merge(merged_4,matches_outcome,by='matchId')


#PCA ANALYSIS
pca4=princomp(merged_all_4[,2:13],cor = TRUE)
plot(pca4, main = 'Youwin odds - PCA analysis')

str(pca4)
summary(pca4,loadings=T)

biplot(pca4)
plot(pca4$scores[,1],pca4$scores[,2],col=merged_all_4$Outcome,pch=".",cex=5, main = 'Youwin odds - PCA analysis by Match Outcomes')
legend(-11, 8, legend=c("HOME", "TIE", "AWAY"),
       col=c(1, 2,3), pch=".", cex=0.8, pt.cex=5)

require(rgl)
plot3d(pca4$scores[,1:3], col=merged_all_4$Outcome)

## COMEON
data_5 =odd_details[bookmaker=='ComeOn']
# data_x =odd_details[bookmaker=='Pinnacle'& betType=='dc']

#order data in ascending date
odds_5=data_5[order(matchId,oddtype,bookmaker,date)]
# ou bets are evaluated separately
odds_5=odds_5[betType!='ou']
# only total handicap == 2.5 is taken into account
odds_2.5_5 =data_5[totalhandicap=='2.5']

#final odds were taken into account

odds_final_5=odds_5[,list(final_odd=odd[.N]),
                    by=list(matchId,oddtype,bookmaker)]
wide_odds_final_5=dcast(odds_final_5,
                        matchId~oddtype+bookmaker,
                        value.var='final_odd')

odds_2.5_final_5=odds_2.5_5[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]

wide_odds_2.5_final_5=dcast(odds_2.5_final_5,
                            matchId~oddtype+bookmaker,
                            value.var='final_odd')

merged_5=merge(wide_odds_final_5,wide_odds_2.5_final_5,by='matchId')
merged_5=merged_5[complete.cases(merged_5)]

merged_all_5=merge(merged_5,matches_outcome,by='matchId')


#PCA ANALYSIS
pca5=princomp(merged_all_5[,2:13],cor = TRUE)
plot(pca5, main = 'ComeOn odds - PCA analysis')

str(pca5)
summary(pca5,loadings=T)

biplot(pca5)
plot(pca5$scores[,1],pca5$scores[,2],col=merged_all_5$Outcome,pch=".",cex=5, main = 'ComeOn odds - PCA analysis by Match Outcomes')
legend(-13, 12, legend=c("HOME", "TIE", "AWAY"),
       col=c(1, 2,3), pch=".", cex=0.8, pt.cex=5)

require(rgl)
plot3d(pca5$scores[,1:3], col=merged_all_5$Outcome)
