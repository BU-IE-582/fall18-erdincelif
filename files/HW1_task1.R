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

#convert the date from epoch 
#matches[,match_date:=anydate(date)]
matches[,match_time:=anytime(date)]
matches[,Year:=year(match_time)]
# matches[,Month:=month(match_time)]
# matches[,Weekday:=day(match_time)]
# matches[,Hour:=hour(match_time)]

#filter na scores
matches[is.na(score)]

#omit the rows with score = NA & divide the score into 2
matches_played = na.omit(matches,cols = "score") 
matches_played[,c("score1","score2"):=tstrsplit(score,':')]

#add the scores up
matches_played[,totscore:= (as.numeric(score1)+as.numeric(score2))]
matches_played[,IsOver:=as.numeric(totscore>2)]

#filter over under 2.5
data =odd_details[betType=='ou' & totalhandicap=='2.5']
data[,totalhandicap:=NULL]

#order data in ascending date
odds_ov_un=data[order(matchId,oddtype,bookmaker,date)]

odds_ov_un_initial=data[,list(start_odd=odd[1]),
                              by=list(matchId,oddtype,bookmaker)]

odds_ov_un_final=data[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]

#transform to wide format            
wide_odds_initial=dcast(odds_ov_un_initial,
                        matchId~oddtype+bookmaker,
                        value.var='start_odd')

## PINNACLE 
#get pinnacle initial over under odds
pinnacle_over_under=odds_ov_un_initial[bookmaker=='Pinnacle']

pinnacle_wide=dcast(pinnacle_over_under,
                    matchId~oddtype,
                    value.var='start_odd')

# join odds with matches
merged_matches1=merge(matches_played,pinnacle_wide,by='matchId')

#setkey(matches,matchId)
#setkey(pinnacle_wide,matchId)
#joined_matches=pinnacle_wide[matches]

merged_matches1[,probOver:=1/over]
merged_matches1[,probUnder:=1/under]

merged_matches1[,totalProb:=probOver+probUnder]

merged_matches1[,probOver:=probOver/totalProb]
merged_matches1[,probUnder:=probUnder/totalProb]

merged_matches1=merged_matches1[complete.cases(merged_matches1)]
merged_matches1[,totalProb:=NULL]


cutpoints=seq(0,1,0.05)
merged_matches1[,odd_cut_over:=cut(probOver,cutpoints)]

summary_table1=merged_matches1[,list(empirical_over=mean(IsOver),
                                   probabilistic_over=mean(probOver),.N),
                                   by=list(Year,odd_cut_over)]

# list(Year) eklersen yillara göre de böler

summary_table1=summary_table1[order(Year)]

# plot(summary_table1[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),
#      xlab='Empirical Over', ylab='Probabilistic Over', main="Pinnacle - Over Bets - Version 1")
# abline(0,1,col='red')

plot(summary_table1[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),cex=sqrt(summary_table1$N),
     xlab='Empirical Over', ylab='Probabilistic Over', main="Pinnacle - Over Bets - All")
abline(0,1,col='red')

summary_table1_v2 =summary_table1[N>=10]
plot(summary_table1_v2[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),cex=sqrt(summary_table1_v2$N),
     xlab='Empirical Over', ylab='Probabilistic Over', main="Pinnacle - Over Bets - N>10")
abline(0,1,col='red')

## BET365
#get bet365 initial over under odds
bet365_over_under=odds_ov_un_initial[bookmaker=='bet365']

bet365_wide=dcast(bet365_over_under,
                    matchId~oddtype,
                    value.var='start_odd')

# join odds with matches
merged_matches2=merge(matches_played,bet365_wide,by='matchId')

merged_matches2[,probOver:=1/over]
merged_matches2[,probUnder:=1/under]

merged_matches2[,totalProb:=probOver+probUnder]

merged_matches2[,probOver:=probOver/totalProb]
merged_matches2[,probUnder:=probUnder/totalProb]

merged_matches2=merged_matches2[complete.cases(merged_matches2)]
merged_matches2[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
merged_matches2[,odd_cut_over:=cut(probOver,cutpoints)]

summary_table2=merged_matches2[,list(empirical_over=mean(IsOver),
                                     probabilistic_over=mean(probOver),.N),
                               by=list(Year,odd_cut_over)]


summary_table2=summary_table2[order(Year)]

plot(summary_table2[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),cex=sqrt(summary_table2$N),
     xlab='Empirical Over', ylab='Probabilistic Over', main="Bet365 - Over Bets - All")
abline(0,1,col='red')

summary_table2_v2 =summary_table2[N>=10]
plot(summary_table2_v2[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),cex=sqrt(summary_table2_v2$N),
     xlab='Empirical Over', ylab='Probabilistic Over', main="Bet365 - Over Bets - N>10")
abline(0,1,col='red')

## BWIN
#get bwin initial over under odds
bwin_over_under=odds_ov_un_initial[bookmaker=='bwin']

bwin_wide=dcast(bwin_over_under,
                  matchId~oddtype,
                  value.var='start_odd')

# join odds with matches
merged_matches3=merge(matches_played,bwin_wide,by='matchId')

merged_matches3[,probOver:=1/over]
merged_matches3[,probUnder:=1/under]

merged_matches3[,totalProb:=probOver+probUnder]

merged_matches3[,probOver:=probOver/totalProb]
merged_matches3[,probUnder:=probUnder/totalProb]

merged_matches3=merged_matches3[complete.cases(merged_matches3)]
merged_matches3[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
merged_matches3[,odd_cut_over:=cut(probOver,cutpoints)]

summary_table3=merged_matches3[,list(empirical_over=mean(IsOver),
                                     probabilistic_over=mean(probOver),.N),
                               by=list(Year,odd_cut_over)]


summary_table3=summary_table3[order(Year)]

plot(summary_table3[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),cex=sqrt(summary_table3$N),
     xlab='Empirical Over', ylab='Probabilistic Over', main="bwin - Over Bets - All")
abline(0,1,col='red')

summary_table3_v2 =summary_table3[N>=10]
plot(summary_table3_v2[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),cex=sqrt(summary_table3_v2$N),
     xlab='Empirical Over', ylab='Probabilistic Over', main="bwin - Over Bets - N>10")
abline(0,1,col='red')

## COMEON
#get bwin initial over under odds
comeon_over_under=odds_ov_un_initial[bookmaker=='ComeOn']

comeon_wide=dcast(comeon_over_under,
                matchId~oddtype,
                value.var='start_odd')

# join odds with matches
merged_matches4=merge(matches_played,comeon_wide,by='matchId')

merged_matches4[,probOver:=1/over]
merged_matches4[,probUnder:=1/under]

merged_matches4[,totalProb:=probOver+probUnder]

merged_matches4[,probOver:=probOver/totalProb]
merged_matches4[,probUnder:=probUnder/totalProb]

merged_matches4=merged_matches4[complete.cases(merged_matches4)]
merged_matches4[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
merged_matches4[,odd_cut_over:=cut(probOver,cutpoints)]

summary_table4=merged_matches4[,list(empirical_over=mean(IsOver),
                                     probabilistic_over=mean(probOver),.N),
                               by=list(Year,odd_cut_over)]


summary_table4=summary_table4[order(Year)]

plot(summary_table4[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),cex=sqrt(summary_table4$N),
     xlab='Empirical Over', ylab='Probabilistic Over', main="ComeOn - Over Bets - All")
abline(0,1,col='red')

summary_table4_v2 =summary_table4[N>=10]
plot(summary_table4_v2[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),cex=sqrt(summary_table4_v2$N),
     xlab='Empirical Over', ylab='Probabilistic Over', main="ComeOn - Over Bets - N>10")
abline(0,1,col='red')


## YOUWIN
#get youwin initial over under odds
youwin_over_under=odds_ov_un_initial[bookmaker=='youwin']

youwin_wide=dcast(youwin_over_under,
                  matchId~oddtype,
                  value.var='start_odd')

# join odds with matches
merged_matches5=merge(matches_played,comeon_wide,by='matchId')

merged_matches5[,probOver:=1/over]
merged_matches5[,probUnder:=1/under]

merged_matches5[,totalProb:=probOver+probUnder]

merged_matches5[,probOver:=probOver/totalProb]
merged_matches5[,probUnder:=probUnder/totalProb]

merged_matches5=merged_matches5[complete.cases(merged_matches5)]
merged_matches5[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
merged_matches5[,odd_cut_over:=cut(probOver,cutpoints)]

summary_table5=merged_matches5[,list(empirical_over=mean(IsOver),
                                     probabilistic_over=mean(probOver),.N),
                               by=list(Year,odd_cut_over)]


summary_table5=summary_table5[order(Year)]

plot(summary_table5[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),cex=sqrt(summary_table5$N),
     xlab='Empirical Over', ylab='Probabilistic Over', main="Youwin - Over Bets - All")
abline(0,1,col='red')

summary_table5_v2 =summary_table5[N>=10]
plot(summary_table5_v2[,list(empirical_over,probabilistic_over)], xlim = c(0,1), ylim = c(0,1),cex=sqrt(summary_table5_v2$N),
     xlab='Empirical Over', ylab='Probabilistic Over', main="Youwin - Over Bets - N>10")
abline(0,1,col='red')


## PART 1.b
#Yearly change in Pinnacle odds (ou bettype & over odds)

cutpoints_b=seq(0,1,0.1)
merged_matches_b=merged_matches1[,odd_cut_over:=cut(probOver,cutpoints_b)]

summary_table_b=merged_matches_b[,list(empirical_over=mean(IsOver),
                                   probabilistic_over=mean(probOver),.N),
                             by=list(Year,odd_cut_over)]


summary_table_b=summary_table_b[order(Year)]
data03 =summary_table_b[odd_cut_over=="(0.3,0.4]"]
plot(data03[,list(Year,empirical_over)], ylim = c(0,1), main="bin=(0.3,0.4]", ylab='Probabilities', col="red")
par(new=TRUE)
plot(data03[,list(Year, probabilistic_over)],ylab='', ylim = c(0,1))
legend(2016,1, c("Empirical Over","Probabilistic Over"), pch =1, col =c("black","red"))

data04 =summary_table_b[odd_cut_over=="(0.4,0.5]"]
plot(data04[,list(Year,empirical_over)], ylim = c(0,1), main="bin=(0.4,0.5]", ylab='Probabilities', col="red")
par(new=TRUE)
plot(data04[,list(Year, probabilistic_over)],ylab='', ylim = c(0,1))
legend(2016,1, c("Empirical Over","Probabilistic Over"), pch =1, col =c("black","red"))

data05 =summary_table_b[odd_cut_over=="(0.5,0.6]"]
plot(data05[,list(Year,empirical_over)], ylim = c(0,1), main="bin=(0.5,0.6]", ylab='Probabilities', col="red")
par(new=TRUE)
plot(data05[,list(Year, probabilistic_over)],ylab='', ylim = c(0,1))
legend(2016,1, c("Empirical Over","Probabilistic Over"), pch =1, col =c("black","red"))

data06 =summary_table_b[odd_cut_over=="(0.6,0.7]"]
plot(data06[,list(Year,empirical_over)], ylim = c(0,1), main="bin=(0.6,0.7]", ylab='Probabilities', col="red")
par(new=TRUE)
plot(data06[,list(Year, probabilistic_over)],ylab='', ylim = c(0,1))
legend(2016,1, c("Empirical Over","Probabilistic Over"), pch =1, col =c("black","red"))
