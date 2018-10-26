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

#add the scores up
matches_played[,totscore:= (as.numeric(score1)+as.numeric(score2))]
matches_played[,IsOver:=as.numeric(totscore>2)]
match_scores = matches_played[,c("leagueId","home", "away","score","date","score1","score2"):=NULL]

## BET355
data_1 =odd_details[bookmaker=='bet365']
# data_x =odd_details[bookmaker=='bet365'& betType=='dc']

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

merged_all_1=merge(merged_1,match_scores,by='matchId')

#PCA ANALYSIS
pca=princomp(merged_all_1[,2:13],cor = TRUE)
plot(pca, main = 'Bet365 odds - PCA analysis')

str(pca)
summary(pca,loadings=T)

# #first component
# barplot(pca$scores[,1])
# 
# #second component
# barplot(pca$scores[,2])

biplot(pca, main="Bet365 - PCA - Biplot")
plot(pca$scores[,1],pca$scores[,2],col=merged_all_1$IsOver+1,pch=".",cex=5, main = "Bet365 - PCA Analysis - Over/Under 2.5")
legend(-12, 12, legend=c("UNDER 2.5", "OVER 2.5"),
       col=c(1, 2), pch=".", cex=0.8, pt.cex=5)

require(rgl)
#plot3d(pca$scores[,1:3], col=merged_all_1$totscore+1)
plot3d(pca$scores[,1:3], col=merged_all_1$IsOver+1)

summary(pca$scores)
#cor(pca$scores)

summary(merged_1[,2:13])
cor(merged_1[,2:13])
#options(max.print = 100)

# MDS 

#MANHATTAN DISTANCE

distance.matrix_m <- dist(scale((merged_1[,2:13]), center=TRUE, scale=TRUE),
                          method="manhattan")

## do the MDS math (this is basically eigen value decomposition)
mds.stuff_m <- cmdscale(distance.matrix_m, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per_m <- round(mds.stuff_m$eig/sum(mds.stuff_m$eig)*100, 1)
mds.var.per_m

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values_m <- mds.stuff_m$points
plot(mds.values_m[,1],mds.values_m[,2],xlab='', ylab='',col=1, pch='.',cex=5, main = "Bet365 - MDS plot using Manhattan distance")
# merged_1_try = merged_1[,matchId:=NULL]
# distances_m=dist(merged_1[,2:13], method = "manhattan", diag = TRUE, upper = TRUE)
# distances_m[is.na(distances_m)]=0
# mds_m=cmdscale(distances_m,eig=TRUE)
# plot(mds_m$points[,1],mds_m$points[,2],xlab='', ylab='',col=1, pch='.',cex=5, main = "Bet365 - MDS plot using Manhattan distance")
# text(mds[,1],mds[,2],cex = .75,pos=4,label="*")

# distances_e=dist(merged_1[,2:13], method = "euclidean", diag = TRUE, upper = TRUE)
# distances_e[is.na(distances_e)]=0
# mds_e=cmdscale(distances_e,eig=TRUE)
# plot(mds_e$points[,1],mds_e$points[,2],xlab='', ylab='',col=1, main = "Bet365 - MDS plot using Euclidean distance")

#x=sum(mds_e$eig[1])/sum(mds_e$eig)

# euclideanMDS=dist(mds)
# sum((as.matrix(euclideanMDS)-as.matrix(distances))^2)

#EUCLIDEAN DISTANCE
distance.matrix <- dist(scale((merged_1[,2:13]), center=TRUE, scale=TRUE),
                        method="euclidean")

## do the MDS math (this is basically eigen value decomposition)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.var.per

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(X=mds.values[,1],
                       Y=mds.values[,2])
mds.data
plot(mds.values[,1],mds.values[,2],pch='.',cex=5, main="Bet365 - MDS plot using Euclidean distance",xlab='', ylab='',col=1)
