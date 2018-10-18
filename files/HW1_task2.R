require(data.table)
#require(dplyr)
require(anytime)

#read data 
#matches = readRDS("C:/Users/09172/Desktop/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
odds = readRDS("C:/Users/09172/Desktop/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds")

#data.table(matches)
data.table(odds)

#matches=unique(matches)
odds =unique(odds)

odds[,type:=NULL] #deleted since all are soccer
odds[,match_time:=anytime(date)]
odds[,Year:=year(match_time)]

odds_1x2=odds[betType=='1x2' & bookmaker=='Pinnacle']
odds_1x2_odd1 = odds_1x2[oddtype=='odd1'& Year=='2017']

odds_1x2_odd1[,c("totalhandicap","oddtype","betType","bookmaker", "Year"):=NULL]

#order data in ascending date
odds_1x2_odd1=odds_1x2_odd1[order(matchId,date)]
odds_1x2_odd1=odds_1x2_odd1[complete.cases(odds_1x2_odd1)]
data=odds_1x2_odd1[,list(matchId, date, odd,.N),by=list(matchId)]
data[, matchId:=NULL]

require(ggplot2)
# # require(hexbin)
#  data.frame(data_all)
#  ggplot(data_all, aes(x=matchId,y=start_odd))
# +geom_col()
# 
# +geom_dotplot(binaxis = "y")
# 
# geom_point() +geom_jitter() 
# +
# scale_color_brewer(type='diverging', palette=4) +
# xlab("Carats") + ylab("Price") + ggtitle("Diamonds")
#    

 ##ÇALISTIIIII    
data$matchId=as.factor(data$matchId)
head(data)
ggplot(data, aes(x=matchId, y=odd))+
       geom_point(aes(color=date)) + scale_color_gradient(low="blue", high="red")+
   xlab("Match ID") + ylab("Odds") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=2))+ 
                                           ggtitle("Change in Odds in 2017 for Pinnacle")


# #Nmax=max(data$N)
# 
# # wide_data=dcast(data,unique(data$matchId) ~ odd ,fun.aggregate = NULL,
# #                         value.var='odd')
# 
# #OLMADI
# require(ggplot2)
# ggplot(data, aes(x = unique(data$matchId), y = data$odd))
# data90=data[N>=90]
# unique(data90$matchId)
# 
# #OLMADI :(

# 
# #OLMADI VOL63576
# require(maps)
# plot(data$N ~ data$odd, type="n")
# map("world", add=TRUE, lwd=2, col="gray")
# points(napol$Lat ~ napol$Lon, pch=16)



