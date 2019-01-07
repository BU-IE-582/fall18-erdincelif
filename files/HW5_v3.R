require(stats)
require(TSdist)
require(cluster)

set.seed(123)

data=read.csv(file="C:/Users/09172/Desktop/PhD Dersler/2018-2/IE 582/HW files/HW5/Musk1.csv", header = FALSE, sep = ",")
#data= as.data.frame(data)

# bag=data[,2]
# bagID=unique(bag)

d=c(3,5,7,10,12,15,17,20,22,25,30,40)
acc_E=array()
acc_M=array()
acc_E_h=array()
acc_M_h=array()

#########################
# function to find medoid in cluster i
clust.centroid = function(z, dat, clusters) {
  ind = (clusters == z)
  colMeans(dat[ind,])
}
##########################

for (i in 1:length(d)) {
  
k=d[i]
distE=data.frame()
distM=data.frame()
distE_h=data.frame()
distM_h=data.frame()

n=nrow(data)

# Euclidean  - kmeans

cluster_E=pam(data[,3:length(data)],k,metric = "euclidean")
center_E=cluster_E$medoids
#clusters=cluster$cluster

# Manhattan - kmeans

cluster_M=pam(data[,3:length(data)],k,metric = "manhattan")
center_M=cluster_M$medoids

# Euclidean  - hierarchical
h_cluster_E = hclust(dist(data[,3:length(data)], method = "euclidean"), method = "average")
clust_E <- cutree(h_cluster_E, k = k)
# table(clust_E,class_E)
# h_center_E=clust.centroid(k,data[,3:length(data)], clust_E)

h_center_E=NULL

for (s in 1:k) {
  h_center_E <- rbind(h_center_E, colMeans(data[,3:length(data)][clust_E == s, , drop = FALSE]))
}

# Manhattan  - hierarchical
h_cluster_M = hclust(dist(data[,3:length(data)], method = "manhattan"), method = "average")
clust_M <- cutree(h_cluster_M, k = k)

h_center_M=NULL

for (s in 1:k) {
  h_center_M <- rbind(h_center_M, colMeans(data[,3:length(data)][clust_M == s, , drop = FALSE]))
}


for (j in 1:n) {
    for (p in 1:k){
        distE[j,p]=EuclideanDistance(as.double(data[j,3:length(data)]), as.vector(center_E[p,]))
        distM[j,p]=ManhattanDistance(as.double(data[j,3:length(data)]), as.vector(center_M[p,]))
        distE_h[j,p]=EuclideanDistance(as.double(data[j,3:length(data)]), as.vector(h_center_E[p,]))
        distM_h[j,p]=ManhattanDistance(as.double(data[j,3:length(data)]), as.vector(h_center_M[p,]))
         # distE=dist(as.double(data[1,3:length(data)]), as.vector(center[[1]][1,]))
  }}


dat_E = cbind(data[,1:2], distE)
dat_M = cbind(data[,1:2], distM)
dat_E_h = cbind(data[,1:2], distE_h)
dat_M_h = cbind(data[,1:2], distM_h)

# colnames(dat_E) = c("v1","v2","v3","v4","v5")
# colnames(dat_M) = c("v1","v2","v3","v4","v5")
# dat_E_2 = aggregate(.~v2, data=dat_E, mean)
# dat_M_2 = aggregate(.~v2, data=dat_M, mean)

dat_E_2= aggregate(dat_E, by=list(dat_E[,2]), mean)
dat_M_2= aggregate(dat_M, by=list(dat_M[,2]), mean)
dat_E_2_h= aggregate(dat_E_h, by=list(dat_E_h[,2]), mean)
dat_M_2_h= aggregate(dat_M_h, by=list(dat_M_h[,2]), mean)

# LASSO
library(caret)
dat_E_2[,c(1,3)]=NULL
dat_M_2[,c(1,3)]=NULL
dat_E_2_h[,c(1,3)]=NULL
dat_M_2_h[,c(1,3)]=NULL
# 
class_E=as.factor(dat_E_2[,1])
class_M=as.factor(dat_M_2[,1])
class_E_h=as.factor(dat_E_2_h[,1])
class_M_h=as.factor(dat_M_2_h[,1])
# dat_E_2=as.data.frame(dat_E_2)
m=k+1
colnames(dat_E_2) <- c(paste0("v", 1:m))
colnames(dat_M_2) <- c(paste0("v", 1:m))
colnames(dat_E_2_h) <- c(paste0("v", 1:m))
colnames(dat_M_2_h) <- c(paste0("v", 1:m))

# Euclidean  - kmeans - accuracy 
model_E = train(as.factor(v1)~.,data=dat_E_2, method="pda", metric="Accuracy", maximize = TRUE,
             trControl=trainControl("cv",number=10))

predicted_E = predict(model_E, dat_E_2)
tbl_E=table(predicted_E, class_E)
acc_E[i]=sum(diag(tbl_E))/length(class_E)

# Manhattan - kmeans - accuracy 
model_M = train(as.factor(v1)~.,data=dat_M_2, method="pda", metric="Accuracy", maximize = TRUE,
                trControl=trainControl("cv",number=10))

predicted_M = predict(model_M, dat_M_2)
tbl_M=table(predicted_M, class_M)
acc_M[i]=sum(diag(tbl_M))/length(class_M)

# Euclidean  - hierarchical - accuracy 
model_E_h = train(as.factor(v1)~.,data=dat_E_2_h, method="pda", metric="Accuracy", maximize = TRUE,
                trControl=trainControl("cv",number=10))

predicted_E_h = predict(model_E_h, dat_E_2_h)
tbl_E_h=table(predicted_E_h, class_E_h)
acc_E_h[i]=sum(diag(tbl_E_h))/length(class_E_h)


# Manhattan  - hierarchical - accuracy 
model_M_h = train(as.factor(v1)~.,data=dat_M_2_h, method="pda", metric="Accuracy", maximize = TRUE,
                  trControl=trainControl("cv",number=10))

predicted_M_h = predict(model_M_h, dat_M_2_h)
tbl_M_h=table(predicted_M_h, class_M_h)
acc_M_h[i]=sum(diag(tbl_M_h))/length(class_M_h)
}

# COMPARISON OF METHODS & k & DISTANCE
plot(d, acc_E, col="red", type="b", ylab="Accuracy", xlab="k",
     xlim=c(0, 42), ylim=c(0.60, 1))
par(new=TRUE)
lines(d,acc_M, ylab="Accuracy", xlab="k",type = "b",
     xlim=c(0, 42), ylim=c(0.60, 1))
par(new=TRUE)
lines(d, acc_E_h, col="green", ylab="Accuracy", xlab="k",type="b",
     xlim=c(0, 42), ylim=c(0.60, 1))
par(new=TRUE)
lines(d, acc_M_h, col="blue", ylab="Accuracy", xlab="k",type="b",
     xlim=c(0, 42), ylim=c(0.60, 1))
legend("bottomright", legend=c("k-means/Euc", "k-means/Man", "hclust/Euc", "hclust/Man"),
       col=c("red", "black", "green","blue"), lty=1:2, cex=0.7)


#SELECTED MODEL: kmeans, k=17, distance: manhattan
k= 17
m=k+1

cluster_final=pam(data[,3:length(data)],k,metric = "manhattan")
center_final=cluster_final$medoids

dist_final=data.frame()

for (t in 1:n) {
  for (g in 1:k){
    dist_final[t,g]=ManhattanDistance(as.double(data[t,3:length(data)]), as.vector(center_final[g,]))
  }}
dat_final = cbind(data[,1:2], dist_final)
dat_final_2= aggregate(dat_final, by=list(dat_final[,2]), mean)
dat_final_2[,c(1,3)]=NULL
class_final=as.factor(dat_final_2[,1])
colnames(dat_final_2) <- c(paste0("v", 1:m))

model_final = train(as.factor(v1)~.,data=dat_final_2, method="pda", metric="Accuracy", maximize = TRUE,
                trControl=trainControl("cv",number=10))
  
predicted_final = predict(model_final, dat_final_2, type = "prob")

library(ROCR)

pred <- prediction(predicted_final[,2], class_final)
perf <- performance(pred,"tpr","fpr")
plot(perf)


# require(factoextra)
# fviz_nbclust(data[,3:length(data)], kmeans, method = "wss") +
#   geom_vline(xintercept = 3, linetype = 2)
