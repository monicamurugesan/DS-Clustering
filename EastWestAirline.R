library(factoextra)
library(cluster)
library(readxl)
suppressPackageStartupMessages(library(dendextend))
library(fpc)
library(NbClust)
?
setwd("H:\\RStudio\\assignment")
data1East <-read.csv(file.choose())
dat1<-data1East[,2:11]
View(dat1)
attach(dat1)
cc1_miles <-ifelse(cc1_miles==1,2500 ,ifelse(cc1_miles==2,7500,ifelse(cc1_miles==3,17500,ifelse(cc1_miles==4,32500,ifelse(cc1_miles==5,50000,0)))))
cc2_miles<-ifelse(cc2_miles==1,2500,ifelse(cc2_miles==2,7500,ifelse(cc2_miles==3,17500,ifelse(cc2_miles==4,32500,ifelse(cc2_miles==5,50000,0)))))
cc3_miles<-ifelse(cc3_miles==1,2500,ifelse(cc3_miles==2,7500,ifelse(cc3_miles==3,17500,ifelse(cc3_miles==4,32500,ifelse(cc3_miles==5,50000,0)))))
dat <-scale(dat1)
distance <-dist(dat,method="euclidean")
hc<-hclust(distance,method="ward.D2")
as.dendrogram(hc)
cd = color_branches(hc,k=3)

clusters<-cutree(hc,k=3)
rect.hclust(hc,k=3,border="orange")
table(clusters)

noc <-as.matrix(clusters)
fin <-data.frame(dat1,noc)
final1<-fin[,c(ncol(fin),1:(ncol(fin)-1))]
View(final1)
g1<-aggregate(dat1,list(clusters),median)

data.frame(Cluster=g1[,1],Freq=as.vector(table(clusters)),g1[,-1])


centroid = function(i, dat1, clusters) 
{
  ind = (clusters == i)
  colMeans(dat1[ind,])
}

sapply(unique(clusters), centroid,dat1, clusters)


###KMeans 
km <-kmeans(dat,3,nstart=10)
str(km)
library(animation)
km <-kmeans.ani(dat,3)
wss1 <-(nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:11) 
  wss1[i] = sum(kmeans(dat, centers=i)$withinss)
plot(1:11, wss1, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

###For Finding K values 
library(kselection)
k<-kselection(dat1,k_threshold=0.9,parallel=TRUE,max_centers = 12)
k
km$cluster
fin2<-data.frame(dat1,km$cluster)
View(fin2)
aggregate(dat1,by=list(km$cluster),FUN=mean)


##clara
clar <-clara(dat1,2,sample=100)
clar

table(data1East$Award.,km$cluster)
install.packages("dbscan")
library(dbscan)
library(fpc)
?dbscan
db<-fpc::dbscan(dat,4,MinPts = 5,method="hybrid",showplot = 1,countmode=NULL)
fviz_cluster(db, dat, stand = FALSE, frame = FALSE,geom="point")
?fviz_cluster
