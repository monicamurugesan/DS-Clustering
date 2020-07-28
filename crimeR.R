library(cluster)
library(factoextra)
setwd("H:\\RStudio\\assignment")
crimed<-read.csv(file.choose())
View(crimed)
attach(crimed)
crime <-crimed[,-1]
View(crime)
attach(crime)
head(crime)
summary(crime)
datcrime<-scale(crime)
View(datcrime)
library(dendextend)

dist <-dist(datcrime,method="euclidean")
hc_crime<-hclust(dist,method="ward.D2")
hc_crime
plot(hc_crime)
as.dendrogram(hc_crime)

crimetr <-cutree(hc_crime,k=4)
rect.hclust(hc_crime,4,border="red")
table(crimetr)
noc <-as.matrix(crimetr)
fin <-data.frame(crime,noc)

View(fin)

crimeagg<-aggregate(crime,list(crimetr),median)
crimeagg



centroid = function(i, crime, crimetr) 
{
  ind = (crimetr == i)
  colMeans(crime[ind,])
}

sapply(unique(crimetr), centroid,crime, crimetr)


##K-Means Clustering

kmeans_crime<-kmeans(crime,4,nstart=20)
str(kmeans_crime)

###Visualization of K-Means Clusters
library(animation)
crimek <-kmeans.ani(crime,4)
wss_crime<-(nrow(crime)-1)*(sum(apply(crime,2,var)))

for (i in 2:4) 
  wss_crime[i] = sum(kmeans(crime, centers=i)$withinss)
plot(1:4, wss_crime, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

###K point selection
library(kselection)
kselect <-kselection(crime,k_threshold = 0.6,parallel=TRUE,max_centers = 11)
kselect
final <-data.frame(crime,kmeans_crime$cluster)
final
kmd <-aggregate(dat1,by=list(km$cluster),FUN=mean)
View(kmd)

##clara
cl_crime<-clara(crime,2,sample=200)
cl_crime
clusplot(cl_crime)
fviz_cluster(kmeans_crime, data = crime,
             palette = c("black","red","blue","orange"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)
kmeans_crime
kmeans_crime$centers
#dbscan
table(X,kmeans_crime$cluster)



