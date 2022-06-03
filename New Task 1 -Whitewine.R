
#importing relevant libraries
library(readxl)
library(dplyr)
library(fpc)
library(MASS)
library(caret)
library(flexclust)
library(NbClust)
library(datasets)           
library(ggplot2)
library(gridExtra)
library(class)
library(factoextra)
#importing the dataset
WhitewineData <- read_xlsx("./whitewine_v2.xlsx")
boxplot(WhitewineData)

#summary
summary(WhitewineData)


oldpar = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(WhitewineData[[i]])
  mtext(names(WhitewineData)[i], cex = 0.8, side = 1, line = 2)
}
par(oldpar)

pairs(WhitewineData [, -grep("quality", colnames(WhitewineData))])

#Scaling
scaledWhiteWineData <- data.frame(scale(WhitewineData))

#outlier detection
outliers = c()
for ( i in 1:11 ) {
  stats = boxplot.stats(WhitewineData[[i]])$stats
  bottom_outlier_rows = which(WhitewineData[[i]] < stats[1])
  top_outlier_rows = which(WhitewineData[[i]] > stats[5])
  outliers = c(outliers , top_outlier_rows[ !top_outlier_rows %in% outliers ] )
  outliers = c(outliers , bottom_outlier_rows[ !bottom_outlier_rows %in% 
                                                 outliers ] )
}

#removing detected outliders
boxplot(scaledWhiteWineData$fixed.acidity) 
par(mfrow=c(2,6),oma=c(1,1,0,0)+0.1,mar=c(3,3,1,1)+0.1)

boxplot(WhitewineData$'fixed acidity',col="slategray2",pch=19)
mtext("Fixed Acidity",cex=0.8,side=1,line=2)

boxplot(WhitewineData$'volatile acidity',col="slategray2",pch=19)
mtext("Volatile Acidity",cex=0.8,side=1,line=2)

boxplot(WhitewineData$'citric acid',col="slategray2", pch=19)
mtext("citric Acid",cex=0.8,side=1,line=2)

boxplot (WhitewineData$'residual sugar',col="slategray2", pch=19)
mtext("residual sugar",cex=0.8,side=1,line=2)

boxplot (WhitewineData$'chlorides',col="slategray2", pch=19)
mtext("chlorides",cex=0.8,side=1,line=2)

boxplot (WhitewineData$'free sulfur dioxide',col="slategray2", pch=19)
mtext("free sulfur dioxide",cex=0.8,side=1,line=2)

boxplot (WhitewineData$'total sulfur dioxide',col="slategray2", pch=19)
mtext("total sulfur dioxide",cex=0.8,side=1,line=2)

boxplot (WhitewineData$'density',col="slategray2", pch=19)
mtext("density",cex=0.8,side=1,line=2)

boxplot (WhitewineData$'pH' ,col="slategray2", pch=19)
mtext("pH",cex=0.8,side=1,line=2)

boxplot (WhitewineData$'sulphates' ,col="slategray2", pch=19)
mtext("sulphates",cex=0.8,side=1,line=2)

boxplot (WhitewineData$'alcohol' ,col="slategray2", pch=19)
mtext("alcohol",cex=0.8,side=1,line=2)

boxplot (WhitewineData$'quality' ,col="slategray2", pch=19)
mtext("quality",cex=0.8,side=1,line=2)


#identifying & removing outliers(Fixed Acidity)
boxplot(scaledWhiteWineData$fixed.acidity)
outliers<- boxplot (scaledWhiteWineData$fixed.acidity,plot = FALSE)$out
newWhiteWineData<-scaledWhiteWineData[-which(scaledWhiteWineData$fixed.acidity 
                                             %in% outliers),]
boxplot(newWhiteWineData$fixed.acidity)

#identifying & removing outliers(Volatile Acidity)
boxplot(scaledWhiteWineData$volatile.acidity)
outliers<- boxplot (scaledWhiteWineData$volatile.acidity,plot = FALSE)$out
newWhiteWineData<-scaledWhiteWineData[-which(
  scaledWhiteWineData$volatile.acidity %in% outliers),]
boxplot(newWhiteWineData$volatile.acidity)

#identifying & removing outliers(Citric Acid)
boxplot(scaledWhiteWineData$citric.acid)
outliers<- boxplot (scaledWhiteWineData$citric.acid,plot = FALSE)$out
newWhiteWineData<-scaledWhiteWineData[-which(
  scaledWhiteWineData$citric.acid %in% outliers),]
boxplot(newWhiteWineData$citric.acid)

#identifying & removing outliers(Residual Sugar)
boxplot(scaledWhiteWineData$residual.sugar)
outliers<- boxplot (scaledWhiteWineData$residual.sugar,plot = FALSE)$out
newWhiteWineData<-scaledWhiteWineData[-which(
  scaledWhiteWineData$residual.sugar %in% outliers),]
boxplot(newWhiteWineData$residual.sugar)

#identifying & removing outliers(Chlorides)
boxplot(scaledWhiteWineData$chlorides)
outliers<- boxplot (scaledWhiteWineData$chlorides,plot = FALSE)$out
newWhiteWineData<-scaledWhiteWineData[-which(
  scaledWhiteWineData$chlorides %in% outliers),]
boxplot(newWhiteWineData$chlorides)

#identifying & removing outliers(Free Sulfur Dioxide)
boxplot(scaledWhiteWineData$free.sulfur.dioxide)
outliers<- boxplot (scaledWhiteWineData$free.sulfur.dioxide,plot = FALSE)$out
newWhiteWineData<-scaledWhiteWineData[-which(
  scaledWhiteWineData$free.sulfur.dioxide %in% outliers),]
boxplot(newWhiteWineData$free.sulfur.dioxide)

#identifying & removing outliers(Total Sulfur Dioxide)
boxplot(scaledWhiteWineData$total.sulfur.dioxide)
outliers<- boxplot (scaledWhiteWineData$total.sulfur.dioxide,plot = FALSE)$out
newWhiteWineData<-scaledWhiteWineData[-which(
  scaledWhiteWineData$total.sulfur.dioxide %in% outliers),]
boxplot(newWhiteWineData$total.sulfur.dioxide)

#identifying & removing outliers(Density)
boxplot(scaledWhiteWineData$density)
outliers<- boxplot (scaledWhiteWineData$density,plot = FALSE)$out
newWhiteWineData<-scaledWhiteWineData[-which(
  scaledWhiteWineData$density %in% outliers),]
boxplot(newWhiteWineData$density)

#identifying & removing outliers(pH)
boxplot(scaledWhiteWineData$pH)
outliers<- boxplot (scaledWhiteWineData$pH,plot = FALSE)$out
newWhiteWineData<-scaledWhiteWineData[-which(
  scaledWhiteWineData$pH %in% outliers),]
boxplot(newWhiteWineData$pH)

#identifying & removing outliers(Sulphates)
boxplot(scaledWhiteWineData$sulphates)
outliers<- boxplot (scaledWhiteWineData$sulphates,plot = FALSE)$out
newWhiteWineData<-scaledWhiteWineData[-which(
  scaledWhiteWineData$sulphates %in% outliers),]
boxplot(newWhiteWineData$sulphates)

#Alcohol
boxplot(newWhiteWineData$alcohol)

#initializing clusters(clusers=k)

#k=2
k2 <- kmeans(newWhiteWineData,2,iter.max = 140,algorithm = "Lloyd",nstart = 100)
k2
fviz_cluster(k2,newWhiteWineData)

#k=3
k3 <- kmeans(newWhiteWineData,3,iter.max = 140,algorithm = "Lloyd",
             nstart = 100)
k3
fviz_cluster(k3,newWhiteWineData)

#k=4
k4 <- kmeans(newWhiteWineData,4,iter.max = 140,algorithm = "Lloyd",
             nstart = 100)
k4
fviz_cluster(k4,newWhiteWineData)

#Finding the best number of clusters using Elbow Method
fviz_nbclust (newWhiteWineData, kmeans, method = "wss") +geom_vline(
  xintercept = 4, linetype = 2)+labs (subtitle = "ELBOW METHOD")

#Finding the best number of clusters using Silhouette Method
fviz_nbclust (newWhiteWineData, kmeans, method = "silhouette")+labs (
  subtitle ="SILHOUETTE METHOD")

#NB Clust
NbClust (data = newWhiteWineData, diss = NULL, distance="euclidean", 
         min.nc = 2, max.nc = 15, method="kmeans",index="all")

table(k2$cluster,newWhiteWineData$quality)
table(k3$cluster,newWhiteWineData$quality)
table(k4$cluster,newWhiteWineData$quality)



#principal component analysis (PCA)
pca<-prcomp(newWhiteWineData[,-12])
pca
summary(pca)
pcaWineData<-pca$x[,9:11]
plot(pcaWineData,pch=16,col=rgb(0,0,0,0.60))

pcaCluster <- kemans(pcaWineData,2,nstart=25)
pcaCluster$cluster
pcaCluster
fvliz_cluster(pcaCluster,pcaWineData)
