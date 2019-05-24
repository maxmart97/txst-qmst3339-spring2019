#Clustering-A2


########################## START SCRIPT ##########################


#Setup

#Install various packages if needed.
install.packages(c("plyr",'ggplot2', "cluster", "lattice", "graphics", "grid"))

#Potential libraries used to solve perform clustering and show data.
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)
library(datasets)

#Package used for this assignment.
install.packages("rattle")
library(rattle)
data(wine, package="rattle")
attach(wine)


########################## 1 - Partitional Clustering ##########################


#1
View(wine)
wine_scaled <- scale(wine[,2:14])

#2a
set.seed(1234)
wss <- numeric(15) # 15 levels of k
for (k in 1:15) wss[k] <- sum(kmeans(wine_scaled, centers=k, nstart=25)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

#2b
set.seed(1234)
wine_km = kmeans(wine_scaled, centers=3, nstart=25)
wine_km

#2c
wine_km$size
wine_km$centers

#2d
wine_default_and_km <- data.frame(wine, wine_km$cluster)
table(wine_default_and_km$Type, wine_default_and_km$wine_km.cluster, dnn = c("Wine Type", "Cluster"))


########################## 2 - Hierarchical Clustering ##########################


#Setup
library(datasets)
data(iris, package="datasets")
attach(iris)

#1
set.seed(123)
indexes <- sample(1:dim(iris)[1], 40) #Matrix of indexes for the 40 randomly selected records
iris_40 <- iris[indexes, 1:4] #40 randomly selected records from iris data set with only numerical variables.
iris_all <- iris[indexes, 1:5] #Need this because you cannot label dendrogram without the Species column.

#2
graphics.off()
dendrogram <- hclust(dist(iris_40[,1:4]), method="ward.D")
par(mar = c(3, 4, 4, 2) + 0.1)
plot(dendrogram, labels=iris_all$Species, xlab="", sub="")
title(xlab="Species", line=1, cex.lab=1.2)

#3
rect.hclust(dendrogram, k=3)

########################## END SCRIPT ##########################