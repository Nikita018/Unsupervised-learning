##########################
## Statistical Data Mining 2
## Homework 2

## Nikita Goswami
## Created : 3/07/2020
## Hierarchial Clustering and PCA to cluster different variety of wheat
##########################

rm(list = ls())

library("multtest")
library("fpc")
library("cluster")
library("bootcluster")
library("fossil")


# set working directory
setwd("~/R workingDir")

# Import the data and look at the first six rows
Seeds_data <- read.table(file = 'Seeds.txt', header = T, sep="\t")
head(Seeds_data)


seeds <- Seeds_data
seeds['Seed.Group'] <- NULL


pairs(Seeds_data,col=Seeds_data$Seed.Group)

####################################################################
### Apply hierarchical clustering to the samples using correlation-based distance, and plot the dendrogram. Do the genes separate the samples into two groups ? 
### Do your results depend on the type of linkage used ?
####################################################################
complete.hclust <- hclust(dist(seeds), method = "complete")
X11()
plot(complete.hclust, main = "Complete Linkage")

# For 2 clusters
# ct <- cutree(complete.hclust, k = 2)
# si <- silhouette(ct, dist = dist(seeds))
# 
# X11()
# plot(si, main = "For complete linkage for 2 clusters")
 

# For 3 clusters
ct <- cutree(complete.hclust, k = 3)
si <- silhouette(ct, dist = dist(seeds))


X11()
plot(si, main = "For complete linkage for 3 clusters")

#Accuracy :
seeds_actual=as.numeric(Seeds_data$Seed.Group)
table(ct, Seeds_data$Seed.Group)
#ct   A  B  C
#1 46 22  0
#2 20  0 65
#3  0 46  0

# For 4 clusters
ct <- cutree(complete.hclust, k = 4)
si <- silhouette(ct, dist = dist(seeds))

X11()
plot(si, main = "For compete linkage for 4 clusters")


##########################################################
## Single linkage
##########################################################
single.hclust <- hclust(dist(seeds), method = "single")
X11()
plot(single.hclust, main = "single Linkage")
# For 3 clusters
ct <- cutree(single.hclust, k = 3)
si <- silhouette(ct, dist = dist(seeds))

X11()
plot(si, main = "For Single linkage for 3 clusters")

#Accuracy :
table(ct, Seeds_data$Seed.Group)



##########################################################
## Average linkage
##########################################################
average.hclust <- hclust(dist(seeds), method = "average")
X11()
plot(average.hclust, main = "average Linkage")
# For 3 clusters
ct <- cutree(average.hclust, k = 3)
si <- silhouette(ct, dist = dist(seeds))

X11()
plot(si, main = "For average linkage for 3 clusters")

#Accuracy :
table(ct, Seeds_data$Seed.Group)



############################################################
### Cluster the data based on K-means or K-medoids. Use an analytical technique to
### justify your choice in "k". How did the performance compare to the hierarchical
### clustering of part a? Which did you feel was a better method for this data? 
############################################################

# Using kmeans clustering to find clusters using Gap Statistic
kmeans <- clusGap(seeds, kmeans, nstart = 20, K.max = 10, B = 100)

x11()
plot(kmeans, main = "kmeans : Gap Statistic")

# Cluster = 3 gives best result as per Fap statistic
km_clust <- kmeans(seeds, centers = 3, nstart = 10)
table(km_clust$cluster, Seeds_data$Seed.Group)
# Comparison
####A   B   C
##  8   0   64
##  1   59  0
##  57  9   1



# Lets try k-medoids
kmedoids <- pamk(seeds)

kmedoids$nc # 2
# K-medoids say 2 cluster is the best

table(kmedoids$pamobject$clustering, Seeds_data$Seed.Group)
#A  B  C
#1 53  0 65
#2 13 68  0
# 2 cluster is not a good idea
# Lets try 3 clusters

# Gap statistic for kmedoid
gap_kmed <- clusGap(seeds, pam, K.max = 10, B = 100)
x11()
plot(gap_kmed, main = "kmedoids : Gap Statistic")



# Lets see what 3 clusters for k-medoids turns out as
kmed <- pamk(seeds,krange=3)

table(kmed$pamobject$clustering, Seeds_data$Seed.Group)
# A  B  C
# 1 54  9  0
# 2 11  0 65
# 3  1 59  0


# Kmedoids and Average linkage Hierarchial clustering have the least misclassification ~ 10%

