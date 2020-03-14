##########################
## Statistical Data Mining 2
## Homework 2

## Nikita Goswami
## Created : 3/07/2020
## Hierarchial Clustering and PCA to cluster healthy patient and diseased patient genes
##########################

rm(list = ls())

# set working directory
setwd("~/R workingDir")

# Import the data and look at the first six rows
Gene_data <- read.csv(file = 'Ch10Ex11.csv', header = F)
head(Gene_data)

####################################################################
### Apply hierarchical clustering to the samples using correlation-based distance, and plot the dendrogram. Do the genes separate the samples into two groups ? 
### Do your results depend on the type of linkage used ?
####################################################################
complete.hclust <- hclust(as.dist(1 - cor(Gene_data)), method = "complete")
plot(complete.hclust, main = "Complete Linkage")


single.hclust <- hclust(as.dist(1-cor(Gene_data)),method='single')
plot(single.hclust, main = "Single Linkage")

average.hclust <- hclust(as.dist(1-cor(Gene_data)),method='average')
plot(average.hclust, main = "Average Linkage")

centroid.hclust <- hclust(as.dist(1-cor(Gene_data)),method='average')
plot(average.hclust, main = "centroid Linkage")



#####################################################################
### Find out which gene differs the most across the 2 classes
#####################################################################

# Applying PCA to see which gene differs the most
# transpose the Gene_data matrix
t_Gene_data <- t(Gene_data)
# approach : Find out the loading scores of PC1 loading vector
pca.gene <- prcomp(t_Gene_data)
# columns of rotation contain eigen vectors/loading scores
head(pca.gene$rotation)
summary(pca.gene)


load.pca <- apply(pca.gene$rotation, 1, sum)
# ordering absolute values of loading scores for PC1
# order function gives a serial number to the loading score values before ordering/sorting
index <- order(abs(load.pca), decreasing = TRUE)
index[1:10]
# The top 10 most differing genes are 865  68 911 428 624  11 524 803 980 822