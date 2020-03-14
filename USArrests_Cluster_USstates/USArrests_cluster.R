
#############################################################################
##  CHierarchial clustering on states of USA
##
## Nikita Goswami 
## Created: March 7, 2020
## 
#############################################################################
rm(list = ls())
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("multtest")
biocLite("cluster")
#install.packages("BiocManager")
#BiocManager::install()
# install.packages("bioconductor")
# install.packages("fpc")
# install.packages("bootcluster")

###############################################################################
### Using hierarchical clustering with complete linkage and
### Euclidean distance, cluster the states.
###############################################################################


# EDA
dim(USArrests) #50 observations for 4 classes, Murder, Assault, UrbanPop, Rape
head(USArrests)
# compute dissimilarity matrix
USArrests_dis = dist(USArrests)

set.seed(2)
complete.hclust <- hclust(USArrests_dis, method = "complete")
X11()
plot(hc.complete)


###############################################################################
### Cut the dendrogram at a height that results in three distinct clusters. 
### Which states belong to which clusters ?
###############################################################################
cutree(complete.hclust, 3)


################################################################################
### Hierarchically cluster the states using complete linkage and Euclidean distance, 
### after scaling the variables to have standard deviation one
###############################################################################
USArrests.sd1 <- scale(USArrests)
complete.hclust.sd <- hclust(dist(USArrests.sd1), method = "complete")
X11()
plot(complete.hclust.sd)


###################################################################################
### What effect does scaling the variables have on the hierarchical clustering obtained ?
### In your opinion, should the variables be scaled before the inter-observation dissimilarities are computed ? Provide a justification for your answer.
###################################################################################
cutree(complete.hclust.sd, 3)

#################
#install.packages("ggfortify")
# visualize the data using PC1 and PC2 to make sense of the dendrogram
library(ggfortify)
X11()
autoplot(kmeans(USArrests, 3), data = USArrests, label = TRUE, label.size = 3)

