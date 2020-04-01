#############################################################################
##  Comparison of Clustering Techniques : Hierarchical Clustering only and along with Self-Organizing Maps
##
## Nikita Goswami 
## Created: March 31, 2020
## 
#############################################################################
rm(list = ls())
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("multtest")
biocLite("cluster")
library(kohonen)
library(ISLR)
library(fossil)


###############################################################################
### Using hierarchical clustering with complete linkage and
### Euclidean distance, cluster the states.
### Cut the dendrogram at a height that results in three distinct clusters. 
###############################################################################

# EDA
dim(USArrests) #50 observations for 4 classes, Murder, Assault, UrbanPop, Rape
head(USArrests)
USArrests.scaled = scale(USArrests)
# compute dissimilarity matrix
USArrests_dis = dist(USArrests.scaled)

set.seed(2)
complete.hclust <- hclust(USArrests_dis, method = "complete")
X11()
plot(complete.hclust, main ="USArrests Data : Complete Linkage")

ct = cutree(complete.hclust, 3)


#################################################################################
### Fit a SOM to the data and present the results (e.g., U-matrix, phase plots if
### appropriate, hclust on prototypes). Is this what you would expect? Does this result
### generally support your results in Part A.
##################################################################################
# Fitting a SOM
set.seed(123)
som_grid = somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
USArrests.som <- som(USArrests.scaled, grid = som_grid, rlen = 2000)

som_class <- USArrests.som$unit.classif

X11()
plot(USArrests.som, main = "USArrests Data: Grid 5*5")

# Checking if we are converging
graphics.off()
X11()
plot(USArrests.som, type="changes", main="Changes plot: Grid 5*5")

# How many different clusters do we have
X11()
plot(USArrests.som, type="count", main = "Count plot: Grid 5*5")


# Plot the mapping
X11()
plot(USArrests.som, type="mapping", main="Mapping plot: Grid 5*5")


# Lets look at a UMatrix Plot
coolBlueHotRed <- function(n, alpha=1){rainbow(n, end=4/6, alpha= alpha)[n:1]}
X11()
plot(USArrests.som, type="dist.neighbours", palette.name = coolBlueHotRed)
# Red farther from neighbors(dissimilar), blue is more similar to neighbours

# cluster the data
codes <- USArrests.som$codes[[1]]
d <- dist(codes)
hc <- hclust(d)
X11()
plot(hc)

#som_cluster <- cutree(hc,h=7.5)
USArrest_cluster <- cutree(hc, 3)

my_pal <- c("red", "blue", "yellow")
my_bgcol <- my_pal[USArrest_cluster]
# Plot SOM with found clusters
X11()
plot(USArrests.som, type="mapping", col = "black", bgcol= my_bgcol)
add.cluster.boundaries(USArrests.som, USArrest_cluster)



# Comparison using Rand Index
# Adding a column to som_class with row_id
som_class_df <- as.data.frame(som_class)
som_class_df$id  <- 1:nrow(som_class_df )
colnames(som_class_df)[1] <- "merge_col"

# Fixing column names for NCI_cluster
USArrest_cluster_df <- as.data.frame(USArrest_cluster)
USArrest_cluster_df$id  <- 1:nrow(USArrest_cluster_df)
colnames(USArrest_cluster_df)[2] <- "merge_col"

# Merge som_class and NCI_cluster
Combined = merge(som_class_df,USArrest_cluster_df,by = intersect(names(som_class_df), names(USArrest_cluster_df)))
Combined$merge_col <- NULL
table(Combined$USArrest_cluster,ct)

# comparing the clusters based on rand index
rand.index(as.numeric(Combined$USArrest_cluster), as.numeric(ct))
# 0.54
adj.rand.index(as.numeric(Combined$USArrest_cluster), as.numeric(ct))
# 0.078

