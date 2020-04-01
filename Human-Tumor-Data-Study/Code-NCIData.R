#############################################################################
##  Clustering of Tumor Micro-array data using Self-Organizing Maps
##
## Nikita Goswami 
## Created: March 31, 2020
## 
#############################################################################
rm(list = ls())

library(ElemStatLearn)
data(nci)

dim(nci)

# Transpose the Dataframe
nci_t <- t(nci)

nci.scaled = scale(nci_t)

# Fitting a SOM
set.seed(123)
som_grid = somgrid(xdim = 3, ydim = 3, topo = "hexagonal")
nci.som <- som(nci.scaled, grid = som_grid, rlen = 5000)

som_class <- nci.som$unit.classif

X11()
plot(nci.som, main = "USArrests Data: Grid 3*3")

# Checking if we are converging
graphics.off()
X11()
plot(nci.som, type="changes", main="Changes plot: Grid 3*3")

# How many different clusters do we have
X11()
plot(nci.som, type="count", main = "Count plot: Grid 3*3")


# Plot the mapping
X11()
plot(nci.som, type="mapping", main="Mapping plot: Grid 3*3")


# Lets look at a UMatrix Plot
coolBlueHotRed <- function(n, alpha=1){rainbow(n, end=4/6, alpha= alpha)[n:1]}
X11()
plot(nci.som, type="dist.neighbours", palette.name = coolBlueHotRed)
# Red farther from neighbors(dissimilar), blue is more similar to neighbours

# cluster the data
codes <- nci.som$codes[[1]]
d <- dist(codes)
hc <- hclust(d)
X11()
plot(hc)


#som_cluster <- cutree(hc,h=7.5)
som_class <- nci.som$unit.classif
NCI_cluster <- cutree(hc, 2)

my_pal <- c("green", "violet", "pink", "orange", "black", "maroon", "light green", "light blue", "magenta", "purple")
my_bgcol <- my_pal[NCI_cluster]
# Plot SOM with found clusters
X11()
plot(nci.som, type="mapping", col = "black", bgcol= my_bgcol, main = "Mapping Plot for 2 clusters")
add.cluster.boundaries(nci.som, NCI_cluster)

nci.labels <- colnames(nci)


# Adding a column to som_class with row_id
som_class_df <- as.data.frame(som_class)
som_class_df$id  <- 1:nrow(som_class_df )
colnames(som_class_df)[1] <- "merge_col"

# Fixing column names for NCI_cluster
NCI_cluster_df <- as.data.frame(NCI_cluster)
NCI_cluster_df$id  <- 1:nrow(NCI_cluster_df)
colnames(NCI_cluster_df)[2] <- "merge_col"

# Merge som_class and NCI_cluster
Combined = merge(som_class_df,NCI_cluster_df,by = intersect(names(som_class_df), names(NCI_cluster_df)))

Combined$merge_col <- NULL
table(Combined$NCI_cluster, nci.labels)
