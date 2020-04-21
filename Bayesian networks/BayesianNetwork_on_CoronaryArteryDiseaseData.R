#####################################################################
## This code demonstrates Bayesian Networks in R                   ## 
## We infer networks from data                                     ##
## Creator : Nikita Goswami                                                ##
## Created: April 21, 2020                                          ##
##
#####################################################################
rm(list = ls())
graphics.off()

######################################################
## See: http://cran.r-project.org/web/views/gR.html
##################################################
library(gRain)
#library(RHugin)
library(Rgraphviz)
library(gRbase)
library(ggm)
BiocManager::install(c("bnlearn"))
library(gRim)
library(gRim)
library(igraph)
library(bnlearn)


# load the data
data(cad1, package = "gRbase")


# Infer a BN
cad.bn <- hc(cad1) #hillClimbing algorithm
# The object that comes out is not compatible with gRain package, 
# so converting it to
net <- as(amat(cad.bn), "graphNEL")

#Plot the "best" network
X11()
plot(net)

# The network does not make much sense as we know that sex and smoking should be upstream
# knowledge of sex and smoking can help in identifying the disease
###############################################################
# Impose prior knowledge on the ordering of the variables....
# Block 1: background 
# Block 2: disease
# Block 3: disease manifestation
# Block 4: Clinical information
###############################################################
names(cad1)
block <- c(1, 3, 3, 4, 4, 4, 4, 1, 2, 1, 1, 1, 3, 2) #assign variables a block
blM <- matrix(0, nrow = 14, ncol = 14)
rownames(blM) <- names(cad1)
colnames(blM) <- names(cad1)

# fill in the illegal edges
for
 (b in 2:4){
  print(block)
blackL <- data.frame(get.edgelist(as(blM, "igraph")))
	blM[block == b, block < b] <- 1
}
names(blackL) <- c("from", "to")
#Refit the network under the new constraints 
cad.bn2 <- hc(cad1, blacklist = blackL)
net.constr <- as(amat(cad.bn2), "graphNEL")

quartz()
plot(net.constr)


quartz()
plot(net)










