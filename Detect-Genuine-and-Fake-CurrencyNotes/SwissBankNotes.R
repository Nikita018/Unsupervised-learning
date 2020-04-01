#############################################################################
##  Detecting counterfiet swiss bank notes from the actual ones
##
## Nikita Goswami 
## Created: March 30, 2020
## 
#############################################################################
rm(list = ls())
graphics.off()

setwd("~/R workingDir")

# Load Library
library(kohonen)


# Load data
Notes <- get(load('SwissBankNotes.rdata'))
head(Notes)


# Genuine Notes
genuine_notes <- Notes[1:100,]
dim(genuine_notes)
pc_genuine <- prcomp(scale(genuine_notes))
X11()
plot(pc_genuine, main = "Principal Components for 100 genuine Notes")

X11()
biplot(pc_genuine, main = "Principal Components on 100 genuine Notes")
# Genuine Notes are forming a cluster 

#  Notes
fake_notes <- Notes[101:200,]
dim(fake_notes)
pc_fake <- prcomp(scale(fake_notes))
X11()
plot(pc_fake, main = "Principal Components for 100 fake Notes")

X11()
biplot(pc_fake, main = "Principal Components on 100 fake Notes")
# Fake Notes have outliers


# PCA on complete dataset (Original + Fake Notes)
# Scaling Data before looking for Principal Components
pc_Notes <- prcomp(scale(Notes))
X11()
plot(pc_Notes, main = "Principal Components on all 200 Notes")

X11()
biplot(pc_Notes, main = "Biplot for all 200 Notes")

# Adding color to life
install.packages("ggfortify")
library(ggfortify)
fake_notes$Type <- "counterfeit"
genuine_notes$Type <- "genuine"
Swiss_Notes <- rbind(fake_notes, genuine_notes)
X11()
autoplot(pc_Notes, data = Swiss_Notes, colour = 'Type',
         loadings = TRUE, loadings.colour = 'green',
         loadings.label = TRUE, loadings.label.size = 6,variance_percentage = TRUE,legend = TRUE)
# Genuine and Fake notes are separated well in PC1 and PC2

summary(fake_notes$diagonal)
summary(genuine_notes$diagonal)
