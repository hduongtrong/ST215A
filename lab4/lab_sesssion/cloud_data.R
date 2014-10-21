library(dplyr)
library(ggplot2)

setwd(file.path(Sys.getenv("GIT_REPO_LOC"), "classes/STAT215A_Fall2013/gsi_lab4/Lab"))
# Get the data for three images

image1 <- read.table('image1.txt', header=F)
image2 <- read.table('image2.txt', header=F)
image3 <- read.table('image3.txt', header=F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

head(image1)
summary(image1)

# The raw image (red band, from nadir).
ggplot(image1) + geom_point(aes(x=x, y=y, color=AN))

# The classification.
ggplot(image1) + geom_point(aes(x=x, y=y, color=factor(label)))

# Class conditional densities.
ggplot(image1) + geom_density(aes(x=AN, group=factor(label), fill=factor(label)), alpha=0.5)


