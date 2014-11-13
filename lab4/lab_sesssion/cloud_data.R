library(dplyr)
library(ggplot2)
library(fields)
# setwd(file.path(Sys.getenv("GIT_REPO_LOC"), "classes/STAT215A_Fall2013/gsi_lab4/Lab"))
setwd("~/Dropbox/School/ST215/Lab/lab4/image_data/")
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
ggplot(train[train$k == 1,]) + geom_point(aes(x=x, y=y, color=CF))

# The classification.
ggplot(image1) + geom_point(aes(x=x, y=y, color=factor(label)))

# Class conditional densities.
ggplot(image1) + geom_density(aes(x=AN, group=factor(label), fill=factor(label)), alpha=0.5)

# Plotting histogram and density
ggplot(image1) + geom_histogram(aes(x=AN))
ggplot(image1) + geom_point(aes(x=DF,y=CF))
plot(image1[c('DF','CF','BF','AF','AN')])
image.plot(cor(image1[c('DF','CF','BF','AF','AN')]))
plot(image1[c('NDAI','SD','CORR')])

ggplot(image3) + geom_density(aes(x=CORR, group=factor(label), fill=factor(label)), alpha=0.5)

ggplot(data = image1) + 
  geom_point(aes(x=NDAI,y=SD,color = factor(label)), alpha = 0.5)

cor(image2)[3:11,3]

#####################
# lmfit
#####################
lmfit = lm(label ~ NDAI + SD + CORR + DF + CF + BF + AF + AN, data = image1)
mi.empirical(image1$label, image1$NDAI)

ggplot(data = train) + 
  geom_boxplot(aes(factor(label), CORR))
ggplot(data = train) +
  geom_density(aes(x = NDAI))
qqnorm(train$AN)
qqline(train$logSD)
cor(train$SD, train$label)

