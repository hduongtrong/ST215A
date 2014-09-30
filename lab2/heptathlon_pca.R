# PCA Example taken from A Handbook of Statistical Analyses Using R
# Everitt and Hothorn
library("GGally")
library("lattice")
library("ggplot2")
library("dplyr")
library("HSAUR")

data("heptathlon", package = "HSAUR")
# Recode data so up is always good
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

# We will exclude score from our PCA
score <- heptathlon$score
heptathlon <- select(heptathlon, -(score))

# Make a scatter plot matrix
# ggpairs, in  the GGally library, is crazy slow, so use lattice.
#ggpairs(heptathlon, upper="blank")
pairs(heptathlon)

# Show the correlation matrix
round(cor(heptathlon), 2)

# Calculate the PCA using prcomp.
heptathlon_pca <- prcomp(heptathlon, scale = TRUE)
print(heptathlon_pca)
summary(heptathlon_pca)

# Compare to the eigen decompisition of the covariance
heptathlon_cov <- cov(scale(heptathlon))
heptathlon_eigen <- eigen(heptathlon_cov)

# Inspect the loadings of the first PC
cbind(heptathlon_pca$rotation[,1],
      heptathlon_eigen$vectors[,1])

cbind(heptathlon_pca$sdev ^ 2,
      heptathlon_eigen$values)

# Compare the two.  Note that eigenvector is only defined up to sign,
# so we have to take the minimum of the difference and the sum.
plus.diff <- apply(heptathlon_pca$rotation + heptathlon_eigen$vectors,
                   MARGIN=2, FUN=function(x) { sum(abs(x)) })
minus.diff <- apply(heptathlon_pca$rotation - heptathlon_eigen$vectors,
                    MARGIN=2, FUN=function(x) { sum(abs(x)) })

# There are some numeric differences.  prcomp claims to be numerically better.
sum(apply(rbind(plus.diff, minus.diff), MARGIN=2, min))
.Machine$double.eps


# Calculate the first PC
center <- heptathlon_pca$center
scale <- heptathlon_pca$scale
hm <- as.matrix(heptathlon)
pc1 <- as.numeric(scale(hm, center = center, scale = scale) %*% heptathlon_pca$rotation[, 1])

# We can do this automatically with predict
cbind(pc1, predict(heptathlon_pca)[,1])

# Turns out this PC is very correlated to the score of the judges
cor(score, heptathlon_pca$x[,1])
plot(score, heptathlon_pca$x[,1])

# Show the plot of variances
plot(heptathlon_pca)

# Another way to visuallize this
cumulative.var <- cumsum(heptathlon_pca$sdev ^ 2) / sum(heptathlon_pca$sdev ^ 2)
ggplot(data=NULL, aes(x=1:length(cumulative.var), y=cumulative.var)) +
  geom_point(size=3) + geom_line()

# Visualize top two components
biplot(heptathlon_pca, col = c("black", "gray"))
