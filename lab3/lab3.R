library(parallel)
library(doParallel)
library(foreach)
library(rlecuyer)

# 0. Load Data and register Parallel
setwd(dirname(sys.frame(1)$ofile))
# setwd("~/Dropbox/School/ST215/Lab/lab3/")
load("lingBinary.RData")
data = data.matrix(lingBinary[,7:474])
#nCores <- 8
nCores <- as.numeric(Sys.getenv('NSLOTS'))
registerDoParallel(nCores)
rm(lingBinary); gc()

################################################################
# 1. Define Correlation Function of Two Clusters
################################################################
cluster.Corr = function(cl1, cl2)
{
  # correlation as suggested in Ben-Hur, Elisseeff, Guyon paper
  # is always at least 1/k for k is the number of cluster in cl1 
  # and cl2. To make the corr going from 0 to 1, we adjusted the 
  # corr by substracting 1/k away, and rescale back. 
  cor.raw = sum(table(cl1,cl2)^2)/sqrt(sum(table(cl1)^2))/
    sqrt(sum(table(cl2)^2))
  adj = 1/length(table(cl1))/length(table(cl2)); adj = sqrt(adj)
  (cor.raw - adj)/(1-adj)
}
# A Function to test cluster.Cor, make sure it returns 0 
# correlation when assigning cluster randomly. 
MC = function(n=1000,k=3)
{
  x = sample(1:k, n, replace=TRUE)
  y = sample(1:k, n, replace=TRUE)
  cluster.Corr(x,y)
}
# MC()

#################################################################
# 2. K-means Stability Test
#################################################################
n = nrow(data); p = round(0.8*n)
# Generate a vector of FALSE length n, and set 80% of them to
# be true randomly

taskFun <- function(){
  mn <- mean(rnorm(10000000))
  return(mn)
}

RNGkind("L'Ecuyer-CMRG")
out2 <- foreach(i = 1:100) %dopar% {
  cat('Starting ', i, 'th job.\n', sep = '') 
  outSub <- taskFun()
  cat('Finishing ', i, 'th job.\n', sep = '') 
  outSub # this will become part of the out object
}

RNGkind("L'Ecuyer-CMRG")
out <- foreach(i = 1:100) %dopar% {
  cat('Starting ', i, 'th job.\n', sep = '')
  
  sub1 = rep(FALSE,n); sub1[sample.int(n, p)] <- TRUE
  sub2 = rep(FALSE,n); sub2[sample.int(n, p)] <- TRUE
  cl1 = kmeans(data[sub1,], centers = 3)$cluster
  cl2 = kmeans(data[sub2,], centers = 3)$cluster
  intersect = sub1 & sub2
  cl1.intersect = cl1[intersect[sub1]]
  cl2.intersect = cl2[intersect[sub2]]
  
  cat('Finishing ', i, 'th job.\n', sep = '')
  cluster.Corr(cl1.intersect,cl2.intersect)
}


