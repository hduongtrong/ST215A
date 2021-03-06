library(parallel)
library(doParallel)
library(foreach)
library(rlecuyer)
library(microbenchmark)
library(Rcpp)
library(ggplot2)
library(gridExtra)

# 0. Load Data and register Parallel

working.directory = "~/Documents/ST215A/ST215A/lab3"
nCores <- as.numeric(Sys.getenv('NSLOTS'))
# working.directory = "~/Dropbox/School/ST215/Lab/lab3/"
# nCores <- 16
setwd(working.directory)
registerDoParallel(nCores)

load("PC2.RData")
data = sqrt(out$values)*out$vectors

# args=(commandArgs(TRUE))
# for(i in 1:length(args))
# {
#   eval(parse(text=args[[i]]))
# }

################################################################
# 1. Define Correlation Function of Two Clusters
################################################################
# R's version
cluster.CorR = function(cl1, cl2)
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

# R's version improved. Calculate the Table only once
cluster.CorR2 = function(cl1, cl2)
{
  # Calculate the table function only once.
  joint.dist = table(cl1, cl2)
  cor.raw = sum(joint.dist^2)/sqrt(sum(rowSums(joint.dist)^2))/
  sqrt(sum(colSums(joint.dist)^2))
  adj = 1/nrow(joint.dist)/ncol(joint.dist); adj = sqrt(adj)
  (cor.raw - adj)/(1-adj)
}

# A third function to calculate Correlation using 
# table function written in C++. Note that this table function 
# only works in our specific case where elements are natural 
# numbers starting from 1 (the cluster vector).
sourceCpp(file.path(working.directory, 'table.cpp'))
cluster.CorCPP = function(cl1, cl2, k)
{
	joint.dist = tableCPP(cl1, cl2, k)
    cor.raw = sum(joint.dist^2)/sqrt(sum(rowSums(joint.dist)^2))/
    sqrt(sum(colSums(joint.dist)^2))
    adj = 1/nrow(joint.dist)/ncol(joint.dist); adj = sqrt(adj)
    (cor.raw - adj)/(1-adj)
}

# A Function to test cluster.Cor, make sure it returns 0 
# correlation when assigning cluster randomly. 
MC = function(n=1e6,k=3)
{
  x = sample(1:k, n, replace=TRUE)
  y = sample(1:k, n, replace=TRUE)
  print(system.time(cluster.CorR(x,y)))
  print(system.time(cluster.CorR2(x,y)))
  print(system.time(cluster.CorCPP(x,y,k)))
}
# MC()

#################################################################
# 2. K-means Stability Test
#################################################################
n = nrow(data); p = round(0.8*n)
# Generate a vector of FALSE length n,], and set 80% of them to
# be true randomly
# Please set the size of outer loop and inner loop accordingly
# to your machine's power. This setting was used on the SCF 
# Cluster. 
RNGkind("L'Ecuyer-CMRG")
out <- foreach(i = 2:10) %do% {
  out.sm <- foreach(j = 1:128) %dopar% {
    cat('Starting ', i, ".", j, 'th job.\n', sep = '')
    
    sub1 = rep(FALSE,n); sub1[sample.int(n, p)] <- TRUE
    sub2 = rep(FALSE,n); sub2[sample.int(n, p)] <- TRUE
    cl1 = kmeans(data[sub1,], centers = i)$cluster
    cl2 = kmeans(data[sub2,], centers = i)$cluster
    intersect = sub1 & sub2
    cl1.intersect = cl1[intersect[sub1]]
    cl2.intersect = cl2[intersect[sub2]]
    
    cat('Finishing ', i, ".", j, 'th job.\n', sep = '')
    cluster.CorCPP(cl1.intersect,cl2.intersect, i)
  }
  unlist(out.sm)
}
print(out)
#save(out, file = paste("KMean",i,".RData", sep="")
save(out, file = "Kmean5.RData")
