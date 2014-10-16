library(elasticnet)

setwd("~/Documents/ST215A/ST215A/lab3")
# setwd("~/Dropbox/School/ST215/Lab/lab3/")

load("lingBinary.RData")
X = (data.matrix(lingBinary[,7:474]))
#nCores <- 8
nCores <- as.numeric(Sys.getenv('NSLOTS'))
rm(lingBinary); gc();
n = nrow(X); k = ncol(X)

D = matrix(,nrow=n,ncol=n)
D = 1-exp(-dist(X, method="manhattan")/134)
H = diag(rep(1,n))-matrix(1/n,n,n)

D = (-0.5)*H%*%as.matrix(D)%*%H/(n/2)
rm(H); rm(X); gc();

n.spar = n/2
K = 5
out = spca(D, K = K, para = rep(n.spar,K), type="Gram", 
           sparse = "varnum", trace=TRUE)




