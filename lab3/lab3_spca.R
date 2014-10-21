library(rARPACK)
library(RhpcBLASctl)


setwd("~/Documents/ST215A/ST215A/lab3")
# setwd("~/Dropbox/School/ST215/Lab/lab3/")

load("lingBinary.RData")
X = (data.matrix(lingBinary[,7:474]))
#nCores <- 8
nCores <- as.numeric(Sys.getenv('NSLOTS'))
blas_set_num_threads(nCores)
rm(lingBinary); gc();
n = round(nrow(X)); k = ncol(X)

D = matrix(,nrow=n,ncol=n)
D = 1-exp(-dist(X[1:n,], method="manhattan")/134)
H = diag(rep(1,n))-matrix(1/n,n,n)

D = (-0.5)*H%*%as.matrix(D)%*%H/(n/2)
rm(H); rm(X); gc();

out = eigs_sym(D,5)
save(out, file = "PC2.RData")
