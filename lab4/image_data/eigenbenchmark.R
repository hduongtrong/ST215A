library("corpcor")
library("rARPACK")
#library(rbenchmark)
library(microbenchmark)
library(irlba)

# generate a "fat" data matrix
n = 5000
X = matrix(rnorm(n*n), n, n)
X = t(X)%*%X

# compute SVD
system.time( (s1 = eigen(X,symmetric = TRUE)) ) 
#   user  system elapsed 
# 58.487   1.042  35.895 
system.time( (s2 = svd(X, nu = 5, nv = 0)))
# user  system elapsed 
# 304.342   5.497 138.519
system.time( (s3 = fast.svd(X)) )
# user  system elapsed 
# 396.007   8.395 187.781 
system.time( (s4 = eigs_sym(X, 5)))
# user  system elapsed 
# 1.909   0.029   2.013 
system.time( (s5 = svds(X,k=5)) )
# user  system elapsed 
# 7.686   0.082   4.113 
system.time( (s6 = irlba(X, nu = 5, nv = 0)))
# user  system elapsed 
# 27.559   0.684  12.167 

eps = 1e-9
sum(abs(s1$values[1:5]-s4$values) > eps)
microbenchmark(
  svd(X),
  fast.svd(X),
  svds(X,k=49),
  unit="us"
)

sum(abs(s1$d-s2$d) > eps)
sum(abs(abs(s1$u)-abs(s2$u)) > eps)
sum(abs(abs(s1$v)-abs(s2$v)) > eps)

eps = 1e-10
sum(abs(s1$dp[1:49]-s3$d) > eps) 
sum(abs(abs(s1$u[,1:49])-abs(s3$u)) > eps)
sum(abs(abs(s1$v[,1:49])-abs(s3$v)) > eps)
# Unit: microseconds
#            expr      min       lq   median       uq      max
#          svd(X) 37467.11 38868.86 39412.82 40754.23 134706.7
#     fast.svd(X) 29510.61 31043.12 31725.83 32746.21 123791.5
# svds(X, k = 49) 30192.09 31739.79 32405.88 33481.88 128894.8
# 
# makes sense as fast.svd is advertised as being optimized for 
# "fat" and thin matrices

microbenchmark(
  svd(X,nu=5,nv=5),
  fast.svd(X),
  svds(X,k=5),
  unit="us"
)
# Unit: microseconds
#                   expr      min       lq   median       uq       max neval
# svd(X, nu = 5, nv = 5) 37867.79 39402.14 40414.92 42767.06 131423.68   100
#            fast.svd(X) 29613.24 31076.17 31890.66 33195.33 127140.20   100
#         svds(X, k = 5) 22665.00 23845.07 24534.15 25343.10  37097.22   100

# what about irlba?
microbenchmark(
  svd(X,nu=5,nv=5),
  fast.svd(X),
  svds(X,k=5),
  irlba(X,nu=5,nv=5),
  unit="us"
)

# Unit: microseconds
#                     expr      min       lq   median        uq       max
#   svd(X, nu = 5, nv = 5) 37878.46 39475.63 40358.68  41649.39 142594.23
#              fast.svd(X) 29812.35 31112.91 31741.84  32633.31 133708.70
#           svds(X, k = 5) 22618.61 23783.08 24038.23  24843.90  32687.29
# irlba(X, nu = 5, nv = 5) 92601.41 95066.86 96946.68 100925.54 196666.90
