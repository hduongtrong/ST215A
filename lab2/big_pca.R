# Demonstrate the time-saving use of sparse matrices and irlba.

library("Matrix")
library("irlba")
library("microbenchmark")

GetCov <- function(p, m, max.corr = .5, sparse = T) {
  # Generate a sparse covariance matrix with limited off-diagonal elements.
  # Args:
  #   p: dimensionality of the cov mat
  #   m: number non-zero elements in each direction off the diagonal
  #   max.corr: maximum correlation between variables
  #   sparse: whether to use sparse data structure (Matrix vs matrix)
  #
  # Returns:
  #   A matrix with nonzeros close to the diagonal and zeros everywhere else
  #   Each row will look like 
  #       0 0 0 0 0 .1 .2 ... .9 1 .9  ... .2 .1 0 0 0 0 0

  r <- seq(max.corr, 0, length.out=m + 1)
  r <- r[ -length(r)]
  if (sparse) {
    mat <- Matrix(0, nrow = p, ncol = p, sparse = T)
  } else {
    mat <- matrix(0, nrow = p,ncol = p)
  }
  
  for (i in 1:length(r)) {
    mat[seq(from = i+1, by = p+1, length.out = p-i )] <- r[i]
  }
  
  mat <- mat + t(mat)
  diag(mat) <- 1
  return(mat)
}

# Generate the data
p <- 10
m <- 2

# Get the covariance matrix, set number of non zero off diagonals at 40
# First, use sparse matrices and check the size
m.sparse <- GetCov(p, m, .5, T)
object.size(m.sparse)

# Now use dense matrices and check the size
m.dense <- GetCov(p, m, .5, F)
object.size(m.dense)

# Get the Cholesky decomposition of the cov mat so we can generate the data matrix X
microbenchmark(t(chol(m.dense)), unit="ms")
microbenchmark(t(chol(m.sparse)), unit="ms")
A.sparse <- t(chol(m.sparse))
              
# Generate X with desired covariance
n <- 1000
X <- as.matrix(t(A.sparse %*% matrix(rnorm(n*p), p, n)))
X <- scale(X, center=TRUE, scale=TRUE)
plot(cov(X, X), m.sparse); abline(0, 1)

# Plot one realization of X - we can see the correlation structure
plot(X[1,], type = 'l')

# Calculate the PCA
# Using the built-in method
microbenchmark(prcomp(X), times=1, unit="ms")
pcas <- prcomp(X, center=FALSE, scale=FALSE)

# Use the irlba library to calculate the first few PCs
microbenchmark(irlba(X, nu = 5, nv = 5), times=1, unit="ms")
pcas_few <- irlba(X, nu = 5, nv = 5)

foo <- svd(X, nu=5, nv=5)
bar <- eigen(cov(X, X))

bar$values
pcas$sdev^2
foo$d
pcas_few$d



# Let's try our own power method to get the top PC
niter <- 500
covmat <- t(X) %*% (X)
vec <- rnorm(p)
for (i in 1:niter) {
  vec = covmat %*% vec
  vec = vec / sqrt(sum(vec^2))
}
# See what the singular value is
head(sqrt((covmat %*% vec) / vec))