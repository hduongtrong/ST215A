# Example usage of RCpp.

library('Rcpp')
library('microbenchmark')

working.directory <- file.path(Sys.getenv("GIT_REPO_LOC"),
                               "classes/STAT215A_Fall2013/lab_sessions/",
                               "10-07-2014_lab3/")

# Flags for openMP.  If you're not using openMP, you don't need these.
Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
Sys.setenv("PKG_LIBS"="-fopenmp")

sourceCpp(file.path(working.directory, 'Rcpp_demo.cpp'))

x = rnorm(1e7)
y = rnorm(1e7)
z <- cbind(x, y)

# Define our own R distance function
DistanceR <- function(x, y) {
  return(sqrt(sum((x - y) ^ 2)))
}

# Test our R version vs. our C++ version, with a strangely underperforming
# openMP function to boot.
microbenchmark(DistanceR(x, y),
               DistanceCPP(x, y),
               DistanceMatrixCPP(z),
               DistanceCPPParallel(x, y), times = 5)

# All the answers are the same:
DistanceCPP(x, y)
DistanceCPPParallel(x, y)
DistanceR(x, y)
DistanceMatrixCPP(z)
