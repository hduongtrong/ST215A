# Example usage of RCpp.
library('Rcpp')
library('microbenchmark')
library('RhpcBLASctl')
library('rARPACK')

#working.directory <- "~/Dropbox/School/ST215/Lab/lab3/"
working.directory <- "/accounts/grad/hduong/Documents/ST215A/ST215A/lab3/"
# Flags for openMP.  If you're not using openMP, you don't need these.
# Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
# Sys.setenv("PKG_LIBS"="-fopenmp")
# Sys.setenv("PKG_CXXFLAGS"="-I/path/ -fopenmp -I/Users/hd/Downloads/eigen-eigen-1306d75b4a21/")
#

blas_set_num_threads(12)
sourceCpp(file.path(working.directory, 'FastDist.cpp'))
sourceCpp(file.path(working.directory, 'FastDistARMA.cpp'))
n = 2e3
M = matrix(rnorm(n^2),nrow = n, ncol = n)
M = t(M)%*%M
# print(system.time(eigen(M)))
# print(system.time(getEigenValuesEG(M)))
# print(system.time(getEigenValuesARMA(M)))
print(system.time(eigs_sym(M, 3)))
