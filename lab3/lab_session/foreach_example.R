# Example based on Chris Paciorek's tutorial:
# http://www.stat.berkeley.edu/scf/paciorek-parallelWorkshop.pdf
#
# Note: there is a conflict between openBLAS, the multi-threaded linear
# algebra package, and foreach.  It can cause linear algebra operations
# within a foreach loop to hang
# If your system uses openBLAS (note that the SCF computers do),
# then before running R, execute the command:
#
# export OMP_NUM_THREADS=1
#
# This command sets an environment variable that tells BLAS to only
# use a single thread.

library('foreach')
library('doParallel')


# Draw a random dense linear model (some boring data)
GenerateData <- function() {
  p <- 100
  n <- 1000
  x <- matrix(rnorm(p*n),nrow = n, ncol = p)
  beta <- rt(p, df = 3)
  y <- x %*% beta
  return(list(y = y, x = x))
}

# Set the number of cores.  On a UNIX machine, you can
# look at /proc/cpuinfo.  On a mac, you can look at the system
# profiler.
ncores = 2
registerDoParallel(ncores)

# How many times to fit the linear model.
repetions <- 500

# At this point, open up a terminal and run top to watch
# what is going on.

# Serial execution of lm
start.time <- Sys.time()
serial.results <- list()
for (i in 1:repetions) {
  data.list <- GenerateData()
  serial.results[[i]] <- lm(y ~ x, data.list)$coefficients
}
duration.1 <- Sys.time() - start.time
# Parallel execution of lm
start.time <- Sys.time()
parallel.results <- foreach(i = 1:repetions) %dopar% {
  data.list <- GenerateData()
  return(lm(y ~ x, data.list)$coefficients)
}
duration.2 <- Sys.time() - start.time

print(duration.1)
print(duration.2)

x = rnorm(1e8)

f = function()
{
  for (i in 1:length(x))
  {
    x[i] = x[i]+1
  }
}
g = cmpfun(f)
start.time <- Sys.time()
h(rnorm(1000,100))
duration.3 <- Sys.time() - start.time
h = cmpfun(dist)

n = 5000
x = matrix(runif(n*n), ncol = n, nrow = n)
start.time <- Sys.time()
x %*% x
d4 <- Sys.time() - start.time

