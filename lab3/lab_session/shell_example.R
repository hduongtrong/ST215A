library(foreach)
library(doParallel)

# Your script goes here.  Remember that it has to produce some kind of saved output.

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
ncores = 4
registerDoParallel(ncores)

parallel.results <- foreach(i = 1:4) %dopar% {
  # Make sure that each process has its own random number stream.
  set.seed(i)
  data.list <- GenerateData()
  filename <- sprintf("regression_%d.csv", i)
  write.csv(lm(y ~ x, data.list)$coefficients, file=filename,
            quote=F, row.names=F)
  return(filename)
}