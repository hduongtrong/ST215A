library(microbenchmark)
library(parallel)
library(doParallel)
library(rlecuyer)
library(ggplot2)


nCores = 2
registerDoParallel(cores = nCores)

EM = function(X, n_steps = 100, eps = 1e-3, density1 = dpois,
              density2 = dpois)
{
  # Initiate Data
  #mu1 = sample(X,1); mu2 = sample(X,1); p = 0.5;
  mu1 = X[1]+0.1; mu2 = X[2]+0.2; p = 0.5
  n = length(X); gamma = numeric(n)

  # Define Responsibility Function to Calculate the E step
  responsibility = function(x, mu1, mu2, p)
  { 
    A = p*density2(x,mu2)
    A/(A+(1-p)*density1(x,mu1))
    #p*density2(x,mu2)/(p*density2(x,mu2)+(1-p)*density1(x,mu1))
  }
  for (i in 1:n_steps)
  {
    gamma = as.numeric(lapply(X,
      function(x){responsibility(x, mu1, mu2, p)}))
    
    mu1.old = mu1; mu2.old = mu2; p.old = p;
    mu1 = sum((1-gamma)*X)/(n-sum(gamma))
    mu2 = sum(gamma*X)/(sum(gamma))
    p  = sum(gamma)/n
    #if (max(abs(mu1.old - mu1), abs(mu2.old - mu2), abs(pi.old - pi)) < eps)
    #  break;  
  }
  c(mu1, mu2, p) 
}
## Test the EM function
EM.CI = function(n.times = 100, n.samples = 1000, mu1 = 1, mu2 = 4, p = 0.3)
{
  RNGkind("L'Ecuyer-CMRG")
  out = foreach(i = 1:n.times) %dopar% {
    print(paste('Starting ', i, 'th job.\n', sep = '')); flush.console();
    X1 = rpois(n.samples, mu1); X2 = rpois(n.samples, mu2);
    D = rbinom(n.samples, 1, p); X = (1-D)*X1 + D*(X2)
    outSub = EM(X)
    cat('Finishing ', i, 'th job.\n', sep = ''); flush.console()
    outSub
  }
  out
}

res = EM.CI(100, 1000)
# Convert list to matrix
res = do.call(rbind,res)
# Since the two distribution order is not specified, for 
# each given original distribution, e.g. 30% chance from 
# Poisson(1) and 70% chance from Poisson(4). The algorithm
# can result in mu1 = 4 and mu2 = 1, with probability switched.
# We write function fix.output to take into account this 
# non-ordered property of two density function. 
fix.output = function(res)
{
  r = matrix(, nrow = nrow(res), ncol = ncol(res))
  ii1 = res[,1]<res[,2]
  r[,1][ii1] = res[,1][ii1]
  r[,2][ii1] = res[,2][ii1]
  r[,3][ii1] = res[,3][ii1]
  ii2 = res[,1]>= res[,2]
  r[,1][ii2] = res[,2][ii2]
  r[,2][ii2] = res[,1][ii2]
  r[,3][ii2] = 1 - res[,3][ii2]
  r
}
r = fix.output(res)
res2 = EM.CI(100, 1000, 2, 3)
res2 = do.call(rbind, res2)
r2 = fix.output(res2)

df = data.frame(r,r2);
names(df) = c("m11","m21","p1","m12","m22","p2")

ggplot(data=df) + 
  geom_density(aes(m11), color = "red") +
  geom_density(aes(m21), color = "red") +
  geom_density(aes(m12), color = "blue") +
  geom_density(aes(m22), color = "blue") + 
  xlab("Estimate of Mean of Mixture two-Poisson by EM") +
  ylab("Distribution of Estimates over 100 simulations") + 
  ggtitle("First parameter pair of Two-Poisson: (1,4) in red.
           Second parameter pair of Two-Poisson: (2,3) in blue")
#debug(EM)
#Rprof("exampleAgg.out")
#y = EM(X)
#Rprof(NULL)
#print(summaryRprof("exampleAgg.out"))
#print(microbenchmark(EM(X), times = 1))