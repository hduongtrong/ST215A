library(ggplot2)
library(parallel)
library(doParallel)
library(foreach)

working.directory = Sys.getenv("FinalProject")
setwd(working.directory)

source("ModelSelectCriterions.R")
source("Data.R")
source("Lasso.R")


nCores = as.numeric(Sys.getenv('NSLOTS'))
registerDoParallel(nCores)

# Whether you want to run models or just load the function 
.main. = FALSE

###############################################################################
### 1. Functions
###############################################################################

RunAllY1 = function(Model = RunGlmnetIC, ...)
{
  # Description: Run a model on all columns of Y and get the performance
  # Args:
  #   Model: The model, e.g. RunGlmnet, RunGlmnetCV
  #   ...  : Arguments for that model
  # Returns:
  #   A data frame of performance as reported by ReportGlmnetPerf, each row is
  #   for a column of Y.
  for (i in 1:ncol(Y))
  {
    res = Model(..., Y.col = i)
    if (i == 1) 
    {
      df = data.frame(matrix(ncol = ncol(res), nrow = ncol(Y)))
      colnames(df) = colnames(res)
    }
    df[i,] = res
  }
  df$voxel = 1:ncol(Y)
  df
}
RunAllY2 = function(Model = RunGlmnetIC, ...)
{
  # Parallel Version of RunAllY1
  out = foreach (i = 1:ncol(Y)) %dopar%
  {
    Model(..., Y.col = i)
  }
  out = do.call(rbind, out)
  out$voxel = 1:ncol(Y)
}

if (nCores > 1) {
  RunAllY = RunAllY2
} else {
  RunAllY = RunAllY1
}

VarSelectCor = function(X, y, pct = 0.10)
{
  list.cor = abs(apply(X, 2, function(x) cor(x, y)))
  cutoff = quantile(list.cor, 1 - pct)
  which(list.cor > cutoff)
}

VarSelectRanForest = function(X, y, pct = 0.10)
{
  model = randomForest(x = X, y = y, 
                       ntree = 300, mtry = 500, importance = TRUE)
  imp = model$importance
  cutoff = quantile(imp, 1 - pct)
  which(imp > cutoff)
}

VarSelectLasso = function(X, y, pct = 0.10)
{
  model = glmnet(X, y, 
                 family = "gaussian", standardize = FALSE, 
                 intercept = TRUE, alpha = alpha)
  b = coef(model)
  a = apply(b, 2, function(x) sum(x != 0))
  n.nonzero = round(ncol(X)*pct)
  matched.idx.sorted = findInterval(x = n.nonzero, vec = sort(a))
  matched.value  = sort(a)[matched.idx.sorted]
  matched.idx    = which(a == matched.value)
  res = b[2:nrow(b), matched.idx]
  which(res != 0)
}
###############################################################################
### 2. Run The Model
###############################################################################
if (.main.)
{
  cat("Running Lasso AIC \n")
  dfAIC = RunAllY(Model = RunGlmnetIC, X = X, Y = Y, 
                  train = train, val = val, criterion = GetAIC)
  cat("Running Lasso AICc \n")
  dfAICc = RunAllY(Model = RunGlmnetIC, X = X, Y = Y, 
                   train = train, val = val, criterion = GetAICc)
  cat("Running Lasso BIC \n")
  dfBIC = RunAllY(Model = RunGlmnetIC, X = X, Y = Y, 
                  train = train, val = val, criterion = GetBIC)
  cat("Running Lasso CV \n")
  dfCV  = RunAllY(Model = RunGlmnetCV, X = X, Y = Y, 
                     train = train, val = val, s = "lambda.1se")
  cat("Running Lasso ESCV \n")
  dfES = RunAllY(Model = escv.glmnet, X = X, Y = Y, train = train, val = val, 
                 nfolds = 5, ntaus = 100, verbose = TRUE, parallel = FALSE)
  
  dfAIC$MS = 'AIC'; dfAICc$MS = 'AICc'; dfBIC$MS = 'BIC'; dfCV$MS = 'CV'
  dfES$MS = 'ESCV'
  df = rbind(dfAIC, dfAICc, dfBIC, dfCV, dfES)
  png("./graphs/Lasso_Cor%03d.png")
  ggplot(data = df) + geom_line(aes(x = voxel,  y = Cor, color = MS))
  dev.off()
  png("./graphs/Lasso_MSE%03d.png")
  ggplot(data = df) + geom_line(aes(x = voxel,  y = MSE, color = MS))
  dev.off()
  png("./graphs/Lasso_Lambda%03d.png")
  ggplot(data = df) + geom_line(aes(x = voxel,  y = Lambda, color = MS)) +
    scale_y_log10()
  dev.off()
  png("./graphs/Lasso_Df%03d.png")
  ggplot(data = df) + geom_line(aes(x = voxel,  y = DF, color = MS)) +
    scale_y_log10()
  save(df, file = SafeFileName("./RData/LassoModelSelection000.RData", 5))
}



