library(ggplot2)

working.directory = Sys.getenv("FinalProject")
setwd(working.directory)

source("ModelSelectCriterions.R")
source("Data.R")
source("Lasso.R")
# Whether you want to run models or just load the function 
.main. = TRUE

###############################################################################
### 1. Functions
###############################################################################

RunAllY = function(Model = RunGlmnetIC, ...)
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

###############################################################################
### 2. Run The Model
###############################################################################
if (.main. == TRUE)
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
  dfCV.se  = RunAllY(Model = RunGlmnetCV, X = X, Y = Y, 
                     train = train, val = val, s = "lambda.1se")
  dfCV.min = RunAllY(Model = RunGlmnetCV, X = X, Y = Y, 
                     train = train, val = val, s = "lambda.min")
  cat("Running Lasso ESCV \n")
  dfES = RunAllY(Model = escv.glmnet, X = X, Y = Y, train = train, val = val, 
                 nfolds = 5, ntaus = 100, verbose = TRUE)
  
  dfAIC$MS = 'AIC'; dfAICc$MS = 'AICc'; dfBIC$MS = 'BIC'; dfCV$MS = 'CV'
  dfES$MS = 'ESCV'
  df = rbind(dfAIC, dfAICc, dfBIC, dfCV, dfES)
  png(SafeFileName("./graphs/Lasso_Cor000.png"))
  ggplot(data = df) + geom_line(aes(x = voxel,  y = Cor, color = MS))
  dev.off()
  png(SafeFileName("./graphs/Lasso_MSE000.png"))
  ggplot(data = df) + geom_line(aes(x = voxel,  y = MSE, color = MS))
  dev.off()
  png(SafeFileName("./graphs/Lasso_Lambda000.png"))
  ggplot(data = df) + geom_line(aes(x = voxel,  y = Lambda, color = MS)) +
    scale_y_log10()
  dev.off()
  png(SafeFileName("./graphs/Lasso_Df000.png"))
  ggplot(data = df) + geom_line(aes(x = voxel,  y = DF, color = MS)) +
    scale_y_log10()
  save(df, file = SafeFileName("./RData//LassoModelSelection000.RData", 5))
}

# Experiment Code
if (FALSE)
{
  lbda = sort(0.001*2^(seq(from = 0, to = 10, by = 0.1)), decreasing = TRUE)
  glmnet.fit = glmnet(X[train, ], Y[train, 1], 
                      family = "gaussian", standardize = FALSE, 
                      intercept = TRUE)
  plot(glmnet.fit, xvar = "lambda")
  a = glmnet.fit$beta
  
  plot(log10(1+colSums(abs(a))))
  df1 = melt(es, id = 'id', variable_name = 'series')
  ggplot(df1, aes(id, value)) + geom_line(aes(colour = series))
}