library(glmnet)
library(ggplot2)

working.directory = Sys.getenv("FinalProject")
alpha = Sys.getenv("alpha")
setwd(working.directory)
source("ModelSelectCriterions.R")

# Whether you want to load data or not
.load.data. = TRUE
# Whether you want to run models or not. Setting TRUE will also load data.
.main. = TRUE
if (.main.) .load.data = TRUE
# If both are set to FALSE, only functions are loaded

###############################################################################
### 1. Functions
###############################################################################
Plot.X = function(.X = X, func = sd)
{
  col.axis = 2
  col.sd = apply(X, col.axis, func)
  plot(col.sd)
}

CleanData = function(X, train)
{
  # Description: 
  #   This function standardize the data with respect to the 
  #   training samples. So, the training data part will have mean zero, 
  #   standard deviation 1, but the test data part might not (though should be
  #   close). Also, constant columns are removed
  # Args: 
  #   X    : A matrix of data to be standardized (column-wise)
  #   train: the row index in X that belong to the train dataset
  # Return: 
  #   The standardized matrix of X
  col.sd = apply(X[train, ], 2, sd)
  X = X[, col.sd != 0]
  col.m = colMeans(X[train, ])
  col.sd = col.sd[col.sd != 0]
  scale(X, center = col.m, scale = col.sd)
}


ReportGlmnetPerf = function(y, yhat, model, best.model)
{
  # Description: Report the performance of a glmnet or cv.glmnet model
  # Args:
  #   y         : the true output
  #   yhat      : the prediced output
  #   model     : the glmnet fitted model, to extract the degree of freedom and 
  #               lambda parameter
  #   best.model: index of the chosen model (out of many lambdas)
  # Return: 
  #   A vector of correlation, slope, R-squared, MSE, lambda, and degree of 
  #   freedom for the selected (best) model
  
  # In cv.glmnet, degree of freedom is reported as nzero
  if ('nzero' %in% names(model)) model$df = model$nzero 
  res = c(cor(        y, yhat), 
          GetSlope(   y, yhat),
          GetRsquared(y, yhat),
          GetMSE(     y, yhat),
          model$lambda[best.model], 
          model$df[best.model])
  res = matrix(res, 1, length(res))
  col.names = c('Cor', 'Slope', 'R-Square', 'MSE', 'Lambda', 'Df')
  colnames(res) = col.names
  res
}

RunGlmnetIC = function(X = X, Y = Y, train = train, val = val,
                       Y.col = 1, criterion = GetAICc)
{
  # Description: Run the glmnet model for one column of Y, then report
  #              performance on validation set. Model is selected by 
  #              criterion (AIC, BIC, AICc)
  # Args:
  #   X        : the input matrix
  #   Y        : the output matrix
  #   Y.col    : the output column to predict
  #   train    : row index of train data
  #   val      : row index of test data
  #   criterion: One of (GetAICc, GetAIC, GetBIC). Criterion to select the best
  #              model
  # Return:
  #   Performance of model as reported using ReportGlmnetPerf function
  tm = Sys.time()
  glmnet.fit = glmnet(X[train, ], Y[train, Y.col], 
                      family = "gaussian", standardize = FALSE, 
                      intercept = TRUE, alpha = alpha)
  yhats = predict(glmnet.fit, X[train, ])
  IC = SelectModel(Y[train, Y.col], yhats, glmnet.fit$df, criterion)
  best.model = which(IC == min(IC))[1]
  
  yhat = predict(glmnet.fit, X[val, ], s = glmnet.fit$lambda[best.model])
  cat("Running Time (s): ", 
      round(difftime(Sys.time(), tm, units = "secs"), 2), Y.col, "\n")
  ReportGlmnetPerf(y = Y[val, Y.col], yhat = yhat, 
                model = glmnet.fit, best.model)
}

RunGlmnetCV = function(X = X, Y = Y, train = train, val = val,
                       Y.col = 1)
{
  # Description: Run the cv.glmnet model for one column of Y, then report
  #              performance on validation set.
  # Args:
  #   X        : the input matrix
  #   Y        : the output matrix
  #   Y.col    : the output column to predict
  #   train    : row index of train data
  #   val      : row index of test data
  # Return:
  #   Performance vector of model as reported using ReportGlmnetPerf function
  tm = Sys.time()
  glmnetcv.fit = cv.glmnet(X[train, ], Y[train, Y.col], 
                      family = "gaussian", standardize = FALSE, 
                      intercept = TRUE, nfolds = 5, alpha = alpha)
  yhat = predict(glmnetcv.fit, X[val, ], s = glmnetcv.fit$lambda.min)
  best.model = which(glmnetcv.fit$lambda == glmnetcv.fit$lambda.min)[1]
  cat("Running Time (s): ", 
      round(difftime(Sys.time(), tm, units = "secs"), 2), Y.col, "\n")
  ReportGlmnetPerf(y = Y[val, Y.col], yhat = yhat, model = glmnetcv.fit, 
                best.model)
}

ESCVHelper = function(cv, df.tau, tau, verbose = TRUE)
{
  if (verbose) cat("Calculate ESCV for L1-Norm:", tau, "\n")
  idx = apply(df.tau, 2, function(x) which.min(abs(x - tau)))
  yhat = list()
  for (i in 1:5)
  {
    yhat[[i]] = predict(cv[[i]], X[train, ], s = cv[[i]]$lambda[idx[i]])
  }
  yhat = matrix(unlist(yhat), nrow = length(yhat[[1]]))
  yhat.m = rowMeans(yhat)
  var.ytau = sum(GetRSSs(yhat.m, yhat))/ncol(yhat)
  var.ytau/sum(yhat.m^2)
}

escv.glmnet = function(X = X, Y = Y, train = train, val = val, Y.col = 1,
                       nfolds = 5, ntaus = 10, verbose = TRUE)
{
  tm = Sys.time()
  fold.len = round(length(train)/nfolds)
  cv = list()
  fold.out = list()
  for (i in 1:nfolds)
  {
    if (verbose) cat("Run model on fold:", i, "\n")
    fold.idx = ((i - 1)*fold.len + 1):(i*fold.len)
    fold.in = train[-fold.idx]
    fold.out[[i]] = train[fold.idx]
    cv[[i]] = glmnet(X[fold.in, ], Y[fold.in, Y.col], family = "gaussian", 
                     standardize = FALSE, intercept = TRUE, alpha = alpha)
  }
  
  tau.matrix = list()
  for (i in 1:nfolds)
  {
    tau.matrix[[i]] = colSums(abs(cv[[i]]$beta))
  }
  tau.matrix = matrix(unlist(tau.matrix), nrow = length(tau.matrix[[1]]))
  list.tau = seq(from = min(tau.matrix), to = max(tau.matrix), 
                 length.out = ntaus)
  es = sapply(list.tau, function(x) ESCVHelper(cv, tau.matrix, x, 
                                               verbose = verbose))
  best.tau = list.tau[which.min(es)]
  
  # Now fit the model with best tau (L1 norm) parameter
  model = glmnet(X[train, ], Y[train, Y.col], family = "gaussian", 
                 standardize = FALSE, intercept = TRUE, alpha = alpha)
  all.taus = colSums(abs(model$beta))
  best.tau.idx = which.min(abs(all.taus - best.tau))
  yhat = predict(model, X[val, ], s = model$lambda[best.tau.idx])
  cat("Running Time (s): ", 
      round(difftime(Sys.time(), tm, units = "secs"), 2), Y.col, "\n")
  ReportGlmnetPerf(y = Y[val, Y.col], yhat = yhat, 
                   model = model, best.model = best.tau.idx)
}

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
### 2. Load The Data
###############################################################################
if (.load.data. == TRUE)
{
  load("fMRIdata.RData")
  X = fit_feat; Y = resp_dat; rm(fit_feat); rm(resp_dat) # Change name
  set.seed(1)
  shuffle = sample(nrow(X), nrow(X))
  train = shuffle[1:1000]; 
  val = shuffle[1001:1400]; 
  test = shuffle[1401:nrow(X)];
  # Standardize X with respect to the training set
  X = CleanData(X, train)
}

###############################################################################
### 3. Run The Model
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
  dfCV = RunAllY(Model = RunGlmnetCV, X = X, Y = Y, 
                 train = train, val = val)
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