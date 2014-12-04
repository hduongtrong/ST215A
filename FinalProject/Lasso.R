library(glmnet)

alpha = 1

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
                       Y.col = 1, s = "lambda.min")
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
  best.lambda = glmnetcv.fit[[s]]
  yhat = predict(glmnetcv.fit, X[val, ], s = best.lambda)
  best.model = which(glmnetcv.fit$lambda == best.lambda)[1]
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
  # es = sapply(list.tau, function(x) ESCVHelper(cv, tau.matrix, x, 
  #                                             verbose = verbose))
  es = foreach(i = 1:length(list.tau)) %dopar% {
    ESCVHelper(cv, tau.matrix, list.tau[i], verbose = verbose)
  }
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