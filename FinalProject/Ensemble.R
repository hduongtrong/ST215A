library(nnls)
library(randomForest)
library(gbm)
library(e1071)
library(nnet)
library(monmlp)
library(kernlab)
library(pls)

RunLassoCV = function(X = X, Y = Y, train = train, val = val,
                       Y.col = 1, X.cols = X.cols)
{
  tm = Sys.time()
  glmnetcv.fit = cv.glmnet(X[train, X.cols], Y[train, Y.col], 
                           family = "gaussian", standardize = FALSE, 
                           intercept = TRUE, nfolds = 5, alpha = alpha)
  cat("Running Time (s): ", 
      round(difftime(Sys.time(), tm, units = "secs"), 2), Y.col, "\n")
  yhat = predict(glmnetcv.fit, X[val, X.cols])
}

RunGbm = function(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                  train = train, val = val, get.yhat = TRUE, 
                  shrinkage = 0.05)
{
  tm = Sys.time()
  model = gbm.fit(x = X[train, X.cols], y = Y[train, Y.col], 
                  verbose = FALSE, shrinkage = 0.05, distribution = "gaussian")
  yhat = predict(model, X[val, X.cols], n.trees = 100)
  cat("Running Time (s): ", 
      round(difftime(Sys.time(), tm, units = "secs"), 2), Y.col, "\n")
  if (get.yhat) return(yhat)
  ReportPerf(Y[val, Y.col], yhat)
}

RunRandomForest = function(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                           train = train, val = val, get.yhat = TRUE, 
                           ntree = 300, mtry = 200)
{
  tm = Sys.time()
  model = randomForest(x = X[train, X.cols], y = Y[train, Y.col], 
                       ntree = ntree, mtry = mtry)
  yhat = predict(model, X[val, X.cols])
  cat("Running Time (s): ", 
      round(difftime(Sys.time(), tm, units = "secs"), 2), Y.col, "\n")
  if (get.yhat) return(yhat)
  ReportPerf(Y[val, Y.col], yhat)
}

RunSVM = function(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                  train = train, val = val, cost = 2)
{
  tm = Sys.time()
  svm.fit = svm(x = X[train, X.cols], y = Y[train, Y.col], cost = 2, 
                kernel = "linear")
  cat("Running Time (s): ", 
      round(difftime(Sys.time(), tm, units = "secs"), 2), Y.col, "\n")
  yhat = predict(svm.fit,    X[val,   X.cols])
}

RunNeuralNet = function(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                        train = train, val = val, decay = 40)
{
  tm = Sys.time()
  model = nnet(x = X[train, X.cols], y = Y[train, Y.col], size = 5, 
               rang = 0.5, decay = 40, MaxNWts = 6000, maxit = 100, 
               trace = TRUE)
  cat("Running Time (s): ", 
      round(difftime(Sys.time(), tm, units = "secs"), 2), Y.col, "\n")
  yhat = predict(model, X[val, X.cols])
}


RunEnsemble = function(Y.col = 1)
{
  # 1. Models on full X's
  X.cols = 1:ncol(X)
  cat("1. Fitting models on full set of inputs\n\n")
  cat("1.1. Fitting Lasso\n")
  yhat.lasso.full = RunLassoCV(X = X, Y = Y, train = train, val = val,
                                     Y.col = Y.col, X.cols = X.cols)
  cat("1.2. Fitting Gradient Boosting Machine\n")
  yhat.gbm.full   = RunGbm(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                           train = train, val = val, get.yhat = TRUE, 
                           shrinkage = 0.02)
  cat("1.3. Fitting Random Forest\n")
  yhat.rf.full    = RunRandomForest(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                                    train = train, val = val, get.yhat = TRUE, 
                                    ntree = 300, mtry = 200)
  cat("1.4. Fitting Support Vector Machine\n")
  yhat.svm.full = RunSVM(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                                    train = train, val = val, cost = 2)
  # Models on X selected by Lasso, 5% inputs picked
  X.cols = VarSelectLasso(X[train, ], Y[train, Y.col], pct = 0.05)
  cat("\n2.1. Fitting Lasso\n\n")
  yhat.lasso.lasso = RunLassoCV(X = X, Y = Y, train = train, val = val,
                               Y.col = Y.col, X.cols = X.cols)
  cat("2.2. Fitting Gradient Boosting Machine\n")
  yhat.gbm.lasso   = RunGbm(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                            train = train, val = val, get.yhat = TRUE, 
                            shrinkage = 0.05)
  cat("2.3. Fitting Random Forest\n")
  yhat.rf.lasso  = RunRandomForest(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                                     train = train, val = val, get.yhat = TRUE, 
                                     ntree = 300, mtry = 80)
  cat("2.4. Fitting Support Vector Machine\n")
  yhat.svm.lasso = RunSVM(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                         train = train, val = val, cost = 2)
  yhat.nn.lasso  = RunNeuralNet(X = X, Y = Y, Y.col = Y.col, X.cols = X.cols, 
                          train = train, val = val, decay = 40)
  
  yhats = cbind(yhat.lasso.full, yhat.gbm.full, yhat.rf.full, yhat.svm.full,
                yhat.lasso.lasso, yhat.gbm.lasso, yhat.rf.lasso, 
                yhat.svm.lasso, yhat.nn.lasso)
  idv.cor = apply(yhats, 2, function(x) cor(x, Y[val, Y.col]))
  ens = list()
  ens$idvcor = idv.cor
  nnls.fit = nnls(yhats, Y[val, Y.col])
  ens$coef = nnls.fit$x
  ens$yhat = yhats%*%ens$coef
  ens$cor = cor(ens$yhat, Y[val, Y.col])
  ens$struc = cor(yhats)
  ens$yhats = yhats
  ens
}

for (i in 1:20)
{
  res = RunEnsemble(Y.col = i)
  filename = paste("./RData/", "ens", i, ".RData", sep = "")
  save(res, file = filename)
}


if (FALSE)
{
  Y.col = 1
  X.cols = 1:ncol(X)
  glmnet.fit = cv.glmnet(X[train, X.cols], Y[train, Y.col], 
                      family = "gaussian", standardize = FALSE, 
                      intercept = TRUE, nfolds = 5, alpha = alpha)
  rf.fit = randomForest(x = X[train, X.cols], y = Y[train, Y.col], 
                       ntree = 300, mtry = 200, importance = TRUE)
  gbm1.fit = gbm.fit(x = X[train, X.cols], y = Y[train, Y.col], 
                    verbose = TRUE, shrinkage = 0.05, distribution = "gaussian")
  svm.fit = svm(x = X[train, X.cols], y = Y[train, 1], cost = 2, 
                kernel = "linear")
  yhat1 = predict(glmnet.fit, X[val,   X.cols])
  yhin1 = predict(glmnet.fit, X[train, X.cols])
  yhat2 = predict(rf.fit,     X[val,   X.cols])
  yhin2 = predict(rf.fit,     X[train, X.cols])
  yhat3 = predict(gbm1.fit,   X[val,   X.cols], n.trees = 100)
  yhin3 = predict(gbm1.fit,   X[train, X.cols], n.trees = 100)
  yhat4 = predict(svm.fit,    X[val,   X.cols])
  yhin4 = predict(svm.fit,    X[train, X.cols])
  
  cor(yhat1, Y[val, Y.col])
  cor(yhin1, Y[train, Y.col])
  cor(yhat2, Y[val, Y.col])
  cor(yhin2, Y[train, Y.col])
  cor(yhat3, Y[val, Y.col])
  cor(yhin3, Y[train, Y.col])
  cor(yhat4, Y[val, Y.col])
  cor(yhin4, Y[train, Y.col])
  cor(yhat1 + yhat2 + yhat3 + yhat4, Y[val, Y.col])
  
  ens.model = nnls(cbind(yhat1, yhat2, yhat3, yhat4), Y[val, Y.col])
  ens.coef = ens.model$x
  cor(cbind(yhat1, yhat2, yhat3, yhat4)%*%ens.coef, Y[val, Y.col])
  ens.coef
  
  ################################################################################
  ###
  ################################################################################
  
  
  model = nnet(x = X[train, X.cols], y = Y[train, Y.col], size = 5, 
               rang = 0.5, decay = 200, MaxNWts = 6000, maxit = 100, trace = TRUE)
  yhat = predict(model, X[train, X.cols])
  cor(yhat, Y[train, Y.col])
  yhat = predict(model, X[val, X.cols])
  cor(yhat, Y[val, Y.col])
  
  Y.col = 1
  model = monmlp.fit(X[train, X.cols], as.matrix(Y[train, Y.col]), scale.y = TRUE, 
                     n.trials = 1, hidden1 = 5, hidden2 = 5, 
                     n.ensemble = 10, Th = tansig, To = linear, 
                     Th.prime = tansig.prime, To.prime = linear.prime, 
                     iter.max = 100, monotone = NULL, bag = TRUE, 
                     init.weights = c(-0.5, 0.5), max.exceptions = 10,
                     silent = FALSE)
  yhat = monmlp.predict(X[train, X.cols], weights = model)
  cor(yhat, Y[train, 1])
  yhat = monmlp.predict(X[val  , X.cols], weights = model)
  cor(yhat, Y[val, Y.col])
  
  model = gausspr(X[train, X.cols], Y[train, Y.col], type = NULL, 
                  kernel = 'vanilladot', kpar = 'automatic', var = 0.1, 
                  variance.model = TRUE, tol = 0.05, cross = 0, fit = FALSE)
  yhat = predict(model, X[train, X.cols])
  cor(yhat, Y[train, Y.col])
  yhat = predict(model, X[val, X.cols])
  cor(yhat, Y[val, Y.col])
  
  X.cols = 1:ncol(X)
  model = mvr(Y[train, Y.col] ~ ., data = data.frame(X[train, X.cols]),  
              ncomp = 80, method = pls.options()$pcralg, scale = F)
  yhat = predict(model, X[train, X.cols])
  yhat = rowMeans(yhat)
  length(yhat)
  cor(yhat, Y[train, Y.col])
  yhat = predict(model, X[val, X.cols])
  yhat = rowMeans(yhat)
  cor(yhat, Y[val, Y.col])
}