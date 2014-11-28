library(glmnet)
setwd("~/Dropbox/School/ST215/Lab/FinalProject/")
load("fMRIdata.RData")
source("ModelSelectCriterions.R")

Plot.X = function(.X = X, func = sd)
{
  col.axis = 2
  col.sd = apply(X, col.axis, func)
  plot(col.sd)
}

ReportGLMPerf = function(y, yhat, model, best.model)
{
  res = c(cor(        y, yhat), 
          GetSlope(   y, yhat),
          GetRsquared(y, yhat),
          model$lambda[best.model], 
          model$df[best.model])
  res = matrix(res, 1, length(res))
  col.names = c('Cor', 'Slope', 'R-Square', 'Lambda', 'Df')
  colnames(res) = col.names
  res
}

RunGlmnetIC = function(X = X, Y = Y, train = train, val = val,
                       Y.col = 1, criterion = GetAICc)
{
  tm = Sys.time()
  lbda = 0.001*2^(seq(from = 0, to = 10, by = 0.1))
  glmnet.fit = glmnet(X[train, ], Y[train, Y.col], 
                      family = "gaussian", standardize = TRUE, 
                      intercept = FALSE, lambda = lbda)
  yhats = predict(glmnet.fit, X[train, ])
  IC = SelectModel(Y[train, Y.col], yhats, glmnet.fit$df, criterion)
  best.model = which(IC == min(IC))[1]
  
  yhat = predict(glmnet.fit, X[val, ], s = glmnet.fit$lambda[best.model])
  cat("Running Time (s): ", round(Sys.time() - tm, 2), Y.col, "\n")
  ReportGLMPerf(y = Y[val, Y.col], yhat = yhat, model = glmnet.fit, best.model)
}

RunCVGlmnet = function(X = X, Y = Y, train = train, test = test,
                       X.cols = 1:ncol(X), Y.col = 1)
{
  glmnet.fit = cv.glmnet(X[train, X.cols], Y[train,Y.col], 
                      family = "gaussian", standardize = TRUE, 
                      intercept = TRUE, nfolds = 5)
  yhat = predict(glmnet.fit, X[test, X.cols])
  cor(yhat, Y[test, Y.col])  
}

RunAllY2 = function(Model = RunGlmnet, X = X, Y = Y, 
                   train = train, test = test,
                   X.cols = 1:ncol(X))
{
  res = rep(0, ncol(Y))
  for (i in 1:ncol(Y))
  {
    cat("Predicing Column: ", i, " out of ", ncol(Y), "\n")
    res[i] = Model(X = X, Y = Y, train = train, 
                   test = test, X.cols = X.cols, Y.col = i)
  }
  res
}

RunAllY = function(Model = RunGlmnetIC, ...)
{
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
  df
}
######################################################################
X = fit_feat; Y = resp_dat; rm(fit_feat); rm(resp_dat) # Change name
set.seed(1)
shuffle = sample(nrow(X), nrow(X))
train = shuffle[1:1000]; 
val = shuffle[1001:1400]; 
test = shuffle[1401:nrow(X)]; 
Y.col = 1

l = 0.001*sqrt(2)^(0:20)
l = seq(from = 0.01, to = 0.20, by = 0.001)
glmnet.fit = glmnet(X[train, ], Y[train, Y.col], 
                    family = "gaussian", standardize = TRUE, 
                    intercept = FALSE, lambda = l)
yhat = predict(glmnet.fit, X[train, ])
par(mfrow = c(2,2))
resBIC = SelectModel(Y[train, Y.col], yhat, glmnet.fit$df, GetBIC)
plot(glmnet.fit$lambda, resBIC)
resAIC = SelectModel(Y[train, Y.col], yhat, glmnet.fit$df, GetAIC)
plot(glmnet.fit$lambda, resAIC)
resAICc = SelectModel(Y[train, Y.col], yhat, glmnet.fit$df, GetAICc)
plot(glmnet.fit$lambda, resAICc)


glmnet.fit$lambda[resBIC == min(resBIC)]
glmnet.fit$lambda[resAICc == min(resAICc)]
glmnet.fit$df[res == min(res)]

#glmnet.fit = cv.glmnet(X[train, ], Y[train, 1], 
#                       family = "gaussian", standardize = TRUE, 
#                       intercept = FALSE, nfolds = 5)
