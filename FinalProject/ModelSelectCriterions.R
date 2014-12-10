GetRSS  = function(y, yhat) sum((y - yhat)^2)
GetMSE  = function(y, yhat) sum((y - yhat)^2)/length(y)
GetRSSs = function(y, yhats) apply(yhats, 2, function(.yhat) GetRSS(y, .yhat))
GetAIC  = function(RSS, n, k) n*log(RSS/n) + 2*k 
GetBIC  = function(RSS, n, k) n*log(RSS/n) + log(n)*k
GetAICc = function(RSS, n, k) n*log(RSS/n) + 2*k + 2*k*(k+1)/abs(n - k - 1)
GetLassoDf = function(nrow.X, df) sapply(df, function(x) min(x, nrow.X)) 
GetRidgeDF = function(X, lbda) tr(X %*% solve(t(X) %*% X + 
                    lbda * diag(ncol(X))) %*% t(X))
GetSlope = function(y, yhat) crossprod(yhat, y)/crossprod(yhat, yhat)
GetRsquared = function(y, yhat) 1 - GetRSS(y, yhat)/sum((y - mean(y))^2)
SelectModel = function(y, yhats, dfs, criterion)
{
  RSSs = GetRSSs(y, yhats)
  dfs  = GetLassoDf(length(y), dfs)
  n = length(y)
  apply(cbind(RSSs, dfs), 1, function(x) criterion(x[1], n, x[2]))
}

SafeFileName = function(filename, len.ext)
{
  while (file.exists(filename))
  {
    n = nchar(filename)
    name = substr(filename, 1, n - len.ext - 4)
    id   = as.numeric(substr(filename, n - len.ext - 3, n - len.ext -1)) + 1
    id   = sprintf("%03d", id)
    ext  = substr(filename, n - len.ext + 1, n)
    filename = paste(name, id, ".", ext, sep = "")
  }
  filename
}
ReportPerf = function(y, yhat)
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
  res = c(cor(        y, yhat), 
          GetSlope(   y, yhat),
          GetRsquared(y, yhat),
          GetMSE(     y, yhat))
  res = matrix(res, 1, length(res))
  col.names = c('Cor', 'Slope', 'R-Square', 'MSE')
  colnames(res) = col.names
  res
}
