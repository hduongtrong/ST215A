GetRSS  = function(y, yhat) sum((y - yhat)^2)
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



