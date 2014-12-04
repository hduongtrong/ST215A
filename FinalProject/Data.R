working.directory = Sys.getenv("FinalProject")
setwd(working.directory)

.load.data. = TRUE

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

Plot.X = function(.X = X, func = sd)
{
  col.axis = 2
  col.sd = apply(X, col.axis, func)
  plot(col.sd)
}

if (.load.data.)
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