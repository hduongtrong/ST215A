library(parallel)
library(doParallel)
library(foreach)
library(rlecuyer)

working.directory = "~/Documents/ST215A/ST215A/lab3"
nCores <- as.numeric(Sys.getenv('NSLOTS'))

setwd(working.directory)
registerDoParallel(nCores)

source("DataProcessing.R")
l = getTrainTestBlock(list(image1, image2, image3),k=3, train.pct = 1,
                      fix.random = FALSE)
data = l[[1]]

k = 3; n.images = 3;
RNGkind("L'Ecuyer-CMRG")
out <- foreach(i = 1:32) %dopar% {
  cat('Starting', i, 'th job.\n', sep = ' ')
  train.blocks = sample(n.images*k^2, 15)
  train.idx = data$blockid %in% train.blocks
  model = lm(label ~ NDAI + SD + CORR + DF + CF + 
                    BF   + AF + AN + logSD, 
                  data = data[train.idx,])
  label.hat = predict.lm(model, data[!train.idx, ])
  auc(data[!train.idx,3], label.hat)
}
save(unlist(out),file = "LinearModelAUC.RData")