library(pROC)
library(glmnet)
library(ggplot2)

accuracy = function(label, label.hat)
{
  # Return the percentage of time predicting correctly
  # over total number of prediction.
  t = table(label, label.hat)
  sum(diag(t))/sum(t)
}

meanError = function(label, label.hat)
{
  # The penalty function is defined as: If truth is 1:
  # predict 0 has penalty 0.5
  # predict -1 has penalty 1
  # predict 1 correctly has no penalty.
  t = table(label, label.hat)
  (0.5*(t[1,2]+t[2,1]+t[2,3]+t[3,2])+t[1,3]+t[3,1])/sum(t)
}

cutOff = function(label, thres = 0.5, k = 2)
{
  # Given a number x, if x > thres, return 1,
  # if x < -thres, return -1.
  # if - thres <= x <= thres return 0
  # where label > 0.5, set to 1, the rest 0
  x = (label > thres) + 0.0
  # where label < -0.5 set to -1 instead
  if (k == 3) x[label < -thres] = -1
  x
}

cutOffGridSearch = function(label, label.hat, method = accuracy)
{
  # This function perform a grid search to find the best cutoff 
  # value in converting continuous predicted label into discrete
  # Note that if method is "accuracy", extreme must be max, 
  # If method is "meanError", extreme must be min. 
  if (identical(method, accuracy)) 
  {
    extreme = max
  } else if (identical(method, meanError))
  {
    extreme = min
  } else
  {
    warning("Method must be one of: accuracy, meanError")
  }
  thres = seq(0.01, 0.99, by = 0.01)
  s = lapply(thres, 
             function(x) method(label, cutOff(label.hat, x)))
  s = unlist(s)
  thres[s == extreme(s)][1]
}

CalculateTPR <- function(thresh, preds, truth) {
  as.numeric(sum(preds[truth] > thresh) / sum(truth))
}

CalculateFPR <- function(thresh, preds, truth) {
  as.numeric(sum(preds[!truth] > thresh) / sum(!truth))
}

# Don't use this. It is too slow. O(n^2).
# It is here for demonstration purpose
auc2 <- function(truth, preds)
{
  positive.classifications <- sapply(preds[!truth],
  FUN = function(threshold) { CalculateFPR(threshold, preds, !truth) })
  sum(positive.classifications) / sum(!truth)
}

# Improved version of auc2. O(nlog(n))
auc3 <- function(truth, preds)
{
  r = truth[order(preds)]
  n.truth = sum(r); n = length(r)
  sum(n.truth - cumsum(r)[!as.logical(r)])/n.truth/(length(r) - n.truth)
}

# Benchmark result function
benchmark.auc = function()
{
  # How to call: benchmark.auc()
  truth = rbinom(1e4, 1 ,0.5)
  preds = runif(1e4, -2, 2)
  print(system.time(print(pROC::auc(truth, preds))))
  print(system.time(print(glmnet::auc(truth, preds))))
  print(system.time(print(auc2(truth, preds))))
  print(system.time(print(auc3(truth, preds)))) 
  # Area under the curve: 0.5018
  # user  system elapsed 
  # 0.679   0.168   0.856 
  # [1] 0.4981974
  # user  system elapsed 
  # 0.003   0.000   0.004 
  # [1] 0.4981974
  # user  system elapsed 
  # 2.425   0.385   2.817 
  # [1] 0.4981974
  # user  system elapsed 
  # 0.003   0.001   0.003 
}

# This function rescale auc to have the same meaning like correlation
aucs = function(truth, preds) 2*glmnet::auc(truth, preds)-1

plot.missclassified = function(img.id = 1, preds = yhat, .train = train,
                               .test = test, k = 3)
{
  # Args: 
  #   img.id is the id of image, e.g. 1, 2, or 3
  #   K is the number of partition of an image along each x and y axis
  # Example:
  # data = rbind(train, test)
  # label.hat2 = predict(logreg.fit, data, type = "response")
  # yhat = cutOff(label.hat2)
  # plot.missclassified()
  data = cbind(rbind(.train, .test), preds)
  blockids = seq((img.id - 1)*k^2 + 1, img.id*k^2)
  plot1 = ggplot() + 
    geom_point(data = data[data$blockid %in% blockids, ], 
               aes(x = x, y = y, 
                   color = factor(label + preds))) + 
    scale_color_discrete(guide = guide_legend(
                                      title = NULL, 
                                      direction = "horizontal",
                                      label.position = "bottom",
                                      label.hjust = 0.5, 
                                      label.vjust = 0.5,
                                      label.theme = element_text(angle = 90)), 
                         label = c("True Not-Cloud","Type I, II Error",
                                   "True Cloud")) +
    ggtitle(paste("Classification Error for Image",img.id)) + 
    theme_bw() +
    theme(
       plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,axis.ticks = element_blank()
      ,axis.text.x = element_blank()
      ,axis.text.y = element_blank()
    ) + 
    xlab("") + ylab("")

  for (blockid in unique(.train$blockid))
  {
    if (blockid %in% blockids)
    {
      border = getBorder(blockid)
      plot1 = plot1 + geom_path(data = border, aes(x = x, y = y ), color = "black")    
    }
  }
  plot1
}

getBorder = function(blockid = 2, k =3, list.images = list(image1, image2, image3))
{
  # Given a blockid, this function return the four points of the rectangular
  # surrounding that block of image. 
  blockid = blockid - 1
  img.id = floor((blockid)/(k^2))
  xy = blockid - img.id*k^2
  block.x = xy %% k
  block.y = floor(xy/k)
  img = list.images[[img.id + 1]]
  xmin = min(img$x); xmax = max(img$x); xrange = (xmax - xmin)/k
  ymin = min(img$y); ymax = max(img$y); yrange = (ymax - ymin)/k
  x1 = xmin + block.x*xrange
  x2 = xmin + (block.x + 1)*xrange
  y1 = ymin + block.y*yrange
  y2 = ymin + (block.y + 1)*yrange
  return(data.frame(x = c(x1, x1, x2, x2, x1), y = c(y1, y2, y2, y1, y1)))
}

logitFun = function(p)
{
  log(p/(1-p))
}
plotLogoddTruthPreds = function(truth, preds, m = 100, .xlab = NULL)
{
  # Args:
  #   m is the number of points to evaluate the plot at
  if (length(truth) != length(preds))
  {
    warning("Length of vector truth and probs must be equal")
  }

  ordered.truth = truth[order(preds)]
  ordered.preds = preds[order(preds)]
  n = length(truth)
  interval.size = floor(n/m)
  idx = seq(1, n, by = interval.size)
  n.idx = length(idx)
  # Num of positive in each interval divided by interval length
  prob.truth = diff(cumsum(ordered.truth)[idx])/interval.size
  logodd.truth = logitFun(prob.truth)
  logodd.preds = diff(cumsum(ordered.preds)[idx])/interval.size
  plot(logodd.preds, logodd.truth, xlab = .xlab)
}

misclassfication.matrix = function(label.hat, test.data, k = 4)
{
  # This functions defines k intervals for the range of values in the feature columns of test dataframe.
  # Once the intervals are defined, the misclassfication (1-accuracy) is calculated for all the ranges
  # of the individual features and a final matrix is returned.
  col.names <- c("NDAI","SD","CORR","DF","CF","BF","AF","AN")
  test.data <- cbind(test.data, label.hat)
  matrix.final <- matrix(nrow = k, ncol = 8)
  for (i in c(1:length(col.names)))
  {   
    l = c() 
    groups <- as.numeric(cut(test[,col.names[i]],k))
    for(p in c(1:k))
    {   
      l = c(l, 1 - accuracy(label.hat[groups == p], test.data[groups == p, "label"])) #Change train to train.data
    }   
    matrix.final[,i] <- l
  }   
  colnames(matrix.final) <- c("NDAI","SD","CORR","DF","CF","BF","AF","AN")
  rownames(matrix.final) <- seq(1:k)
  return(matrix.final)
}