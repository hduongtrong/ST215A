library(nnet)
setwd("~/Dropbox/School/ST215/Lab/lab4/")
source("DataProcessing.R")
source("PerformanceMetrics.R")

########################################################################
### 1. Linear Regression
########################################################################
train$logSD = log(train$SD+1)
test$logSD  = log(test$SD +1)
linreg.fit = lm(label ~ NDAI + SD + CORR + DF + CF + 
                        BF   + AF + AN   + logSD, 
                data = train)
label.hat = predict.lm(linreg.fit, test)

### Measuring Linear Regression Performance
accuracy(cutOff(label.hat), test$label)
meanError(cutOff(label.hat), test$label)
### Picking the best cut off threshold
cutOffGridSearch(test$label, label.hat, method = accuracy)


#######################################################################
### 2. Logistic Regression
#######################################################################
logreg.fit = multinom(label ~ NDAI + SD + CORR + DF + CF +
                              BF   + AF + AN + logSD,
                      data = train)
label.hat2 = as.numeric(predict(logreg.fit, test))
accuracy(label.hat2, test$label)
