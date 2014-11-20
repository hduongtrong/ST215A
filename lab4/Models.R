library(nnet)
library(MASS)
library(glmnet)
library(e1071)
library(knitr)

setwd("~/Dropbox/School/ST215/lab4p/")

source("DataProcessing.R")
source("PerformanceMetrics.R")
l = getTrainTestBlock(list(image1, image2, image3), k = 3, 
                      train.pct = 15/27, fix.random = TRUE, 
                      standardize = TRUE)
train = l[[1]]; test = l[[2]]; rm(l);

########################################################################
### 1. Linear Regression
########################################################################
linreg.fit = lm(label ~ NDAI + SD + CORR + DF + CF + 
                        BF   + AF + AN, 
                data = train)

label.hat = predict.lm(linreg.fit, test)

### Measuring Linear Regression Performance
auc(test$label, label.hat)
accuracy(cutOff(label.hat, 0.5), test$label)
# meanError(cutOff(label.hat), test$label), used for case of 3 classes
### Picking the best cut off threshold
cutOffGridSearch(test$label, label.hat, method = accuracy)

#######################################################################
### 2. Logistic Regression
#######################################################################

logreg.fit = glm(label ~ NDAI + SD + CORR + DF + CF +
                              BF + AF + AN,
                      data = train, family = binomial(link = "logit"))

label.hat2 = predict(logreg.fit, test, type = "response")
auc(test$label, label.hat2)
accuracy(cutOff(label.hat2), test$label)

### 2.2. Polynomial Logistic

logpol.fit = glm(label ~ (NDAI + SD + CORR + DF + CF +
                            BF   + AF + AN )^2,
                 data = train, family = binomial(link = "logit"))

label.hat22 = as.numeric(predict(logpol.fit, test, type = "response"))
auc(test$label, label.hat22)
accuracy(cutOff(label.hat22), test$label)


#######################################################################
### 3. Ordered Logit. For 3 classes only
#######################################################################
ordlog.fit = polr(factor(label) ~ NDAI + SD + CORR + DF + CF +
                    BF   + AF + AN + logSD, data = train, method = "logistic")
label.hat3 = as.numeric(predict(ordlog.fit, test))
accuracy(label.hat3, test$label)
meanError(label.hat3, test$label)

#######################################################################
### 4. GLMNET
#######################################################################
ld = c(1,0.5,0.2,0.1,0.05,0.02,0.01,0.005,0.002,0.001,0.0005,0.0002,0.0001,
       5e-5, 2e-5, 1e-5, 5e-6, 2e-6, 1e-6)
glmnet.fit = glmnet(as.matrix(train[,4:11]), as.numeric(train[,3]), 
                    family = "binomial", standardize = TRUE, 
                    intercept = TRUE, lambda = ld)
# label.hat4 is a matrix, each column is one possible yhat, for a 
# corresponding lambda - regularization parameter. 100 columns in total
label.hat4 = predict(glmnet.fit, as.matrix(test[,4:11]), type = "response")
sapply(1:length(ld), function(i) auc(test$label, label.hat4[,i]))

### 4.2. CVGLMNET
cvglm.fit2 = cv.glmnet(as.matrix(train[,4:11]), 
                      as.numeric(train[,3]), family = "binomial",
                      standardize = FALSE, intercept = FALSE,
                      type.measure = "auc",
                      foldid = ceiling(getFold(train$blockid)/3),
                      parallel = FALSE)
label.hat42 = predict(cvglm.fit2, as.matrix(test[,4:11]), type = "response")
auc(test$label, label.hat42)
accuracy(cutOff(label.hat42), test$label)

### 4.3. GLMNET Polynomial
cvglm.fit3 = cv.glmnet(model.matrix(~ (NDAI + SD + CORR + DF + CF +
                                      BF + AF + AN )^2, train), 
                       as.numeric(train[,3]), family = "binomial",
                       standardize = TRUE, intercept = FALSE,
                       type.measure = "auc",
                       foldid = ceiling(getFold(train$blockid)/3),
                       parallel = FALSE)

label.hat43 = predict(cvglm.fit3, model.matrix(~ (NDAI + SD + CORR + 
                      DF + CF + BF + AF + AN )^2, test), type = "response")
auc(test$label, label.hat43)

##########################################################################
### 5. Linear Discriminant Analysis
##########################################################################

qda.fit = lda(label ~ NDAI + SD + CORR + DF + CF +
                   BF + AF + AN,
                 data = train)

label.hat5 = (predict(qda.fit, test))
auc(test$label, label.hat5$x)
accuracy(test$label, label.hat5$class)

##########################################################################
### 6. naiveBayes
##########################################################################

naive.fit = naiveBayes(label ~ NDAI + SD + CORR + DF + CF +
                  BF   + AF + AN,
                data = train)
label.hat6 = predict(naive.fit, test, , type = "raw")
auc(test$label, label.hat6[,2])

##########################################
### 6. Neural Network
##########################################

nnet.fit = nnet(label ~ NDAI + SD + CORR + DF + CF + 
                        BF + AF + AN, 
                data = train, linout = TRUE,
                size = 10)

label.hat6 = predict(nnet.fit, test)
auc(test$label, label.hat6)
accuracy(test$label, cutOff(label.hat6))

########################################################
### 7. Random Forest
########################################################
library(randomForest)
forest.fit = randomForest(factor(label) ~ NDAI + SD + CORR + DF +
                            CF + BF + AF + AN,
                          data = train,
                          ntree = 160)

label.hat7 = predict(forest.fit, test, type = "prob")
random.forest.yhat = label.hat7[,2]
save(random.forest.yhat, file = "random.forest.yhat.RData")
auc(test$label, label.hat7[,2])

###########################################################################
### 8. SVM
###########################################################################
# Careful, might take a long time
SVM.fit = svm(label ~ NDAI + SD + CORR + DF + CF + 
              BF   + AF + AN, 
            data = train,
            kernel = "radial",
            gamma = 0.01,
            cost = 0.01)
label.hat8 = predict(SVM.fit, test)
auc(test$label, label.hat8)

# Save the running time
a = c("Linear", "Logistic", "PolyLogit", "QDA", "naiveBayes", "neuralNet", "randomForest")
b = c(0.140, 1.323, 5.299, 0.583, 1.697, 19.995, 42.920)
model.runtime = data.frame(model = a, runtime = b)
save(model.runtime, file = "model.runtime.RData")

## This is to produce the table for knitr
kable(misclassfication.matrix(cutOff(label.hat2), test), digit = 2)
kable(misclassfication.matrix(cutOff(label.hat7[,2]), test), digit = 2)
kable(cor(train[,4:11][train$label == 0,]))

ggplot() + stat_qq(aes(sample = test$NDAI, geom = "line")) + stat_qq(aes( sample = test$SD))

  |          |Linear |LogisticModel |naiveBayes |QDA       |NeuralNet |RandomForest |LogitCV   |LogitPoly |SVM       |
  |:---------|:------|:-------------|:----------|:---------|:---------|:------------|:---------|:---------|:---------|
  |model     |Linear |LogisticModel |naiveBayes |QDA       |NeuralNet |RandomForest |LogitCV   |LogitPoly |SVM       |
  |mean(AUC) |0.9443203   |0.9381858     |0.9396165  |0.9555549 |0.9557725 |0.9589517    |0.9360831 |0.9584566 |0.9441587 |
