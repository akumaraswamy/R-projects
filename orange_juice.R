install.packages('ISLR')
library(ISLR)
mydata <- OJ
dim(mydata)
names(mydata)
summary(mydata)
View(mydata)
sapply(mydata, function(x) sum(is.na(x)))


install.packages("rpart.plot")
library(rpart.plot)
par( mfrow=c(1,2) )

set.seed(1234)  
rndSample <- sample(1:nrow(mydata),900)
mydata.training <- mydata[rndSample,]
mydata.testing <- mydata[-rndSample,]

#### Decision Tree
ct0 <- rpartXse(Purchase ~ ., mydata.training, se=0)
ps_ct0 <- predict(ct0, mydata.testing, type="class")
(cm0 <- table(ps_ct0, mydata.testing$Purchase))
100*(1-sum(diag(cm0))/sum(cm0))
prp(ct0,type=0,extra=101)

ct1 <- rpartXse(Purchase ~ ., mydata.training, se=1)
ps_ct1 <- predict(ct1, mydata.testing, type="class")
(cm1 <- table(ps_ct1, mydata.testing$Purchase))
100*(1-sum(diag(cm1))/sum(cm1))
prp(ct1,type=0,extra=101)

ct <- rpartXse(Purchase ~ ., mydata.training, se=0.5)
ps1 <- predict(ct, mydata.testing)
ps1
ps2 <- predict(ct, mydata.testing, type="class")
ps2
(cm <- table(ps2, mydata.testing$Purchase))
100*(1-sum(diag(cm))/sum(cm))
prp(ct,type=0,extra=101)

### SVM
s <- svm(Purchase ~ ., mydata.training) # train a SVM over iris data set
ps <- predict(s, mydata.testing) # classify the test data using the trained SVM
(cm_svm <- table(ps, mydata.testing$Purchase)) #confusion matrix for evaluation
100*(1-sum(diag(cm_svm))/sum(cm_svm))  

s2 <- svm(Purchase ~ ., mydata.training, cost=3, kernel="polynomial", degree=3) #training
ps2 <- predict(s2, mydata.testing)  #testing
(cm_svm2 <- table(ps2, mydata.testing$Purchase)) #confusion matrix for evaluation
100*(1-sum(diag(cm_svm2))/sum(cm_svm2))  # the error rate is 8%

### ANN
install.packages("nnet")
library(nnet)
n <- nnet(Purchase ~ ., mydata.training, size=10 ,trace=FALSE, maxit=1000) # train an ANN over iris data set
n
ps_ann <- predict(n, mydata.testing, type="class")  #  classify test data using the trained ANN
ps_ann
(cm_ann <- table(ps_ann, mydata.testing$Purchase)) # confusion matrix for evaluation
100*(1-sum(diag(cm_ann))/sum(cm_ann))

n_50 <- nnet(Purchase ~ ., mydata.training, size=50 ,trace=FALSE, maxit=1000) # train an ANN over iris data set
ps_ann_50 <- predict(n, mydata.testing, type="class")  #  classify test data using the trained ANN
(cm_ann_50 <- table(ps_ann_50, mydata.testing$Purchase)) # confusion matrix for evaluation
100*(1-sum(diag(cm_ann_50))/sum(cm_ann_50))

install.packages("ggplot2")
library(ggplot2)
install.packages("NeuralNetTools")
library(NeuralNetTools)
## Feature importance (left graph)
garson(n) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
## Network diagram (rigth graph)
plotnet(n)

garson(n_50) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
## Network diagram (rigth graph)
plotnet(n_50)

### Deep Learning
install.packages("h2o")
library(h2o)
h2oInstance <- h2o.init(ip="localhost")
trH  <- as.h2o(mydata[rndSample, ],"trH") #training data: randomly picked 100 points from iris data
tsH <- as.h2o(mydata[-rndSample, ],"tsH")
mdl <- h2o.deeplearning(x=2:18, y=1, training_frame=trH)
mdl
#classify the test data using the trained DLNN
#mdl: is the trained DLNN, tsH: test data
preds <- h2o.predict(mdl,tsH)[,"predict"]
preds

(cm <- table(as.vector(preds), as.vector(tsH$Purchase))) #confusion matrix for evaluation
100*(1-sum(diag(cm))/sum(cm)) #the error rate 

