install.packages("randomForest")
library(randomForest)
library(DMwR2)
install.packages("gbm")
library(gbm)

set.seed(1234)

### Random Forest
rndSample <- sample(1:nrow(mydata),900)
mydata.training <- mydata[rndSample,]
mydata.testing <- mydata[-rndSample,]

m <- randomForest(Purchase ~., mydata.training, ntree=100)
 ps_rand <- predict(m,mydata.testing)
 (cm_rand <- table(ps_rand, mydata.testing$Purchase)) 
 100*(1-sum(diag(cm_rand))/sum(cm_rand))
 

 ### Gradient Boosing
 clean.data <- mydata
 clean.data[,1]<- gsub("CH","0",clean.data[,1], fixed = TRUE)
 clean.data[,1] <- gsub("MM","1",clean.data[,1], fixed = TRUE)
  
 sample <- sample(1:nrow(clean.data),900)
 tr <- clean.data[sample,]
 ts <- clean.data[-sample,]
 
 m_gbm <- gbm(Purchase ~., data=tr, distribution = "bernoulli",
              n.trees=5000,cv.folds = 5)
 
  (best <- gbm.perf(m_gbm, plot.it=FALSE, method="cv"))  
 ps <- predict(m_gbm, ts, n.trees=best, type="response")

 (cm <- table(ps, ts$Purchase))
 100*(1-sum(diag(cm))/sum(cm))
  