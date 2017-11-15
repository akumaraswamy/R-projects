library('DMwR')
library('dplyr')
library('ggplot2')
library("car")
library("corrplot")
library('data.table')
library('Metrics')

set.seed(1234)
train_data <- read.csv("train.csv",header= TRUE)
nrow(train_data)
str(train_data)
test_data <- read.csv("test.csv",header= TRUE)
nrow(test_data)
str(test_data)
#Check the data for NAs
miss_value_train <- sapply(train_data, function(x) sum(is.na(x)))
miss_value_test <- sapply(test_data, function(x) sum(is.na(x)))
sprintf('Num missing values: Training data - %d',sum(miss_value_train))
sprintf('Num missing values: Test data - %d',sum(miss_value_test))

### Check rows where NAs exceed the default 20% columns
# NAs are within the threshold, so rows are not removed after this step
manyNAs(train_data,nORp = 0.2)
manyNAs(test_data,nORp = 0.2)


#### Exploration #####
num_var <- names(train_data)[which(sapply(train_data, is.numeric))]
cat_var <- names(train_data)[which(sapply(train_data, is.factor))]

train_cat <- setDT(train_data)[,.SD, .SDcols = cat_var]
train_num <- setDT(train_data)[,.SD, .SDcols = num_var]

# Neighborhood - SalePrice relation
ggplot(train_data, aes(x=train_data$Neighborhood, y=train_data$SalePrice))+
  geom_boxplot(notch=FALSE, outlier.shape=NA, fill="red", alpha=0.1)+
  xlab('Neighborhood')+ylab('Sale Price')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle('Neighborhood - SalePrice BoxPlot')+
  ggsave('neigh_saleprice.png')


####Display distribution of category variable
plotHistFunc <- function(x, na.rm = TRUE, ...) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    plot <-ggplot(x,aes_string(x = nm[i]))+
      geom_histogram(stat = 'count',alpha = .5,fill = "dodgerblue")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    ggsave(plot,filename=paste("cat/",nm[i],".png",sep=""))
  }
  
}
plotHistFunc(train_cat)

##### Display distribution of continuous variable
plotDensityFunc <- function(x, na.rm = TRUE, ...) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    plot <-ggplot(data=train_num) +
      geom_line(aes(x=train_num[[nm[i]]]),alpha = .5,stat="density")+ 
      xlab(nm[i])+
      ggtitle(paste('Density - ',nm[i],sep=""))
    ggsave(plot,filename=paste("num/",nm[i],".png",sep=""))
  }
  
}

plotDensityFunc(train_num)

####Box Plot
plotBoxPlotFunc <- function(x, na.rm = TRUE, ...) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    plot <-ggplot(data=train_num,aes(x=train_num[[nm[i]]],y=train_num$SalePrice))+
      geom_boxplot(notch=FALSE, outlier.shape='circle', fill="red", alpha=0.1)+ 
      xlab(nm[i])+ylab('SalePrice')
      ggtitle(paste('BoxPlot - ',nm[i],sep="")) 
    ggsave(plot,filename=paste("box/",nm[i],".png",sep=""))
  }
  
}
plotBoxPlotFunc(train_num)


correlations <- cor(na.omit(train_num[,-1, with = FALSE]))
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]

png('corrplot.png')
corrplot(correlations, method="circle")
dev.off()

which(correlations[, "SalePrice"] > 0)
which(correlations[, "SalePrice"] < 0)

house_price <- train_data[,1:80]
SalePrice <- train_data[,81]
house_price <- rbind(house_price,test_data)


# categorize the SF variable into bins
house_price$Exterior1st = NULL
house_price$Exterior2nd = NULL

house_price <- house_price %>%
  mutate(YearBuilt = case_when(YearBuilt < YearRemodAdd ~ YearRemodAdd)) 



house_price <- house_price %>%
  mutate(YearBuilt = case_when(YearBuilt > 1800 & YearBuilt <= 1900 ~ '1800-1900',
                               YearBuilt > 1900 & YearBuilt <= 1950 ~ '1901-1950',
                               YearBuilt > 1950 & YearBuilt <= 1980 ~ '1950-1980',
                               YearBuilt > 1980 & YearBuilt <= 1990 ~ '1980-1990',
                               YearBuilt > 1990 & YearBuilt <= 2000 ~ '1990-2000',
                               YearBuilt > 2000 & YearBuilt <= 2010 ~ '2000-2010',
                               YearBuilt < 2017  ~ '2017'))

house_price$YearRemodAdd = NULL

house_price <- house_price %>%
  mutate(GrLivArea = case_when(GrLivArea <=1126 ~ 'S', 
                               GrLivArea >1126 & GrLivArea <=1502 ~ 'M',  
                               GrLivArea >1502 ~ 'L'))

house_price <- house_price %>%
  mutate(TotalBsmtSF = case_when(TotalBsmtSF <=793 ~ 'S', 
                                 TotalBsmtSF >793 & TotalBsmtSF <=1502 ~ 'M',  
                                 TotalBsmtSF >1502 ~ 'L'))

house_price$TotalBsmtSF <- as.factor(house_price$TotalBsmtSF)
house_price$GrLivArea <- as.factor(house_price$GrLivArea)
house_price$YearBuilt <- as.factor(house_price$YearBuilt)


#Using central imputation to fix NAs
clean_house_price <- centralImputation(house_price)
summary(clean_house_price)

#### One Hot Encoding
#library('dummies')
#clean_house_price_ohe <- dummy.data.frame(clean_house_price, names=cat_var, sep="_")


#split the cleaned to test and train. Join the classifier to training
cleaned_train <- clean_house_price[1:1460,]
cleaned_train <- cbind(cleaned_train,SalePrice)
cleaned_test <- clean_house_price[1461:2919,]

feature_count = ncol(cleaned_train)
cleaned_valid_Y <- cleaned_train[731:1460,78]
cleaned_valid_X <- cleaned_train[731:1460,1:77]
cleaned_train <- cleaned_train[1:730,1:78]

## Print model error
print_model_metrics <- function(model_name,predicted_value,actual_value){
  error <- abs(predicted_value - actual_value)
  mae.hp <- mean(error) 
  mse.hp <- mean((error)^2) 
  rmse.hp <- sqrt(mse.hp)
  rmsle.hp <- rmse(log(predicted_value),log(actual_value))
  show(model_name);
  show('**********************************')
  show(paste('Mean absolute error ',mae.hp));
  show(paste('Mean Square error ',mse.hp));
  show(paste('Root Mean square error ',rmse.hp));
  show(paste('Root Mean square log error ',rmsle.hp));
}


########################################################
#### Linear regression model
#######################################################
set.seed(1234)
hp_all_lm <- lm(SalePrice ~ ., data = cleaned_train)
summary(hp_all_lm)
lm.predictions.hp <- predict(hp_all_lm, cleaned_valid_X) # prediction
print_model_metrics('Linear Regression - Training',lm.predictions.hp,cleaned_valid_Y)
png('linear_model.png')
par(mfrow=c(2,2))
plot(hp_all_lm)
dev.off()

######################################################
#Predict Linear regression high correlation coefficient
#####################################################
hp_lm <- lm(SalePrice ~ LotFrontage+LotArea+OverallQual+YearBuilt+
              BsmtFullBath+FullBath+HalfBath+
              MasVnrArea+BsmtFinSF1+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+GrLivArea+
            FullBath+BedroomAbvGr+TotRmsAbvGrd+Fireplaces+
            GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF, data = cleaned_train)

summary(hp_lm)
lm.predictions.hp <- predict(hp_lm, cleaned_valid_X) # prediction
print_model_metrics('Linear regression High Coeff features',lm.predictions.hp,cleaned_valid_Y)
png('lm_selfeature.png')
par(mfrow=c(2,2))
plot(hp_lm)
dev.off()

################################################################
# Training Regression tree
###############################################################
require(rpartXse) #book package
require(rpart.plot)

(final.tree <- rpartXse(SalePrice ~ ., data = cleaned_train,se = 0.5))
# Regression tree - Validation
tree.predictions.hp <- predict(final.tree, cleaned_valid_X) # prediction
print_model_metrics('Regression Tree',tree.predictions.hp,cleaned_valid_Y)

png('regression_tree.png')
prp(final.tree,extra=101,box.col="orange",split.box.col="grey") 
dev.off()

#############################################################
# Gradient boosting machine
#############################################################
library(gbm)
#split the cleaned to test and train. Join the classifier to training
set.seed(1234)
m_gbm <- gbm(SalePrice ~., data=cleaned_train,n.trees=500, shrinkage = 0.1,
             interaction.depth = 15,
             cv.folds = 10, distribution = "gaussian")
png('gbm_perf.png')
(best <- gbm.perf(m_gbm, plot.it=TRUE, method="cv"))  
dev.off()
ps_gbm <- predict(m_gbm, cleaned_valid_X, n.trees=best, type="response")
print_model_metrics('Gradient Boosting Machine',ps_gbm,cleaned_valid_Y)

###############################################################
# Random Forest
###############################################################
library(randomForest)

rf_model <- randomForest(SalePrice ~., cleaned_train, ntree=500)
rf.predictions.hp <- predict(rf_model,cleaned_valid_X)
print_model_metrics('Random Forest',rf.predictions.hp,cleaned_valid_Y)

#######################################################################
#Neural network
#######################################################################
library(h2o)
h2oInstance <- h2o.init(ip="localhost")
trH  <- as.h2o(cleaned_train,"trH")
tsH <- as.h2o(cleaned_test,"tsH")
mdl <- h2o.deeplearning(x=1:77, y=78, training_frame=trH,
          nfolds = 5,score_each_iteration = TRUE,
          distribution = 'gaussian',hidden = c(200,200,200),
          input_dropout_ratio = 0.2,
          l1 = 1e-5, epochs = 10 )
mdl


####################################################
# Predict testing data using LM and RF
####################################################
lm.test.predictions <- predict(hp_lm,cleaned_test)
lm.test.results <- data.frame(Id=cleaned_test$Id,SalePrice=lm.test.predictions)
write.csv(lm.test.results, file='house_price_lm_submission.csv',row.names = F)

rf.test.predictions <- predict(rf_model,cleaned_test)
rf.test.results <- data.frame(Id=cleaned_test$Id,SalePrice=rf.test.predictions)
write.csv(rf.test.results, file='house_price_rf_submission.csv',row.names = F)

ps_gbm <- predict(m_gbm, cleaned_test, n.trees=best, type="response")
gbm.test.results <- data.frame(Id=cleaned_test$Id,SalePrice=ps_gbm)
write.csv(gbm.test.results, file='house_price_gbm_submission.csv',row.names = F)

preds <- h2o.predict(mdl,tsH)[,"predict"]
pred_table <- data.table(as.vector(preds))
nn.test.results <- data.frame(Id=cleaned_test$Id,SalePrice=pred_table$V1)
write.csv(nn.test.results, file='house_price_nn_submission.csv',row.names = F)
