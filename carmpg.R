mpg_data <- read.table("auto-mpg.data-original",sep=""
                       ,header=F
                       ,col.names = c('mpg','cylinders','displacement','hp','wt','acceleration','modelyear','origin','carname'))
mpg_data[,2] <- as.factor(mpg_data[,2])
mpg_data[,7] <- as.factor(mpg_data[,7])
mpg_data[,8] <- as.factor(mpg_data[,8])
summary(mpg_data)
#mpg NAs 8, V4 hp 6

library("ggplot2")
library("car")
library("dplyr")
library("DMwR2")

disp_feq = table(mpg_data$displacement)
barplot(disp_feq,main='Displacement - Frequency')
ggplot(mpg_data,aes(x=displacement)) + geom_bar()
+ggtitle("Displacement - Frequency")

ggplot(mpg_data,aes(y=wt)) + geom_boxplot()
+ggtitle("Weight - Distribution")

ggplot(mpg_data, aes(x=factor(0), y=wt)) + geom_boxplot()
+ggtitle("Weight - Distribution")
+ ylab("Weight") +  xlab("") + theme(axis.text.x=element_blank())

ggplot(mpg_data, aes(x=origin, y=mpg)) + geom_boxplot()
+ geom_rug() 
+ ggtitle("Origin and mpg Distribution")
+ ylab("Origin") +  xlab("") 

ggplot(mpg_data, aes(x=origin, y=mpg)) + geom_violin() +ggtitle('Origin-Mpg Violin plot')+geom_jitter()

library('dplyr')

data2graph <- filter(mpg_data,!is.na(mpg_data$mpg)) %>%
+mutate(new_wt=cut(data2graph$wt, quantile(data2graph$wt,c(0,0.25,.5,.75,1)), include.lowest=TRUE)) 

data2graph$new_wt <- cut(data2graph$wt, quantile(data2graph$wt,c(0,0.25,.5,.75,1)), include.lowest=TRUE)

ggplot(data2graph,aes(x=mpg,y=origin, color=origin)) + geom_point()
+ facet_wrap(~ new_wt) + + guides(color=FALSE)

install.packages('DMwR')
library('DMwR')
nrow(mpg_data)
manyNAs(mpg_data,nORp = 0.1)
clean_mpg_data <- centralImputation(mpg_data)
summary(clean_mpg_data)
 
summary(mpg_data)
cor(mpg_data[, c(1,3:6)], use = "complete.obs")  # First approach
symnum(cor(mpg_data[, c(1,3:6)],use="complete.obs"))

filter(mpg_data, !complete.cases(mpg_data))
unclean_mpg_data <- mpg_data[manyNAs(mpg_data,nORp = 0.1), ]

clean_mpg_data <- mpg_data[-manyNAs(mpg_data,nORp = 0.1), ] # remove rows with NAs where 20% variables are missing
lm(hp ~ displacement, data = clean_mpg_data) # fitting a linear model to these variables
fillhp <- function(oP) ifelse(is.na(oP),NA,40.31 + 0.33 * oP)
mpg_data[is.na(mpg_data$hp), "hp"] <- sapply(mpg_data[is.na(mpg_data$hp), "displacement"], fillhp)

#Q3.6
mpg_data <- read.table("auto-mpg.data-original",sep=""
                       ,header=F
                       ,col.names = c('mpg','cylinders','displacement','hp','wt','acceleration','modelyear','origin','carname'))
mpg_data <- knnImputation(mpg_data, k = 5, meth = "median")

#Q4
clean_mpg_data <- mpg_data[,1:8]

lm.mpg_wt <- lm(mpg ~ wt, data = clean_mpg_data)
summary(lm.mpg_wt)
par(mfrow=c(2,2))
plot(lm.mpg_wt)
plot(clean_mpg_data$wt, clean_mpg_data$mpg,main='Correlation',clab='Car Wt',ylab='miles Per Gallon')

lm.mpg_all <- lm(mpg ~ ., data = clean_mpg_data)
summary(lm.mpg_all)
par(mfrow=c(2,2))
plot(lm.mpg_all)
anova(lm.mpg_all) #acceleration low sse

lm2.mpg_all <- update(lm.mpg_all, . ~ . - acceleration)
summary(lm2.mpg_all)
anova(lm.mpg_all,lm2.mpg_all) # compare the two linear models that we have created
final.lm <- step(lm.mpg_all) # optimize lm.mpg using AIC ( model search)
summary(final.lm) 

require(rpartXse) #book package
require(rpart.plot)

(final.tree <- rpartXse(mpg ~ ., data = clean_mpg_data))
prp(final.tree,extra=101,box.col="orange",split.box.col="grey")


lm.predictions.mpg <- predict(final.lm, clean_mpg_data) # prediction
#rt.a1: decision tree that we trained earlier -- it can be found in models.R
rt.predictions.mpg <- predict(final.tree, clean_mpg_data) # prediction
(mae.mpg.lm <- mean(abs(lm.predictions.mpg - mpg_data[["mpg"]]))) # mean absolute error (MAE)
(mae.mpg.rt <- mean(abs(rt.predictions.mpg - mpg_data[["mpg"]]))) # mean absolute error (MAE)
(mse.mpg.lm <- mean((lm.predictions.mpg - mpg_data[["mpg"]])^2)) # mean squared error (MSE)
(mse.mpg.rt <- mean((rt.predictions.mpg - mpg_data[["mpg"]])^2)) # mean squared error (MSE)
(nmse.mpg.lm <- mean((lm.predictions.mpg-mpg_data[['mpg']])^2)/  #Normalized MSE (NMSE)
    mean((mean(mpg_data[['mpg']])-mpg_data[['mpg']])^2))
(nmse.mpg.rt <- mean((rt.predictions.mpg-mpg_data[['mpg']])^2)/ #Normalized MSE (NMSE)
    mean((mean(mpg_data[['mpg']])-mpg_data[['mpg']])^2))
