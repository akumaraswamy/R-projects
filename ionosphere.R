install.packages("data.table")
library(data.table)
install.packages("curl")
mydata <- fread("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data")
dim(mydata)
summary(mydata)

nmissing_v1 = sum(is.null(mydata$V1)) +sum(is.na(mydata$V1))
sprintf("No of missing V1 values %s",nmissing_v1)
nmissing_v2 = sum(is.null(mydata$V2)) +sum(is.na(mydata$V2))
sprintf("No of missing V2 values %s",nmissing_v2)
nmissing_v3 = sum(is.null(mydata$V3)) +sum(is.na(mydata$V3))
sprintf("No of missing V3 values %s",nmissing_v3)
nmissing_v4 = sum(is.null(mydata$V4)) +sum(is.na(mydata$V4))
sprintf("No of missing V4 values %s",nmissing_v4)
nmissing_v5 = sum(is.null(mydata$V5)) +sum(is.na(mydata$V5))
sprintf("No of missing V5 values %s",nmissing_v5)
nmissing_v6 = sum(is.null(mydata$V6)) +sum(is.na(mydata$V6))
sprintf("No of missing V6 values %s",nmissing_v6)
nmissing_v7 = sum(is.null(mydata$V7)) +sum(is.na(mydata$V7))
sprintf("No of missing V7 values %s",nmissing_v7)
nmissing_v8 = sum(is.null(mydata$V8)) +sum(is.na(mydata$V8))
sprintf("No of missing V8 values %s",nmissing_v8)
nmissing_v9 = sum(is.null(mydata$V9)) +sum(is.na(mydata$V9))
sprintf("No of missing V9 values %s",nmissing_v9)
nmissing_v10 = sum(is.null(mydata$V10)) +sum(is.na(mydata$V10))
sprintf("No of missing V10 values %s",nmissing_v10)
nmissing_v11 = sum(is.null(mydata$V11)) +sum(is.na(mydata$V11))
sprintf("No of missing V11 values %s",nmissing_v11)


v1counts <- table(mydata$V1)
barplot(v1counts)
v2counts <- table(mydata$V2)
barplot(v2counts)
v35counts <- table(mydata$V35)
barplot(v35counts)

install.packages("ggplot2")
library(e1071); library(ggplot2)
qplot(mydata$V22, mydata$V20, colour = mydata$V35, shape = mydata$V35, 
      data = mydata)
qplot(mydata$V1, mydata$V2, colour = mydata$V35, shape = mydata$V35, 
      data = mydata)
c

k_max <- 10
tsse <- sapply(1:k_max,
               function(k){kmeans(kmeans_data, k, nstart=30,iter.max = 12 )$tot.withinss})

plot(1:k_max, tsse, type="b", pch = 20, frame = FALSE, xlab="Number of clusters k",  ylab="Total within-clusters sum of squares")

pca_iono_data <- mydata[,-35]
pca_iono <- prcomp(pca_iono_data)
summary(pca_iono)
pca_iono$x[1:10,]
plot(pca_iono, type = "l")
screeplot(pca_iono)
pairs(pca_iono$x[,1:2])
pca_iono$rotation 




