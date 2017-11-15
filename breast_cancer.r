mydata <- fread("sum(https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data")
print("Data Summary")
summary(mydata)

sprintf ("Number of missing data %s",sum(!complete.cases(mydata)))


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
nmissing_v7 = sum(is.null(mydata$V7)) +sum(is.na(mydata$V7))+ sum(is.na(as.numeric(mydata$V7)))
sprintf("No of missing V7 values %s",nmissing_v7)
nmissing_v8 = sum(is.null(mydata$V8)) +sum(is.na(mydata$V8))
sprintf("No of missing V8 values %s",nmissing_v8)
nmissing_v9 = sum(is.null(mydata$V9)) +sum(is.na(mydata$V9))
sprintf("No of missing V9 values %s",nmissing_v9)
nmissing_v10 = sum(is.null(mydata$V10)) +sum(is.na(mydata$V10))
sprintf("No of missing V10 values %s",nmissing_v10)
nmissing_v11 = sum(is.null(mydata$V11)) +sum(is.na(mydata$V11))
sprintf("No of missing V11 values %s",nmissing_v11)

sprintf('Number of benign identifier: %s',sum(mydata$V11==2))
sprintf('Number of malignant identifier: %s',sum(mydata$V11==4))

png("images/v1.png")
hist(mydata$V1,col="blue",xlab = 'Sample code number',main='Histogram - Sample code number')
dev.off()

png("images/v2.png")
hist(mydata$V2,col="blue",xlab = 'Clump Thickness',main='Histogram - Clump Thickness')
dev.off()

png("images/v3.png")
hist(mydata$V3,col="blue",xlab = 'Uniformity of Cell Size',main='Histogram - Uniformity of Cell Size')
dev.off()

png("images/v4.png")
hist(mydata$V4,col="blue",xlab = 'Uniformity of Cell Shape',main='Histogram - Uniformity of Cell Shape')
dev.off()

png("images/v5.png")
hist(mydata$V5,col="blue",xlab = 'Marginal Adhesion',main='Histogram - Marginal Adhesion')
dev.off()

png("images/v6.png")
hist(mydata$V6,col="blue",xlab = 'Single Epithelial Cell Size',main='Histogram - Single Epithelial Cell Size')
dev.off()

png("images/v7.png")
hist(as.numeric(mydata$V7),col="blue",xlab = 'Bare Nuclei',main='Histogram - Bare Nuclei')
dev.off()

png("images/v8.png")
hist(mydata$V8,col="blue",xlab = 'Bland Chromatin',main='Histogram - Bland Chromatin')
dev.off()

png("images/v9.png")
hist(mydata$V9,col="blue",xlab = 'Normal Nucleoli',main='Histogram - Normal Nucleoli')
dev.off()

png("images/v10.png")
hist(mydata$V10,col="blue",xlab = 'Mitosis',main='Histogram - Mitosis')
dev.off()

png("images/v11.png")
hist(mydata$V11,col="blue",xlab = 'Class',main='Histogram - Class')
dev.off()

png("images/v11.png")
hist(mydata$V11,col="blue",xlab = 'Sample code number',main='Histogram - Sample code number')
dev.off()
