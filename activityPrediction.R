traindat <- read.csv("c:/Users/Dafh/Documents/R/Work/Machine Learning/trainDat.csv", na.strings = c("",NA))
testdat <- read.csv("c:/Users/Dafh/Documents/R/Work/Machine Learning/testDat.csv", na.strings = c("",NA))
joint <- rbind(traindat[,-160], testdat[,-160])
pre_joint <- preProcess(joint,method = c("zv","corr"))
joint1 <- predict(pre_joint,joint)
anyNA(joint1)


na_count <- sapply(joint1, function(y) sum(is.na(y)))

zeroNA <- joint1[,(na_count/nrow(joint1)) < 0.90]
anyNA(zeroNA)
trClean <- zeroNA[1:19622,]
testClean <- zeroNA[19623:19642,]
trClean$classe <- traindat$classe
pre_trclean <- preProcess(trClean, method = c("zv","corr"))
head(colnames(trClean),10)
trClean <- trClean[,-c(1:6)]
library(randomForest)
my_cluster <- makeCluster(detectCores()-1)
library(doParallel)
registerDoParallel(my_cluster)
fitControl <- trainControl(method = "cv", number = 10, allowParallel = TRUE)
x <- trClean[,-46]
y <- trClean[,46]
fit <- train(x,y,method = "rf", data=trClean,trControl = fitControl)
predict(fit,testClean)
