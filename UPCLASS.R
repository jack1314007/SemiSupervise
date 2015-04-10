#http://thinktostart.com/build-a-spam-filter-with-r/

install.packages("caret")
require(caret)
install.packages("kernlab")
require(kernlab)
install.packages("doMC")
require(doMC)
install.packages("upclass")
require(upclass)

# generate training set and test set
dataset <- read.csv("/Users/zsp/Code/SemiSupervise/data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("/Users/zsp/Code/SemiSupervise/data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- dataset[sample(nrow(dataset), 1000),]
trainIndex <- createDataPartition(sample$y, p = .2, list = FALSE, times = 1)
dataTrain <- sample[ trainIndex,]
dataTest  <- sample[-trainIndex,]
registerDoMC(cores=5)

Xtrain <- as.matrix(dataTrain[,-58])
cltrain <- as.matrix(dataTrain[,58])
Xtest <- as.matrix(dataTest[,-58])
cltest <- as.matrix(dataTest[,58])
fitup <- upclassify(Xtrain, cltrain, Xtest, cltest)
fitup$Best$modelName
summary(fitup)
fitup$Best$test$cl
library(mda)
temp <- as.integer(cltest) + 1
confusion(fitup$Best$test$cl,temp)
