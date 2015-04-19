#http://thinktostart.com/build-a-spam-filter-with-r/

install.packages("caret")
require(caret)
install.packages("kernlab")
require(kernlab)
install.packages("doMC")
require(doMC)
install.packages("upclass")
require(upclass)
library(mda)

# generate training set and test set
dataset <- read.csv("/Users/zsp/Code/SemiSupervise/data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("/Users/zsp/Code/SemiSupervise/data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- dataset[sample(nrow(dataset), 1500),]
registerDoMC(cores=4)

up_cof_mean <- c()

for (i in seq(150,1350,150))
{
  up_cof <- c()
  for (j in seq(1,10))
  {
    labelIndex <- sample(1350,i)
    unlabelIndex <- union(setdiff(seq(1,1350),labelIndex),seq(1351,1500))
  
    dataTrain <- sample[ labelIndex,]
    dataTest  <- sample[ unlabelIndex,]
  
    Xtrain <- as.matrix(dataTrain[,1:30])
    cltrain <- as.matrix(dataTrain[,58])
  
    Xtest <- as.matrix(dataTest[,1:30])
    cltest <- as.matrix(dataTest[,58])
  
    fitup <- upclassify(Xtrain, cltrain, Xtest, cltest)
    cl <- fitup$Best$test$cl

    temp <- as.integer(cltest) + 1
    cof <- confusion(fitup$Best$test$cl[(nrow(cltest)-149):nrow(names)],temp[(nrow(cltest)-149):nrow(names)])
    up_cof <- c(up_cof, (cof[1,2]+cof[2,1])/(cof[1,1]+cof[1,2]+cof[2,1]+cof[2,2]))
  }
  up_cof_mean <- c(up_cof_mean, mean(up_cof))
}

plot(seq(1,9)/10, up_cof_mean, col="red", type="o", pch=16)
