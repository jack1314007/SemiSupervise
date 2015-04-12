##############################################################
# CSC 522
# Course Project
# Author: 
##############################################################
rm(list=ls(all=T))
library(spa)
library(mda)

#Read SMS Spam File into table
spam <- read.table("data/spambase/spambase.data",sep=",")

#sample 1000 from sample pool
sampleSet <- spam[sample(1:nrow(spam),1500),]

#Select the First 150 as FIXED test data
testSet <- sampleSet[1351:1500,]
#The rest data as TrainData
trainSet <- sampleSet[1:1350,]

spa.predict<-function(train,test,percentageOfUnlable)
{
  sumSet <- nrow(train)+nrow(test)
  lable_size = sumSet * (1-percentageOfUnlable/100)  
  #sample labeled data
  label.row <- sample(1:nrow(train),lable_size)
  label.data <- train[label.row,]
  
  #Unlabeled data
  unlabel.data <- train[-label.row,]
  unlabel.data <- rbind(unlabel.data,test)
  unlabel.data[,58]<-NA
  
  
  #Train Cases
  mixdata <- rbind(label.data,unlabel.data)
  mixdata.class <- mixdata[,58]
  mixdata <- mixdata[,1:30]
  train.graph <- as.matrix(daisy(mixdata))
  
  #SPA Model
  gsemi<-spa(mixdata.class,graph=train.graph,control=spa.control(gcv="aGCV"))
  
  
  res <- round(gsemi$model$fit*1.2,0)
  a <- confusion(res[1351:1500], test[,58])
  return(a)
}
error <- list()
sumError = seq(10,90,10)
for(i in seq(10,90,10))
{
  eachError <- list()
  sumError1 = 0
  for (j in 1:10 )
  {
    print(i+j)
    eachError[[j]] <- try(spa.predict(trainSet,testSet,i))
    a <- eachError[[j]]
    b = (a[1,1]+a[2,2])/(a[1,1]+a[2,1]+a[1,2]+a[2,2])
    c = 1-b
    sumError1 <- sumError1 + c
    print(eachError[[j]])
    if(class(eachError[j]) == "try-error") next;
  }
  sumError[i/10] <- sumError1/10
  error[[i]] <- eachError
}