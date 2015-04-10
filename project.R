##############################################################
# CSC 522
# Course Project
# Author: 
##############################################################
rm(list=ls(all=T))
library(spa)
#Read SMS Spam File into table
spam <- read.table("data/spambase/spambase.data",sep=",")

#sample labeled data
label.row <- sample(1:nrow(spam),50)
label.data <- spam[label.row,]

#Unlabeled data
unlabel.data <- spam[-label.row,]

#Sample from unlabeled data to train
unlabel.train.row <- sample(as.numeric(rownames(unlabel.data)),750)
unlabel.train.data <- spam[unlabel.train.row,]
unlabel.train.data[,58]<-NA

#Test Cases
testset_withTrain <- spam[-label.row,]
testset_withoutTrain <- spam[-c(label.row,unlabel.train.row),]

#Train Cases
mixdata <- rbind(label.data,unlabel.train.data)
mixdata.class <- mixdata[,58]
mixdata <- mixdata[,1:54]
train.graph <- as.matrix(daisy(mixdata))

#SPA Model
gsemi<-spa(mixdata.class,graph=train.graph,control=spa.control(gcv="aGCV"))

#Predict with testsets


train.class <- label.data[,58]
label.data <- label.data[,1:54]
n <- 1:nrow(label.data)
train.graph1 <- matrix(0,nrow=nrow(label.data),ncol=nrow(label.data))

for(i in n)
{
  for(j in n)
  {
    train.graph1[i,j] = sum(apply(rbind(label.data[i,],label.data[j,]),2,min))
  }
}
