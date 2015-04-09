##############################################################
# CSC 522
# Course Project
# Author: 
##############################################################
rm(list=ls(all=T))
library(RTextTools)
#Read SMS Spam File into table
sms.dir <- "./data/smsspamcollection/SMSSpamCollection"
sms.data <- read.table(sms.dir,sep="\t",quote="",fill=FALSE,col.names=c("class", "sms"),)
matrix <- create_matrix(cbind(sms.data$class,sms.data$sms),)
con <- create_container(matrix, sms.data$class, trainSize=1:2500, testSize=2501:4000, virgin=FALSE)
models <- train_models(con, algorithms=c("MAXENT","SVM"))
results <- classify_models(con, models)
analytics <- create_analytics(con, results)
summary(analytics)
