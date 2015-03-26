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

