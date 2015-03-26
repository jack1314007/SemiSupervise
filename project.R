##############################################################
# CSC 522
# Course Project
# Author: 
##############################################################
rm(list=ls(all=T))
library(RTextTools)
#Read SMS Spam File into table
sms.dir <- "./data/smsspamcollection//SMSSpamCollection"
smsFile <- file(sms.dir,open="rt",encoding="latin1")
text <- readLines(smsFile)
close(smsFile)