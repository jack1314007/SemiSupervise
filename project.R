##############################################################
# CSC 522
# Course Project
# Author: 
##############################################################
rm(list=ls(all=T))
library(RTextTools)

get.msg <- function(path.dir)
{
  con <- file(path.dir, open="rt", encoding="latin1")
  text <- readLines(con)
  msg <- text[seq(which(text=="")[1]+1,length(text),1)]
  close(con)
  return(paste(msg, collapse="\n"))
}