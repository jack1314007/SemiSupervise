# Code for Expectation Maximization(EM)
# http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Expectation_Maximization_(EM)
library(mclust)
modelName = "EEE"
data = iris[,-5]
z = unmap(iris[,5])
msEst <- mstep(modelName, data, z)
names(msEst)
modelName = msEst$modelName
parameters = msEst$parameters
em(modelName, data, parameters)


library(EMCluster, quiet = TRUE)

x1 <- iris[,-5]
sample <- iris[sample(nrow(iris), 100),]
x1 <- sample[,-5]
rownames(x1) <- NULL
emobj <- simple.init(x1, nclass = 3)
emobj <- shortemcluster(x1, emobj)
summary(emobj)
lab <- as.integer(sample[,5])
lab[lab %in% 3] <-0
# k = 2
# nk = floor(k * 2 / 3)
# tmp <- names(sort(table(lab), decreasing = TRUE))[1:nk]
# lab[!(lab %in% tmp)] <- 0
# for(i in 1:nk){
#   tmp.id <- lab == tmp[i]
#   id <- sample(which(tmp.id), sum(tmp.id) * 0.5, replace = FALSE)
#   tmp.id[id] <- FALSE
#   lab[tmp.id] <- 0
# }
# index.lab <- sort(unique(lab))
# lab <- sapply(lab, function(i) which(index.lab == i) - 1)

ret <- emcluster(x1, emobj,assign.class = TRUE)
summary(ret)
library(mda)
confusion(ret$class, as.integer(sample[,5]))


ret <- init.EM(x1, nclass = 3)
ret.new <- assign.class(x1, ret, return.all = FALSE)
str(ret.new)

confusion(ret.new$class, as.integer(sample[,5]))

