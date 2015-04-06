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
