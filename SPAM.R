#http://thinktostart.com/build-a-spam-filter-with-r/

install.packages("caret")
require(caret)
install.packages("kernlab")
require(kernlab)
install.packages("doMC")
require(doMC)

dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- dataset[sample(nrow(dataset), 1000),]
trainIndex <- createDataPartition(sample$y, p = .8, list = FALSE, times = 1)
dataTrain <- sample[ trainIndex,]
dataTest  <- sample[-trainIndex,]
registerDoMC(cores=5)

### finding optimal value of a tuning parameter
sigDist <- sigest(y ~ ., data = dataTrain, frac = 1)
### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
svmTuneGrid <- data.frame(.sigma = sigDist[1], .C = 2^(-2:7))

x <- train(y ~ .,
           data = dataTrain,
           method = "svmRadial",
           preProc = c("center", "scale"),
           tuneGrid = svmTuneGrid,
           trControl = trainControl(method = "repeatedcv", repeats = 5, 
                                    classProbs =  TRUE))
pred <- predict(x,dataTest[,1:57])

acc <- confusionMatrix(pred,dataTest$y)


library(bgmm)
X_s = as.matrix(dataTrain[,20:57])
knowns_s = as.matrix(dataTest[,20:57])
class_s = (dataTest[,58])
rownames(X_s) <- NULL
rownames(knowns_s) <- NULL
modelSemiSupervised = semisupervised(X = X_s, knowns = knowns_s,   class = class_s)
modelSupervised = supervised( knowns = knowns_s,   class = class_s)
plot(modelSupervised)


#EM
library(EMCluster)
emobj <- simple.init(knowns_s, nclass = 2)
emobj <- shortemcluster(knowns_s, emobj)
summary(emobj)
ret <- emcluster(knowns_s, emobj, assign.class = TRUE)
summary(ret)

confusion(ret$class,as.integer(dataTrain[,58]))
