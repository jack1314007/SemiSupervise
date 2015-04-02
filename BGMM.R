#http://bgmm.molgen.mpg.de/rapBGMM/
library(bgmm)
data(genotypes)
modelSupervised = supervised(knowns = genotypes$knowns, class = genotypes$labels)
plot(modelSupervised)
modelSemiSupervised = semisupervised(X = genotypes$X, knowns = genotypes$knowns,   class = genotypes$labels)
plot(modelSemiSupervised)
modelBelief = belief(X = genotypes$X, knowns = genotypes$knowns,    B = genotypes$B)
plot(modelBelief)
modelSoft = soft(X = genotypes$X, knowns = genotypes$knowns,   P = genotypes$B)
plot(modelSoft)
modelUnSupervised = unsupervised(X = genotypes$X, k = 3)
plot(modelUnSupervised)

preds = predict(modelSoft, X = genotypes$X)

str(preds)