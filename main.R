source("Classifier.R")
source("readFrom.R")
source("gettingAndCleaning.R")
source("data.R")
source("Compare.R")

datatrain<-readFrom(dataurl1,"csv");
datatest<-readFrom(dataurl2,"csv");

meanData <- function( YTrain ) {
  dmod <- list ( mean  = colMeans( YTrain ) );
  return( dmod );
}

YTrain <- gettingAndCleaning(datatrain);
YTest <-  gettingAndCleaning(datatest);
score<-c(
euclideanScore(euclideanTrain(YTrain),YTest),
mahalanobisScore(mahalanobisTrain(YTrain),YTest),
manhattanScore(manhattanTrain(YTrain),YTest),
KMeansScore(KMeansTrain(YTrain),YTest),
OutlierCountScore(OutlierCountTrain(YTrain),YTest),
SVMScore(SVMTrain(YTrain),YTest),
KNNScore(KNNTrain(YTrain),YTest),
ScaledManhattanScore(ScaledManhattanTrain(YTrain),YTest),
MahalanobisKNNScore(MahalanobisKNNTrain(YTrain),YTest))

mulFactore<-c(50000,8.1,1000,5,2.6,0.1,30,35,10)
scaledScore<-score/mulFactore
ytrain<-meanData(YTrain)
YT<-t(as.matrix(as.data.frame(ytrain)))
a<-YT[grep("DD", colnames(YT))]
b<-YTest[grep("DD", colnames(YTest))]
probNear<-CompareRythm(a,b,F)
perProb<-probNear*100

ifelse(perProb>80 && sum(scaledScore) <=69,print(paste0("genuin user with probability",perProb)),("imposter"))

