source("Classifier.R")
source("readFrom.R")
source("gettingAndCleaning.R")
source("data.R")

datatrain<-readFrom(train,"csv");
datatest<-readFrom(test,"csv");

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
scaledScore<-score*mulFactore
probNear<-Compare(YTrain,YTest,FALSE)
perProb<-probNear*100

if(perProb>80 || (scaledScore <=__ ||scaledScore >= __ )
   write(TRUE)
   else
     write(FALSE)