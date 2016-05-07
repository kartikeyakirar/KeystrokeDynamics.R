source("Classifier.R")
source("readFrom.R")
source("gettingAndCleaning.R")

train<- '/home/kartikeya/Downloads/keystroke-dynamics (1).csv'
test <-  '/home/kartikeya/Downloads/ksdtest.csv'

datatrain<-readFrom(train,"csv");
datatest<-readFrom(test,"csv");

YTrain <- gettingAndCleaning(datatrain);
YTest <-  gettingAndCleaning(datatest);

euclideanScore(euclideanTrain(YTrain),YTest)
mahalanobisScore(mahalanobisTrain(YTrain),YTest)
manhattanScore(manhattanTrain(YTrain),YTest)
KMeansScore(KMeansTrain(YTrain),YTest)
OutlierCountScore(OutlierCountTrain(YTrain),YTest)
SVMScore(SVMTrain(YTrain),YTest)
KNNScore(KNNTrain(YTrain),YTest)
ScaledManhattanScore(ScaledManhattanTrain(YTrain),YTest)
MahalanobisKNNScore(MahalanobisKNNTrain(YTrain),YTest)