source("Classifier.R")
source("readFrom.R")
source("data.R")
trainset<-readFrom(dataurl1,"csv")
traintest<-readFrom(dataurl2,"csv")

subjectlist<-function(data , subject){
  slist<- sort(levels(data$USER))
  return(slist)
}


dmod =c();
evaluateScore<-function(datatrain,datatest,evalusubject)
{
  for(v in evalusubject){
    YTrain<-as.matrix(subset(datatrain,subset=(USER==v),select=-c(USER)));
    YTest<-as.matrix(subset(datatest,select=-c(USER)));
    s1<-  mahalanobisScore(mahalanobisTrain(YTrain),YTest)
    s2<-  manhattanScore(manhattanTrain(YTrain),YTest)
    s3<-  KMeansScore(KMeansTrain(YTrain),YTest)
    s4<-  OutlierCountScore(OutlierCountTrain(YTrain),YTest)
    s5<-  SVMScore(SVMTrain(YTrain),YTest)
    s6<-  KNNScore(KNNTrain(YTrain),YTest)
    s7<-  ScaledManhattanScore(ScaledManhattanTrain(YTrain),YTest)
    s8<-  MahalanobisKNNScore(MahalanobisKNNTrain(YTrain),YTest)
    s9<- euclideanScore(euclideanTrain(YTrain),YTest)
    addscore<-data.frame(subject=v,s1=s1,s2=s2,s3=s3,s4=s4,s5=s5,s6=s6,s7=s7,s8=s8,s9=s9)
    dmod<-rbind(dmod,addscore)
  }
  
  return(dmod)
}
 # dividing by factore which is nearer to genuin user
esub<-subjectlist(trainset);
scorelist<-evaluateScore(trainset,traintest,esub)
scorelist$s9<-scorelist$s9/50000
scorelist$s1<-scorelist$s1/8.1
scorelist$s2<-scorelist$s2/1000
scorelist$s3<-scorelist$s3/5
scorelist$s4<-scorelist$s4/2.6
scorelist$s5<-scorelist$s5/0.1
scorelist$s6<-scorelist$s6/30
scorelist$s7<-scorelist$s7/35
scorelist$s8<-scorelist$s8/10
scorelist$sum<-scorelist$s1 +scorelist$s2+scorelist$s3+scorelist$s4 +scorelist$s5+scorelist$s6+scorelist$s7 +scorelist$s8+scorelist$s9;

getnamee<-function(scorelist){
  nam<-scorelist$subject[scorelist$sum==min(scorelist$sum)]
  return(nam)}

getnamee(scorelist)

