## Getting and cleaning data according to classifire

gettingAndCleaning<-function(X)
{
  Train <- as.matrix( subset( X, subset = TRUE,select =-c(USER)) );
  return(Train)
}
