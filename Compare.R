Compare<-function(r1,r2,strech)
{
  if (is.numeric(r1)||is.numeric(r2)) {
    write('Can\'t compare Rhythms before calling done()');
  }
  
  lengthofarr<-length(r1)
  if (!strech) {
    dur1 <-sum(r1,na.rm = FALSE)
    dur2 <-sum(r2,na.rm = FALSE)
    
    if (dur1 > dur2) {
      rHi <-r1;
      rLo <-r2;
    } else {
      rHi <-r2;
      rLo <-r1;
    }
    
   
    multiplier <-sum(rLo/rHi)/lengthofarr
    
   
    rHi = rHi*multiplier
  } else {
    rHi = r1;
    rLo = r2;
  }  
  

    
pairmin<- pmin(rHi,rLo)
pairmax<-pmax(rHi,rLo)
return (1- sum((pairmax-pairmin)/pairmax)/lengthofarr);
}

