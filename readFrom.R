readFrom<-function(data,str){
  if( ! file.exists(data) )
    stop( "Password data file ",data," does not exist");
  
  if(str=="txt")
   { datafile<-read.table( data, header = TRUE );}
  else if(str=="csv")
    datafile<-read.csv( data, header = TRUE );
  
  return(datafile)
}
