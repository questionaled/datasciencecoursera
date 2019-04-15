corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  nobs<-vector(mode = "numeric",length=332)
  cors<-vector(mode = "numeric",length=332)
  for(i in 1:332){
    if(i<=9){
      dataname<-paste(directory,"/00",i,".csv",sep="")
      data_p<-read.csv(dataname)
      nobs[i]<-nrow(na.omit(data_p))
      data_p<-na.omit(data_p)
      x_sulfate<-data_p$sulfate
      y_nitrate<-data_p$nitrate
      cors[i]<-cor(x_sulfate,y_nitrate)
    }
    else if(i<=99){
      dataname<-paste(directory,"/0",i,".csv",sep="")
      data_p<-read.csv(dataname)
      nobs[i]<-nrow(na.omit(data_p))
      data_p<-na.omit(data_p)
      x_sulfate<-data_p$sulfate
      y_nitrate<-data_p$nitrate
      cors[i]<-cor(x_sulfate,y_nitrate)
    }
    else{
      dataname<-paste(directory,"/",i,".csv",sep="")
      data_p<-read.csv(dataname)
      nobs[i]<-nrow(na.omit(data_p))
      data_p<-na.omit(data_p)
      x_sulfate<-data_p$sulfate
      y_nitrate<-data_p$nitrate
      cors[i]<-cor(x_sulfate,y_nitrate)
    }
  }
  corr<-NA
  for(i in 1:332){
    if(nobs[i]>threshold){
      corr<-cbind(corr,cors[i])
    }
  } 
  corr<-corr[2:length(corr)]
  corr
}
