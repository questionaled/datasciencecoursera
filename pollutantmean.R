pollutantmean<-function(directory,pollutant,id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  #data_c<-vector(mode = "numeric",length=332)
  data_p<-NULL
  for(i in id){
    if(i<=9){
      dataname<-paste(directory,"/00",i,".csv",sep="")
      data_p<-rbind(data_p,read.csv(dataname))
    }
    else if(i<=99){
      dataname<-paste(directory,"/0",i,".csv",sep="")
      data_p<-rbind(data_p,read.csv(dataname))
    }
    else{
      dataname<-paste(directory,"/",i,".csv",sep="")
      data_p<-rbind(data_p,read.csv(dataname))
    }
  }
  if (pollutant == 'sulfate') {
    mean(data_p$sulfate, na.rm = TRUE)
  } else if (pollutant == 'nitrate') {
    mean(data_p$nitrate, na.rm = TRUE)
  }
  
}
