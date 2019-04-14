complete<-function(directory,id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  nob<-vector(mode = "numeric",length=332)
  for(i in id){
    if(i<=9){
      dataname<-paste(directory,"/00",i,".csv",sep="")
      data_p<-read.csv(dataname)
      nob[i]<-nrow(na.omit(data_p))
    }
    else if(i<=99){
      dataname<-paste(directory,"/0",i,".csv",sep="")
      data_p<-read.csv(dataname)
      nob[i]<-nrow(na.omit(data_p))
    }
    else{
      dataname<-paste(directory,"/",i,".csv",sep="")
      data_p<-read.csv(dataname)
      nob[i]<-nrow(na.omit(data_p))
    }
  }
  data_j<-matrix(nrow=length(id),ncol=2)
  for(i in id){
    for(j in 1:length(id)){
      data_j[j,1]<-id[j]
      data_j[j,2]<-nob[id[j]]
    }
  }
  dimnames(data_j)<-list(1:length(id),c("id","nobs"))
  data_j
}
