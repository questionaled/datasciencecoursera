best <- function(state, outcome) { ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death 
  ## rate
  hospital<-read.csv("hospital-data.csv", colClasses = "character") 
  caredata<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  wherestate<-which(caredata[,7]==state)
  caredata[, 11] <- as.numeric(caredata[, 11])
  caredata[, 17] <- as.numeric(caredata[, 17])
  caredata[, 23] <- as.numeric(caredata[, 23])
  if(outcome=="heart attack"){
    minhospital<-which.min(caredata[wherestate,11])
  }
    else if(outcome=="heart failure"){
      minhospital<-which.min(caredata[wherestate,17])
    }
      else{
        minhospital<-which.min(caredata[wherestate,23])
      }
  IDnum<-caredata[(wherestate[1]+minhospital-1),1]
  hospital[which(hospital[,1]==IDnum),2]
}
