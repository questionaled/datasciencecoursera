rankall <- function(outcome, num = "best") { 
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that outcome is valid
  if (!((outcome == "heart attack") | (outcome == "heart failure")
        | (outcome == "pneumonia"))) {
      stop ("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  col <- if (outcome == "heart attack") {
      11
  }   else if (outcome == "heart failure") {
        17
    }     else {
          23
      }

  data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
  data[, 2] <- as.character(data[, 2])
  states<-levels(data[,7])
  source('rankhospital.R')
  hospitalname<-vector()
  for(i in states){
    numb<-which(data[,7]==i)
    if(num=="worst"){
      hospitalname<-rbind(hospitalname,rankhospital(i,outcome,num))
    }
      else if(num>length(numb)){
        hospitalname<-rbind(hospitalname,NA)
      }
        else{
          hospitalname<-rbind(hospitalname,rankhospital(i,outcome,num))
        }
  }
  ## Return a data frame with the hospital names and the 
  ## (abbreviated) state name    
  listnum<-cbind(hospitalname,states)
  colnames(listnum) <- c("hospital", "state")
  listnum
}
