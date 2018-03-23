rankhospital <- function(state, outcome, num = "best") {
  options(warn=-1)
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validState = (state %in% unique(outcomeData[,7]))
  if (!validState)
      stop("invalid state")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if (outcome == "heart attack")
    columnName = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  else if (outcome == "heart failure")
    columnName = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  else if (outcome == "pneumonia")
    columnName = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  else
    stop("invalid outcome")
  hospitals = c();
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  recordsByState <- subset(outcomeData, State == state)
  write.csv(recordsByState,"as3-rcbs.csv")
  if (num == "best")
      maxRows = 1
  else if (num == "worst")
           maxRows = -1
  else 
      maxRows = num;
  if (NROW(recordsByState) < maxRows)
      return(NA)
  recordsByState[,columnName] <- sapply(recordsByState[,columnName],as.numeric)
  write.csv(recordsByState,"as3-rcbs-numeric.csv")
  if (maxRows > 0)
      recordsByStateSorted <- recordsByState[with(recordsByState,order(recordsByState[columnName],recordsByState["Hospital.Name"])),]
  else if (maxRows < 0)
       {
           recordsByStateSorted <- recordsByState[with(recordsByState,order(-recordsByState[columnName],recordsByState["Hospital.Name"])),]
           maxRows <- -maxRows
       }
  write.csv(recordsByStateSorted,"as3-rcbs-sorted.csv")
  if (is.na(recordsByStateSorted[1,columnName]))
      return(NA);
  for (i in 1:maxRows)
  {
    hospitals <- rbind(hospitals,recordsByStateSorted[i,"Hospital.Name"])
  }
  return(hospitals)
}