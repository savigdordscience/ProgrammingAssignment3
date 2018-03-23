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
  ## Initializing an empty hospitals vector 
  hospitals = c();
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  ## Selecting all of the rows according to the state in interest
  recordsByState <- subset(outcomeData, State == state)
  ## Defining which row should be selected from the sorted and selected rows
  if (num == "best")
    rowNumber <- 1
  else if (num == "worst") 
    rowNumber <- -1 
  else 
    rowNumber <- num;
  ## Return NA in case there aren't enough rows to supply the request
  if (NROW(recordsByState) < rowNumber)
    return(NA)
  ## Changing the selected outcome column into a numeric one
  recordsByState[,columnName] <- sapply(recordsByState[,columnName],as.numeric)

  ## Sort the records by state and define which row number to select  
  if (rowNumber > 0)
    recordsByStateSorted <- recordsByState[with(recordsByState,order(recordsByState[columnName],recordsByState["Hospital.Name"])),]
  else if (rowNumber < 0)
  {
    recordsByStateSorted <- recordsByState[with(recordsByState,order(-recordsByState[columnName],recordsByState["Hospital.Name"])),]
    rowNumber <- -rowNumber
  }
  ## In case outcome is NA stop, and return NA.
  if (is.na(recordsByStateSorted[1,columnName]))
    return(NA);
  ## Return the list of hostpitals from the selected subset of rows.
  hospitals <- rbind(hospitals,recordsByStateSorted[rowNumber,"Hospital.Name"])
  return(hospitals)
}