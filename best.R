best <- function(state, outcome)
{
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
      columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  else if (outcome == "heart failure")
           columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  else if (outcome == "pneumonia")
           columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  else
      stop("invalid outcome")
  
  ## Selecting all of the  outcomes of a certain state
  recordsByState <- subset(outcomeData, State == state)
  
  ## Transform the outcome column into a numeric values
  recordsByState[,columnName] <- sapply(recordsByState[,columnName],as.numeric)
  
  ## Sorting all of the rows according to the outcome and the hospital name
  recordsByStateSorted <- recordsByState[with(recordsByState,order(recordsByState[columnName],recordsByState["Hospital.Name"])),]
  
  recordsByStateSorted[1,"Hospital.Name"]
}
