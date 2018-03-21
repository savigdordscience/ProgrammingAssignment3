best <- function(state, outcome)
{
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validState = (state %in% unique(outcomeData[,7]))
  if (!validState)
    stop("invalid state")
  print("Great State!!!")

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
  recordsByState <- subset(outcomeData, State == state)
  write.csv(recordsByState,"as3-rcbs.csv")
  ##numericColumn <- as.numeric(recordsByState[,columnName])
  ##write.csv(numericColumn,"as3-numeric-columns.csv")
  recordsByState[,columnName] <- sapply(recordsByState[,columnName],as.numeric)
  write.csv(recordsByState,"as3-rcbs-numeric.csv")
  ##recordsByStateNonNA <- recordsByState[,!(is.na(numericColumn))]
  ##write.csv(recordsByStateNonNA,"as3-rcbs-non-na.csv")
  recordsByStateSorted <- recordsByState[with(recordsByState,order(recordsByState["Hospital.Name"])),]
  write.csv(recordsByStateSorted,"as3-rcbs-sorted.csv")
  lowest30DaysMortality = min(recordsByStateSorted[,columnName], na.rm = TRUE)
  print(paste("lowest30DaysMortality=",lowest30DaysMortality))
  lowestHospitalOutcomes = match(lowest30DaysMortality,recordsByStateSorted[,columnName])
  print(paste("lowestHospitalOutcomes=",lowestHospitalOutcomes))
  print(paste("lowestHospitalOutcomes[0]=",lowestHospitalOutcomes))
  recordsByStateSorted[lowestHospitalOutcomes,"Hospital.Name"]
}