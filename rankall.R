rankall <- function(outcome, num = "best") {
  ## Suppress warnings (aimed for the sapply function)
  options(warn=-1)
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  ## Convert the outcome parameter to a column in the read data
  if (outcome == "heart attack")
      columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  else if (outcome == "heart failure")
           columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  else if (outcome == "pneumonia")
           columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  else
    stop("invalid outcome")
  ## Initialize the hospitals data frame
  hospitals <- data.frame();
  setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("hospital", "state"))
  ## Mapping the states from the database into a vector while removing duplicates
  states = unique(outcomeData[,7])
  ##write.csv(states,"as3-states.csv")
  
  ## Mapping the num parameter into a numeric ranking. In order to describe the worst 
  ## ranking, a -1 is assigned into ranking
  if (num == "best")
      ranking <- 1
  else if (num == "worst")
           ranking <- -1
  else 
       ranking <- num;
  
  ## Scanning all of the states for the requested ranking
  for (state in 1:NROW(states))
  {
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    ## Selecting a list of records for a specific state
    recordsByState <- subset(outcomeData, State == states[state])
    
    if (ranking > NROW(recordsByState))
    {
        hospitals <- rbind(hospitals,data.frame(hospital="<NA>",state = states[state]))
        next;
    }
    recordsByState[,columnName] <- sapply(recordsByState[,columnName],as.numeric)
    
    
    dup <- 1
    if (ranking > 0)
        ## Sorting the selected records ascendingly by the outcome and the hospital name (for best results)
        recordsByStateSorted <- recordsByState[with(recordsByState,order(recordsByState[columnName],recordsByState["Hospital.Name"])),]
    else if (ranking < 0)
    {        
             ## Sorting the selected records descendingly by the outcome and ascendingly the hospital name (for worst results)
             recordsByStateSorted <- recordsByState[with(recordsByState,order(-recordsByState[columnName],recordsByState["Hospital.Name"])),]
             dup <- -1
    }
    ## Adding the best/worst/specifically ranked hospital to the hospitals vector
    hospitals <- rbind(hospitals,data.frame(hospital=recordsByStateSorted[dup*ranking,"Hospital.Name"],state = recordsByStateSorted[dup*ranking,"State"]))
  }
 ## Sorted hospitals by the state
 hospitalsSorted <- hospitals[with(hospitals,order(hospitals["state"])),]
 return(hospitalsSorted)
}

