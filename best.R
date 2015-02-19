best <- function(state, outcome)
{
  data <- read.csv("./outcome-of-care-measures.csv")
  interestvars <- c("Hospital.Name","State"
                    ,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                    ,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                    ,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  data <- data[interestvars]
  if (state %in% data$State)
  {
    tempset <- subset(data, State == state)
    if (outcome %in% c("heart attack","heart failure","pneumonia"))
    {
      if (outcome == "heart attack")
      {
        tempset <- subset(tempset, tempset[,3] != "Not Available")
        tempset[,3] <- as.numeric(as.character(tempset[,3]))
        return(as.character(tempset[order(tempset[,3] ,tempset[,1]),][1,1]))
      }
      if (outcome == "heart failure")
      {
        tempset <- subset(tempset, tempset[,4] != "Not Available")
        tempset[,4] <- as.numeric(as.character(tempset[,4]))
        return(as.character(tempset[order(tempset[,4] ,tempset[,1]),][1,1]))
      }
      if (outcome == "pneumonia")
      {
        tempset <- subset(tempset, tempset[,5] != "Not Available")
        tempset[,5] <- as.numeric(as.character(tempset[,5]))
        return(as.character(tempset[order(tempset[,5] ,tempset[,1]),][1,1]))
      }
    }
    else
    {
      stop("invalid outcome")
      
    }
  }
  else
  {
    stop("invalid state")
  }
}
