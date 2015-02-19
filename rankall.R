rankall <- function(outcome, num = "best")
{
  data <- read.csv("./outcome-of-care-measures.csv")
  interestvars <- c("Hospital.Name","State"
                    ,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                    ,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                    ,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  data <- data[interestvars]
  output <- c()
  
  for (state in c(state.abb,"DC"))
  {
    
    if (outcome %in% c("heart attack","heart failure","pneumonia"))
    {
      if (outcome == "heart attack")
      {
        
          tempset <- subset(data, State == state)
          tempset <- subset(tempset, tempset[,3] != "Not Available")
          tempset[,3] <- as.numeric(as.character(tempset[,3]))
          if (is.numeric(num))
          {
            output <- rbind(output, c(as.character(tempset[order(tempset[,3] ,tempset[,1]),][num,1]), state))
          }
          if (num == "best")
          {
            output <- rbind(output, c(as.character(tempset[order(tempset[,3] ,tempset[,1]),][1,1]), state))
          }
          if (num == "worst")
          {
            output <- rbind(output, c(as.character(tempset[order(-tempset[,3] ,tempset[,1]),][1,1]), state))
          }
          if (!is.numeric(num) & num != "best" & num != "worst")
          {
            print(num)
            stop("invalid rank")
          }
        }
        
      
      if (outcome == "heart failure")
      {
        tempset <- subset(data, State == state)
        tempset <- subset(tempset, tempset[,4] != "Not Available")
        tempset[,4] <- as.numeric(as.character(tempset[,4]))
        if (is.numeric(num))
        {
          output <- rbind(output, c(as.character(tempset[order(tempset[,4] ,tempset[,1]),][num,1]), state))
        }
        if (num == "best")
        {
          output <- rbind(output, c(as.character(tempset[order(tempset[,4] ,tempset[,1]),][1,1]), state))
        }
        if (num == "worst")
        {
          output <- rbind(output, c(as.character(tempset[order(-tempset[,4] ,tempset[,1]),][1,1]), state))
        }
        if (!is.numeric(num) & num != "best" & num != "worst")
        {
          print(num)
          stop("invalid rank")
        }
      }
      if (outcome == "pneumonia")
      {
        tempset <- subset(data, State == state)
        tempset <- subset(tempset, tempset[,5] != "Not Available")
        tempset[,5] <- as.numeric(as.character(tempset[,5]))
        if (is.numeric(num))
        {
          output <- rbind(output, c(as.character(tempset[order(tempset[,5] ,tempset[,1]),][num,1]), state))
        }
        if (num == "best")
        {
          output <- rbind(output, c(as.character(tempset[order(tempset[,5] ,tempset[,1]),][1,1]), state))
        }
        if (num == "worst")
        {
          output <- rbind(output, c(as.character(tempset[order(-tempset[,5] ,tempset[,1]),][1,1]), state))
        }
        if (!is.numeric(num) & num != "best" & num != "worst")
        {
          print(num)
          stop("invalid rank")
        }
      }
    }
    else
    {
      stop("invalid outcome")
      
    }
  }
  output <- data.frame(output)
  names(output) <- c("hospital","state")
  return(data.frame(output))
}
