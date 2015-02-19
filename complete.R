complete <- function(directory, id = 1:332) 
{
  #setwd(directory)
  
  data_output <- c()
  for (i in id)
  {
    name <- i
    while (nchar(i) < 3)
    {
      i = paste0("0",i)
    }
    tempdata <- read.csv(paste0(i,".csv"))
    tempdata <- subset(tempdata, sulfate != "NA" & nitrate != "NA")
    temp_results <- cbind(name, nrow(tempdata))
    data_output <- rbind(data_output, temp_results)
  }
  data_output <- data.frame(data_output)
  names(data_output) <- c("id","nobs")
  return(data_output)
}
