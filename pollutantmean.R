pollutantmean <- function(directory, pollutant, id = 1:332) 
{  
  #setwd(directory)
  
  data_input <- c()
  for (i in id)
  {
    while (nchar(i) < 3)
    {
      i = paste0("0",i)
    }
    tempdata <- read.csv(paste0(i,".csv"))
    data_input <- rbind(data_input, tempdata)
  }
  
  data.sub <- subset(data_input, get(pollutant) != "NA")
  
  pollutant_mean <- mean(data.sub[,pollutant])
  return(round(pollutant_mean, digits=3))    
}
