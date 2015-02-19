corr <- function(directory, threshold = 0) {
  #setwd(directory)
  
  corr_vector <- numeric(0)
  for (i in 1:323)
  {
    while (nchar(i) < 3)
    {
      i = paste0("0",i)
    }
    tempdata <- read.csv(paste0(i,".csv"))
    tempdata <- subset(tempdata, sulfate != "NA" & nitrate != "NA")
    if (nrow(tempdata) > threshold)
    {
      corr_vector <- c(corr_vector, cor(tempdata$nitrate,tempdata$sulfate))
    }
    
    
  }
  #corr_vector[is.na(corr_vector)] <- 0
  r
