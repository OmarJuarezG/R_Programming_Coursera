corr_function <- function(directory,threshold=""){
  files <- list.files(path = directory,pattern = ".csv")
  nobs <- numeric()
  for (i in 1:332) {
    data <- read.csv(files[i],header = TRUE,sep = ",")
    data <- data[complete.cases(data),]
    if(threshold<nrow(data)){
      nobs <- c(nobs,cor(data$sulfate,data$nitrate))
    }
  }
  return(nobs)
}