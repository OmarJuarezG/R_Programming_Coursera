complete <- function(directory,id=1:332){
  list_files <- list.files(directory,pattern = ".csv") ##identify all the files
  nobs <- numeric() ##empty vector to store values
  ##for loop starts here
  for (i in id) {
    data <- read.csv(list_files[i],header = TRUE,sep = ",") ## read all the files from 1 to 332
    nobs <- c(nobs,sum(complete.cases(data))) ## store those values
  }
  data.frame(id,nobs)
}