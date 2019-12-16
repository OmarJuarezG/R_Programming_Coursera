pollutantmean <- function(directory,polluntant,id=1:332){
  filelist <- list.files(path = directory,pattern = ".csv")
  for (i in id) {
    data <- read.csv(filelist[i],header = TRUE,sep = ",")
  }
  mean(data[[polluntant]],na.rm = TRUE)
}