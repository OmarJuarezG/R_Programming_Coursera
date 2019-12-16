rankhospital <- function(state, outcome, num="best") {
  ## Read outcome data
  data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ###check for valid outcome input
  #use names(outcome) to check column no.
  #heart attack, column 11
  #heart failure, column 17
  #pnuemonia, column 23
  
  #to look at intermedidate results
  #state <- 'TX'
  #outcome <- 'heart attack'
  #num <- 4
  
  
  
  validState <- unlist(state.abb)
  ###check for valid state
  if (!(state  %in% state.abb))
    stop("invalid state input")
  
  #rename Hospital.Name to Name
  colnames(data1)[1]<- "Name"
  
  
  ##select hospital name, state
  data2 <- data1[c(2,7)]
  
  if (outcome =='heart failure')
    rate <- as.numeric(data1[,17]) else 
      if (outcome == 'heart attack') 
        rate <- as.numeric(data1[,11])  else 
          if (outcome == 'pneumonia'){
            rate <- as.numeric(data1[,23])
          }
  
  
  #combine rate and hospital name
  data3 <- cbind(data2, rate)
  #subset by state
  data3a <- subset(data3, data1$State==state)
  
  #sort using dplyr 
  data4 <- arrange(data3a, data3a[,3], data3a[,1])
  #remove NAs
  data4 <-na.omit(data4)
  
  
  
  #another way to sort
  #data4<- data3a[order(data3a$rate, data3a$Hospital.Name, na.last=NA), ]
  
  ## Return hospital name in that state with lowest 30-day death
  if (num == "best")
    answer <- data4[1,1] else 
      if (num =="worse") 
        answer <- data4[nrow(data4),1] else 
          if (is.numeric(num))  #if num is not a number return the best hospital
            answer <- data4[num,1] else 
              answer <- data4[1,1] 
  #return Hospital Name=Col 1
  answer
}


