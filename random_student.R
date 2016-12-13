#Title: random_student
#Type: function without input
#Description: Picks randoms student from members list without pickcing same name again afterwards
#Author: Jakob Schwalb-Willmann
#Date of creation: 13-12-2016
#License: free for EAGLEs

#------------------------------------------------------------------------------------------------

random_student <- function(){
  #add members here:
  members <- c("Jakob","Julia","Louis","Johannes","Sebastian","Sarah","Marina","Md","Ahmed","Karsten","Fowad","Kamrul","Pilar")
  
  #this part might remain unchanged
  max <- length(members)
  check <- FALSE
  if(exists("random_numb_dig") == FALSE){
    random_numb_dig <<- sample(max,1)
    return(members[random_numb_dig[length(random_numb_dig)]])
  }else{
    if(length(random_numb_dig) == max){
      print("No further numbers are left within the defined range. Starting over...")
      random_numb_dig <<- sample(max,1)
      return(members[random_numb_dig[length(random_numb_dig)]])
    }else{
      while(check == FALSE){
        x_test <- sample(max,1)
        for(i in 1:(length(random_numb_dig))){
          if(random_numb_dig[i] == x_test){
            check <- FALSE
            break()
          }else{
            if(i == length(random_numb_dig)){
              check <- TRUE
              random_numb_dig <<- c(random_numb_dig,x_test)
              return(members[random_numb_dig[length(random_numb_dig)]])
            }
          }
        }
      }
    }
  }
}