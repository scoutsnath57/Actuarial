#Bornhuetter-Ferguson Triangle
bftriangle <- function(claims, premiums) {
  ##The below is to remove the values
  for (a in 1:dim(claims)[1]) {
    for (b in 1:dim(claims)[2]){
      if (is.na(claims[a,b]) ==TRUE) {
        claims[a,b] <- 0
      }
    }
  }       
  
  
  devfactors <- vector(mode = "list")
  counter_2 <- 0
  for (j in dim(claims)[2]:3){
    counter <- 0
    counter_1 <- 0
    for (i in 1:dim(claims)[1]){
      if (claims[i,j] != 0){
        counter <- counter + claims[i,j]
        counter_1 <- counter_1 + claims[i,j-1]
      }
    }
    counter_2<- counter_2 + 1
    devfactors[counter_2] <- counter/counter_1
  }
  
  
  
  #Now let's fill in the 0s
  for (i in dim(claims)[1]:1){
    k <- dim(claims)[1] - 1
    for (j in 2:dim(claims)[2]){
      if (j<dim(claims)[2] &  isTRUE(claims[i,j+1] == 0) & k>0) {
        claims[i,j+1] <- claims[i,j] * devfactors[k][[1]]
      }
      k <- k -1
    }
  }
  
  tclaims <- 0
  for (i in 1:dim(claims)[1]){
    tclaims<- tclaims + claims[i,dim(claims)[2]]
  }
  
  
  sclaims <- 0 #Settled Claims
  counter <- dim(claims)[2]
  for (i in 1:dim(claims)[1]){
    sclaims <- sclaims + claims[i,counter]
    counter <- counter - 1
  }
  
  rreserves <- tclaims - sclaims #Total claims minues settled claims = required reserve
  
  #Now let's do the bornhuetter-ferguson!!
  ult <- claims[1,dim(claims)[2]]
  loss_ratio <- ult/premiums[1,1]
  ult_list = premiums * loss_ratio
  #Making the cumulative development factors
  F <- c(1)
  #m <- length(devfactors)
  for (i in 1:length(devfactors)){
    F[i+1] <- F[i] * devfactors[i][[1]]
  }
  
  newF <- c()
  for (i in 1:length(F)){
    newF[i] <- 1 - 1/F[i]
  }
  
  emerging <- c()
  for (i in 1:length(newF)){
    emerging[i] <- newF[i] * ult_list[i,1]
  }
  
  ORR <- 0
  for (i in 1:length(emerging)){
    ORR <- ORR + emerging[i] 
  }
  names(ult_list) <- c("Ultimate.Claims")
  
  bf_triangle <- list("table" = claims, "total_claims" = tclaims, "settled_claims" = sclaims, "required_reserves" = rreserves, "premiums" = premiums, "ultimate_claims" = ult_list, "emerging_liabilties" = emerging, "overall_reserve_requirement" = ORR)
  
  print(bf_triangle)
}


#Delay Triangle

delaytriangle <- function(claims) {
  ##The below is to remove the values
  for (a in 1:dim(claims)[1]) {
    for (b in 1:dim(claims)[2]){
      if (is.na(claims[a,b]) ==TRUE) {
        claims[a,b] <- 0
      }
    }
  }       
  
  
  devfactors <- vector(mode = "list")
  counter_2 <- 0
  for (j in dim(claims)[2]:3){
    counter <- 0
    counter_1 <- 0
    for (i in 1:dim(claims)[1]){
      if (claims[i,j] != 0){
        counter <- counter + claims[i,j]
        counter_1 <- counter_1 + claims[i,j-1]
      }
    }
    counter_2<- counter_2 + 1
    devfactors[counter_2] <- counter/counter_1
  }
  
  
  
  #Now let's fill in the 0s
  for (i in dim(claims)[1]:1){
    k <- dim(claims)[1] - 1
    for (j in 2:dim(claims)[2]){
      if (j<dim(claims)[2] &  isTRUE(claims[i,j+1] == 0) & k>0) {
        claims[i,j+1] <- claims[i,j] * devfactors[k][[1]]
      }
      k <- k -1
    }
  }
  
  tclaims <- 0
  for (i in 1:dim(claims)[1]){
    tclaims<- tclaims + claims[i,dim(claims)[2]]
  }
  
  
  sclaims <- 0 #Settled Claims
  counter <- dim(claims)[2]
  for (i in 1:dim(claims)[1]){
    sclaims <- sclaims + claims[i,counter]
    counter <- counter - 1
  }
  
  rreserves <- tclaims - sclaims #Total claims minues settled claims = required reserve
  
   d_triangle <- list("table" = claims, "total_claims" = tclaims, "settled_claims" = sclaims, "required_reserves" = rreserves)
  
  print(d_triangle)
 
}

setwd("C:/Users/Oem/Documents/Coding")
getwd()
delay_test <- read.csv("delay_test.csv")
prems <- read.csv("Premiums.csv")

delaytriangle(delay_test)
bftriangle(delay_test,prems)
