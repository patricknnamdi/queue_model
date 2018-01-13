# Initialization and Parameters 
delta <- 100 
alpha.prime <- 1

custRemainArr <- c(0)
custDepartures <- 0.3846708

######### REDISPERSION - Sum of Logistic Function ###########

# output: infestivity (infestations per house per year)
infestivity <- function(t) {
  
  # carrying capacity 3 infestations per house per year
  k<-3 
  # initial infestivity (0.1 infestations per house per year)
  y0<-0.1
  r<-1.68
  out <- k / (1 + ((k - y0) / y0 ) * exp(-r * t))
  return(out)
}

redispersionLogis <- function(arrivalTimeVec, currTime) {
  
  sumInfestivity <- 0
  
  # sum over the logistic function 
  for (i in 1:length(arrivalTimeVec)) {
    diffTime = currTime - arrivalTimeVec[i]
    sumInfestivity <- sumInfestivity + infestivity(diffTime)
  }
  
  return(sumInfestivity)
}

#################### Thinning Algorithm ###################

# Algorithm start time 
time <- 0 

# vector of accepted values for arrival time 
arrivalSelected <- c()

# vector of lambda's at accepted values 
lambdaSelected <- c()

# n is each step of success 
n <- 0

# vector of all lambda and time
allLambda <- c()
allTime <- c()
m <- 0 

thinningTest <- function(currTime, servTime) {
  
  # Generate random number 
  uniRandom <- runif(1, 0, 1)
  
  futureArrivalTime <- currTime + ( (-1/delta) * log(uniRandom))
  if (futureArrivalTime > servTime) {
    lambdaFinal <- alpha.prime + redispersionLogis(custRemainArr, futureArrivalTime)
    allLambda[m] <<- lambdaFinal 
    allTime[m] <<- futureArrivalTime
    m <<- m + 1 
    return(futureArrivalTime)
  }
  
  # Generate second random number 
  uniRandomTwo <- runif(1, 0, 1)
  
  # obtain arrival rate
  lambda <- alpha.prime + redispersionLogis(custRemainArr, futureArrivalTime)
  
  if (uniRandomTwo <= (lambda/delta)) {
    arrivalSelected[n] <<- futureArrivalTime
    lambdaSelected[n] <<- lambda
    n <<- n + 1
    
    allLambda[m] <<- lambda 
    allTime[m] <<- futureArrivalTime
    m <<- m + 1 
    return(thinningTest(futureArrivalTime, servTime))
  }
  
  else {
    allLambda[m] <<- lambda 
    allTime[m] <<- futureArrivalTime
    m <<- m + 1 
    return(thinningTest(futureArrivalTime, servTime))
    
  }
  
}

######### Run ############
thinningTest(time, 9)

# plot lambda over all time
plot(allTime, allLambda, xlab = "Time", ylab = "lambda", type = "l")
points(arrivalSelected, lambdaSelected)
