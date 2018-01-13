#  Simulation of a Single Server Queue with Redispersion with Thinning Algorithm 
#  June 20, 2017 

#  Input

#  alpha.prime - external arrival rate
#  beta - vector of redispersion rates
#  mu - service rate
#  delta - maximum arrival rate (max(lambda(t) = alphaPrime + beta))
#  target.no.cust - simulation will produce that many customers going through the system

#  Variables

#  lambda - alpha.prime + beta vector of total arrival rates
#  current.rate - current arrival rate to the system
#  time - simulation clock
#  timeVec - time at event simulation event 
#  new.time - updated simulation clock
#  virtual.wait - the amount of time an incoming customer would spend in the system
#  no.in.system - number of customers in system
#  next.arr - arrival time of next customer (not interarrival time!)
#  next.serv - service time of next customer
#  departures - vector of future departure times
#  avg.no.in.system - average number of customer in system
#  avg.time.in.system - average time a customer spends in system
#  avg.arr.rate - average rate of arrival
#  redisVect - vector of redispersion rates at each arrival 
#  warmup.no.cust - initial number of customers that will not be counted towards statistics
#  end.warmup - time when the number of departures has reached the limit warmup
#  custRemainArr - arrival times of the remaining customers in the queue 
#  redisVirtual - redispersion as a function of virtual wait 
#  virtualWaitVec - vector of the virtual waits 
#  numCustSyst - number of customers in system at ith time step 
#  load - current load of the system 
#  avgBeta - time average of redispersion 

#  Initialization

set.seed(1)         
no.in.system <- 0
virtual.wait <- 0
avg.no.in.system <- 0
departures <- c()    #  list of departure times for customers in queue
n <- 0
m <- 0
time <- 0
current.rate.v <- c()
redisVect <- c() 
custRemainArr <- c()
redisVirtual <- c()
virtualWaitVec <- c()
numCustSyst <- c() 
load <- c() 
timeVec <- c() 
avgBeta <- 0

# Parameters 
alpha.prime <- 25
mu <- 40
target.no.customers <- 10
warmup.no.cust <- 1
delta <- 100

customer.arrival <- c()
customer.total.time <- c()

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
thinning <- function(currTime, servTime) {
  
  # Generate random number 
  uniRandom <- runif(1, 0, 1)
  
  futureArrivalTime <- currTime + ( (-1/delta) * log(uniRandom))
  if (futureArrivalTime > servTime) {
    # save curr.time, load, and redispersion 
    
    
    return(futureArrivalTime)
  }
  
  # Generate second random number 
  uniRandomTwo <- runif(1, 0, 1)
  
  # obtain arrival rate
  lambda <- alpha.prime + redispersionLogis(custRemainArr, futureArrivalTime)
  if (uniRandomTwo <= (lambda/delta)) {
    
    # save curr.time, load, and redispersion 
    
    
    return(futureArrivalTime)
  }
  else {
    
    # save curr.time, load, and redispersion 
    
    
    return(thinning(futureArrivalTime, servTime))
  }
}

################## WARM UP PERIOD START ######################

#  Create first arrival
current.rate <- alpha.prime

# Generate first interarrival time (exp with rate current.rate)
next.arr <- rexp(1 , current.rate) 

while (n <= warmup.no.cust) {
  
  # System is empty
  if (length(departures) == 0) {  
    
    # Update simulation clock
    new.time <- next.arr  
    m <- m + 1
    
    # store cust arrival time 
    customer.arrival[m] <- next.arr
    custRemainArr[length(custRemainArr) + 1] <- next.arr ##
    
    # calculate avg number of customers in the system
    avg.no.in.system <- avg.no.in.system + ((next.arr - time) * no.in.system)
    
    no.in.system <- 1
    
    # Generate service time for new arrival
    next.serv <- rexp(1, mu)   
    
    # update virtual wait of system 
    virtual.wait <- next.serv
    
    # add virtual wait to vector of VWs
    virtualWaitVec[m] <- virtual.wait
    
    # determine the redispersion as a function of virtual wait
    redisVirtual[m] <- infestivity(virtual.wait)
    
    # schedule future departure time for this cust
    departures[length(departures) + 1] <- new.time + virtual.wait
    
    # determine new rate of arrival dependent on a' and redispersion 
    redisVect[m] <- redispersionLogis(custRemainArr, new.time) 
    current.rate <- alpha.prime + redisVect[m]
    
    # calculate average beta as func of time 
    avgBeta <- avgBeta + ((next.arr - time) * redisVect[m])
    
    # Generate next interarrival time via THINNING
    next.arr <- thinning(new.time, next.serv)
    
    # update the arrival rate vector 
    current.rate.v[n] <- current.rate
    
    # add number of cust in system 
    numCustSyst[m] <- no.in.system
    
    # calculate load of the system 
    load[m] <- sum(departures - new.time)
  }
  
  # System is not empty
  else { 
    
    #  The next event is an arrival
    if (next.arr<departures[1]) { 
      
      # store cust arrival time 
      m <- m + 1
      customer.arrival[m] <- next.arr
      custRemainArr[length(custRemainArr) + 1] <- next.arr ##
      
      # Update simulation clock
      new.time <- next.arr 
      
      # update number in system 
      no.in.system <- no.in.system + 1
      
      # calculate avg number of customers in the system
      avg.no.in.system <- avg.no.in.system + ((next.arr - time) * no.in.system)
      
      # Generate service time for this arrival and calculate virtual wait
      next.serv <- rexp(1, mu) 
      virtual.wait <- virtual.wait - (next.arr - time) + next.serv 
      departures[length(departures) + 1] <- new.time + virtual.wait
      
      # add virtual wait to vector of VWs
      virtualWaitVec[m] <- virtual.wait
      
      # determine the redispersion as a function of virtual wait
      redisVirtual[m] <- infestivity(virtual.wait)
      
      # determine new rate of arrival based on a' and redispersion
      redisVect[m] <- redispersionLogis(custRemainArr, new.time) 
      current.rate <- alpha.prime + redisVect[m]
      
      # calculate average beta as func of time 
      avgBeta <- avgBeta + ((next.arr - time) * redisVect[m])
      
      # Generate next interarrival time via THINNING
      next.arr <- thinning(new.time, next.serv) 
      
      # update the arrival rate vector 
      current.rate.v[n] <- current.rate
      
      # add number of cust in system 
      numCustSyst[m] <- no.in.system
      
      # calculate load of the system 
      load[m] <- sum(departures - new.time)
    }
    
    #  the next event is a departure
    else {
      
      # update simulation clock
      new.time <- departures[1] 
      
      # update virtual wait 
      virtual.wait <- virtual.wait - (new.time - time)
      
      no.in.system <- no.in.system - 1
      
      avg.no.in.system <- avg.no.in.system + ((new.time - time) * no.in.system)
      
      # calculate average beta as func of time 
      avgBeta <- avgBeta + ((next.arr - time) * redisVect[m])
      
      # update the number of cust served by 1
      n <- n + 1
      
      customer.total.time[n] <- new.time - customer.arrival[n]
      
      # remove arrive time of service customer 
      custRemainArr <- custRemainArr[-c(1)] 
      
      # remove this departure from scheduled departures
      departures <- departures[-c(1)] 
      
      # add number of cust in system 
      numCustSyst[n] <- no.in.system
      
      # calculate load of the system 
      load[m] <- sum(departures - new.time)
    }
    
  }
  
  timeVec[m] <- new.time
  time <- new.time
  
}

################## WARM UP PERIOD END ######################

end.warmup<-time
print("avg.no.in.system at warmup.end"); print(avg.no.in.system/time)
avg.no.in.system <- 0
print("end.warmup"); print(end.warmup)
print("n");print(n)
print("no.in.system"); print(no.in.system)

avgBeta <- 0

################## START SIMULATION ######################

while (n <= target.no.customers) {
  
  # System is empty
  if (length(departures) == 0) {  
    
    # Update simulation clock
    new.time <- next.arr  
    m <- m + 1
    
    # store cust arrival time 
    customer.arrival[m] <- next.arr 
    custRemainArr[length(custRemainArr) + 1] <- next.arr
    no.in.system <- 1
    
    # calculate avg number of customers in the system
    avg.no.in.system <- avg.no.in.system + ((next.arr - time) * no.in.system)
    
    # Generate service time for new arrival
    next.serv <- rexp(1, mu)   
    
    # update virtual wait of system 
    virtual.wait <- next.serv
    
    # add virtual wait to vector of VWs
    virtualWaitVec[m] <- virtual.wait
    
    # determine the redispersion as a function of virtual wait
    redisVirtual[m] <- infestivity(virtual.wait)
    
    # schedule future departure time for this cust
    departures[length(departures) + 1] <- new.time + virtual.wait
    
    # determine new rate of arrival dependent on a' and redispersion 
    redisVect[m] <- redispersionLogis(custRemainArr, new.time) 
    current.rate <- alpha.prime + redisVect[m]
    
    # calculate average beta as func of time 
    avgBeta <- avgBeta + ((next.arr - time) * redisVect[m])
    
    # Generate next interarrival time via THINNING
    next.arr <- thinning(new.time, next.serv)
    
    # update the arrival rate vector 
    current.rate.v[n] <- current.rate
    
    # add number of cust in system 
    numCustSyst[m] <- no.in.system
    
    # calculate load of the system 
    load[m] <- sum(departures - new.time)
  }
  
  # System is not empty
  else { 
    
    #  The next event is an arrival
    if (next.arr<departures[1]) { 
      
      # store cust arrival time 
      m <- m + 1
      customer.arrival[m] <- next.arr
      custRemainArr[length(custRemainArr) + 1] <- next.arr
      
      # Update simulation clock
      new.time <- next.arr  
      
      # update number in system 
      no.in.system <- no.in.system + 1
      
      # calculate avg number of customers in the system
      avg.no.in.system <- avg.no.in.system + ((next.arr - time) * no.in.system)
      
      # Generate service time for this arrival and calculate virtual wait
      next.serv <- rexp(1, mu) 
      virtual.wait <- virtual.wait - (next.arr - time) + next.serv 
      departures[length(departures) + 1] <- new.time + virtual.wait
      
      # add virtual wait to vector of VWs
      virtualWaitVec[m] <- virtual.wait
      
      # determine the redispersion as a function of virtual wait
      redisVirtual[m] <- infestivity(virtual.wait)
      
      # determine new rate of arrival based on a' and redispersion
      redisVect[m] <- redispersionLogis(custRemainArr, new.time) 
      current.rate <- alpha.prime + redisVect[m]
      
      # calculate average beta as func of time 
      avgBeta <- avgBeta + ((next.arr - time) * redisVect[m])
      
      # Generate next interarrival time via THINNING
      next.arr <- thinning(new.time, next.serv)
      
      # update the arrival rate vector 
      current.rate.v[n] <- current.rate
      
      # add number of cust in system 
      numCustSyst[m] <- no.in.system
      
      # calculate load of the system 
      load[m] <- sum(departures - new.time)
    }
    
    #  the next event is a departure
    else {
      
      # update simulation clock
      new.time <- departures[1] 
      
      # update virtual wait 
      virtual.wait <- virtual.wait - (new.time - time)
      
      # update num of custs in system 
      no.in.system <- no.in.system - 1
      
      # update calculation of avg cust in system
      avg.no.in.system <- avg.no.in.system + ((new.time - time) * no.in.system)
      
      # calculate average beta as func of time 
      avgBeta <- avgBeta + ((next.arr - time) * redisVect[m])
      
      # update the number of cust served by 1
      n <- n + 1
      
      customer.total.time[n] <- new.time - customer.arrival[n]
      
      # remove arrive time of service customer 
      custRemainArr <- custRemainArr[-c(1)] 
      
      # remove this departure from scheduled departures
      departures <- departures[-c(1)] 
      
      # add number of cust in system 
      numCustSyst[n] <- no.in.system
      
      # calculate load of the system 
      load[m] <- sum(departures - new.time)
    }
    
  }
  
  timeVec[m] <- new.time
  time <- new.time
  
}

################### End Simulation ########################


######################## PLOTS ###########################

# plots of redispersion as the cumulative sum of gamma
plot(customer.arrival, redisVect)

hist(redisVect[warmup.no.cust:target.no.customers],
     main = paste("Distribution of Redispersion Rates"),
     xlab = "Redispersion Rates",
     col = "black",
     border = "blue"
     
)

# plots of redispersion as a function of virtual wait 
if (FALSE) {
hist(redisVirtual,
     main = paste("Distribution of Redispersion Rates - Virtual Wait" ),
     xlab = "Redispersion Rates",
     col = "black",
     border = "Red"
)
}

info <- c(avg.no.in.system / (time - end.warmup), mean(customer.total.time[warmup.no.cust:target.no.customers]), 
          mean(redisVect[warmup.no.cust:target.no.customers]), avgBeta/(time - end.warmup))
print(info)

# cumulative average 
cumAvgCustTime <- cumsum(customer.total.time) / seq_along(customer.total.time)
plot(cumAvgCustTime, type = "l", main = paste("AlphaPrime = 33 & Mu = 40"), xlab = "No. of Customers")

# redispersion as a function of time 
plot(timeVec[warmup.no.cust:target.no.customers], redisVect[warmup.no.cust:target.no.customers], 
     xlab = "Time", ylab = "Redispersion", type = "l")

# load as a function of time 
plot(timeVec[warmup.no.cust:target.no.customers], load[warmup.no.cust:target.no.customers], 
     xlab = "Time", ylab = "Load", type = "l")

# redispersion as a function of load
plot(load[warmup.no.cust:target.no.customers], redisVect[warmup.no.cust:target.no.customers], 
     xlab = "Load", ylab = "Redispersion", type = "p")

# number of customers as a function of time 
barplot(timeVec[warmup.no.cust:target.no.customers], numCustSyst[warmup.no.cust:target.no.customers], 
     xlab = "Time", ylab = "No. of Cust", type = "p")
##################### RUN SimApproxBetaApril19 with parameters #######################

if (FALSE) {

# plot the approximate beta 
plot(timeVec[warmup.no.cust:target.no.customers], 
     redisVirtualApprox[warmup.no.cust:target.no.customers], xlab = "Time", 
     ylab = "Approximate Redispersion", type = "l")

# Scatterplot of redisVirtualApprox and RedisVect 
plot(redisVect[warmup.no.cust:target.no.customers], redisVirtualApprox[warmup.no.cust:target.no.customers],  
     xlab = "Approximate Redispersion", ylab = "Redispersion (Sum of Gamma)", type = "p")

# histogram of redisVirtualApprox
hist(redisVirtualApprox[warmup.no.cust:target.no.customers],
     main = paste("Distribution of Approx. Redispersion Rates"),
     xlab = "Approx. Redispersion Rates",
     col = "black",
     border = "blue"
)

# difference between RedisVect and redisVirtualApprox 
diffBeta <- redisVect - redisVirtualApprox

# plot the difference 
plot(timeVec[warmup.no.cust:target.no.customers], 
     diffBeta[warmup.no.cust:target.no.customers], xlab = "Time", ylab = "Difference", 
     type = "l")

}