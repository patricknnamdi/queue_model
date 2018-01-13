#  Simulation of a Single Server Queue with Redispersion 
#  Feb 21, 2017 

#  Input

#  alpha.prime - external arrival rate
#  beta - vector of redispersion rates
#  mu - service rate
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
#  redisVirtualApprox - redispersion as a function of virtual wait 
#  virtualWaitVec - vector of the virtual waits 
#  numCustSyst - number of customers in system at ith time step 
#  load - current load of the system 

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
redisVirtualApprox <- c()
virtualWaitVec <- c()
numCustSyst <- c() 
load <- c() 
timeVec <- c() 

# Parameters 
alpha.prime <- 30
mu <- 40
target.no.customers <- 20000
warmup.no.cust <- 10000

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
    no.in.system <- 1
    
    # calculate avg number of customers in the system
    avg.no.in.system <- avg.no.in.system + ((next.arr - time) * no.in.system)
    
    # Generate service time for new arrival
    next.serv <- rexp(1, mu)   
    
    # update virtual wait of system 
    virtual.wait <- next.serv
    
    # add virtual wait to vector of VWs
    virtualWaitVec[m] <- virtual.wait
    
    # calculate load of the system 
    load[m] <- sum(departures - new.time)
    
    # determine the redispersion as a function of virtual wait
    redisVirtualApprox[m] <- infestivity(load[m])
    
    # schedule future departure time for this cust
    departures[length(departures) + 1] <- new.time + virtual.wait
    
    # determine new rate of arrival dependent on a' and redispersion 
    redisVect[m] <- redispersionLogis(custRemainArr, new.time) 
    current.rate <- alpha.prime + redisVirtualApprox[m]
    
    # Generate next interarrival time
    next.arr <- new.time + rexp(1, current.rate)  
    
    # update the arrival rate vector 
    current.rate.v[n] <- current.rate
    
    # add number of cust in system 
    numCustSyst[m] <- no.in.system
    
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
      
      # calculate load of the system 
      load[m] <- sum(departures - new.time)
      
      # determine the redispersion as a function of virtual wait
      redisVirtualApprox[m] <- infestivity(load[m])
      
      # determine new rate of arrival based on a' and redispersion
      redisVect[m] <- redispersionLogis(custRemainArr, new.time) 
      current.rate <- alpha.prime + redisVirtualApprox[m]
      
      # Generate next interarrival time
      next.arr <- new.time + rexp(1, current.rate) 
      
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
avg.no.in.system<-0
print("end.warmup"); print(end.warmup)
print("n");print(n)
print("no.in.system"); print(no.in.system)

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
    
    # calculate load of the system 
    load[m] <- sum(departures - new.time)
    
    # determine the redispersion as a function of virtual wait
    redisVirtualApprox[m] <- infestivity(load[m])
    
    # schedule future departure time for this cust
    departures[length(departures) + 1] <- new.time + virtual.wait
    
    # determine new rate of arrival dependent on a' and redispersion 
    redisVect[m] <- redispersionLogis(custRemainArr, new.time) 
    current.rate <- alpha.prime + redisVirtualApprox[m]
    
    # Generate next interarrival time
    next.arr <- new.time + rexp(1, current.rate)  
    
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
      
      # calculate load of the system 
      load[m] <- sum(departures - new.time)
      
      # determine the redispersion as a function of virtual wait
      redisVirtualApprox[m] <- infestivity(load[m])
      
      # determine new rate of arrival based on a' and redispersion
      redisVect[m] <- redispersionLogis(custRemainArr, new.time) 
      current.rate <- alpha.prime + redisVirtualApprox[m]
      
      # Generate next interarrival time
      next.arr <- new.time + rexp(1, current.rate) 
      
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

# cumulative average 
cumAvgCustTime <- cumsum(customer.total.time) / seq_along(customer.total.time)
plot(cumAvgCustTime, type = "l", xlab = "No. of Customers")

# Plot Approx Redispersion - Beta as a function of load
plot(timeVec[warmup.no.cust:target.no.customers], redisVirtualApprox[warmup.no.cust:target.no.customers], 
     xlab = "Time", ylab = "Redispersion - Load", type = "l")

