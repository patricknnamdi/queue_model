#  Simulation of a Single Server Queue with Redispersion with Thinning Algorithm 

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
#  numCustSyst - vector of consecutive num in system (for debugging)
#  p - relates to the number of potential arrivals

#  Initialization

set.seed(1)         
no.in.system <- 0
virtual.wait <- 0
avg.no.in.system <- 0
departures <- c()    #  list of departure times for customers in queue
n <- 0
m <- 0
p <- 0
time <- 0
prev.time <- 0
avgBeta <- 0
redisVect <- c()  
custRemainArr <- c()
virtualWaitVec <- c() 
numCustSyst <- c()  
load <- c() 
customer.arrival <- c()

# Parameters 
alpha.prime <- 5
mu <- 40
target.no.customers <- 1010
warmup.no.cust <- 1000
delta <- 5.5


######### REDISPERSION - Sum of Logistic Function ###########

# output: infestivity (infestations per house per year)
infestivity <- function(t) {
  
  # carrying capacity 3 infestations per house per year
  k <- 3 
  # initial infestivity (0.1 infestations per house per year)
  y0 <- 0.1
  r <- 1.68
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
next.arr <- rexp(1, current.rate) 
print("First Arrival - Warmup"); print(next.arr)

while (n < target.no.customers) {
  
  # System is empty
  if (length(departures) == 0) {  
    
    # Update simulation clock
    prev.time <- time 
    time <- next.arr  
    m <- m + 1
    print("TIME: EMPTY"); print(time)
    
    # calculate avg number of customers in the system
    avg.no.in.system <- avg.no.in.system + ((time - prev.time) * no.in.system)
    print("No.in.system"); print(no.in.system)
    no.in.system <- 1
    
    # add number of cust in system 
    numCustSyst[m] <- no.in.system
    
    # store cust arrival time 
    custRemainArr[1] <- next.arr 
    
    # Generate service time for new arrival
    next.serv <- rexp(1, mu)
    
    # update virtual wait of system 
    virtual.wait <- next.serv
    
    # schedule future departure time for this cust
    departures[length(departures) + 1] <- time + virtual.wait
    print("First departure - Warmup"); print(departures)
    
    # determine new rate of arrival dependent on a' and redispersion 
    redisVect[m] <- redispersionLogis(custRemainArr, time) 
    current.rate <- alpha.prime + redisVect[m]
    
    ################# THINNING ALGORITHM #####################
    
    # thinning algorithm alternative 
    # Generate next interarrival time via THINNING
    
    currTime <- time
    success <- FALSE
    while((currTime < departures[1]) & (success == FALSE)) {
      
      # Generate first and second random number 
      uniRandom <- runif(1, 0, 1)
      uniRandomTwo <- runif(1, 0, 1)
      
      futureArrivalTime <- currTime + ((-1/delta) * log(uniRandom))
      
      # obtain arrival rate
      lambda <- alpha.prime + redispersionLogis(custRemainArr, futureArrivalTime)
      
      if (uniRandomTwo <= (lambda/delta)) {
        
        next.arr <- futureArrivalTime
        print("Thinning Arrival - Empty"); print(next.arr)
        success <- TRUE
      }
      else {
        currTime <- futureArrivalTime
        next.arr <- futureArrivalTime
        # update the load, arrival rate, 
        print("potential arrival- thinning"); print(currTime)
      }
    }
    
    
    print("Next Arrival: System previously empty (thinning)"); print(next.arr)
    
    ##########################################################
    
  }
  
  # System is not empty
  else { 
    
    #  The next event is an arrival
    if (next.arr < departures[1]) { 
      
      # store cust arrival time 
      m <- m + 1
      customer.arrival[m] <- next.arr
      custRemainArr[length(custRemainArr) + 1] <- next.arr 
      
      # Update simulation clock
      prev.time <- time
      time <- next.arr 
      print("TIME: NON-EMPTY"); print(time)
      
      # calculate avg number of customers in the system
      avg.no.in.system <- avg.no.in.system + ((time - prev.time) * no.in.system)
      print("No.in.system"); print(no.in.system)
      # update number in system 
      no.in.system <- no.in.system + 1
      
      # add number of cust in system 
      numCustSyst[m] <- no.in.system
      
      # Generate service time for this arrival and calculate virtual wait
      next.serv <- rexp(1, mu) 
      
      virtual.wait <- virtual.wait - (next.arr - prev.time) + next.serv 
      
      departures[length(departures) + 1] <- time + virtual.wait
      print("First departure - Warmup"); print(departures)
      
      # determine new rate of arrival based on a' and redispersion
      redisVect[m] <- redispersionLogis(custRemainArr, time) 
      current.rate <- alpha.prime + redisVect[m]
      
      ################# THINNING ALGORITHM #####################
      # thinning algorithm alternative 
      # Generate next interarrival time via THINNING
      
      currTime <- time
      success <- FALSE
      while((currTime < departures[1]) & (success == FALSE)) {
        
        # Generate first and second random number 
        uniRandom <- runif(1, 0, 1)
        uniRandomTwo <- runif(1, 0, 1)
        
        futureArrivalTime <- currTime + ((-1/delta) * log(uniRandom))
        
        # obtain arrival rate
        lambda <- alpha.prime + redispersionLogis(custRemainArr, futureArrivalTime)
        
        if (uniRandomTwo <= (lambda/delta)) {
          
          next.arr <- futureArrivalTime
          print("Thinning Arrival - Empty"); print(next.arr)
          success <- TRUE
        }
        else {
          currTime <- futureArrivalTime
          next.arr <- futureArrivalTime
          # update the load, arrival rate, 
          print("potential arrival- thinning"); print(currTime)
        }
      }
      
      ##########################################################
      print("Next Arrival: Non-empty"); print(next.arr)
      
    }
    
    #  the next event is a departure
    else {
      
      # update simulation clock
      prev.time <- time
      print("PREV TIME: DEPARTURE"); print(prev.time)
      time <- departures[1] 
      print("TIME: DEPARTURE"); print(time)
      
      # update virtual wait 
      virtual.wait <- virtual.wait - (time - prev.time)
      
      # average number in system
      avg.no.in.system <- avg.no.in.system + ((time - prev.time) * no.in.system)
      print("avg.no: DEPARTURE"); print(avg.no.in.system)
      
      # current number in system
      no.in.system <- no.in.system - 1
      
      # add number of cust in system 
      numCustSyst[n] <- no.in.system
      
      # remove arrive time of serviced customer 
      custRemainArr <- custRemainArr[-c(1)] 
      
      # remove this departure from scheduled departures
      departures <- departures[-c(1)] 
      
      # update the number of cust served by 1
      n <- n + 1 
      
      if (n == warmup.no.cust) {
        print(noquote("------------------------ WARM UP PERIOD  ---------------------------"))
        end.warmup <- time
        print(noquote(paste("Average Number in System at End of Warmup: ", avg.no.in.system/time)))
        print(noquote(paste("Time at end of Warmup (end.warmup): ", end.warmup)))
        print(noquote(paste("Number of Customers Served (n): ", n)))
        print(noquote(paste("Current Number in System (no.in.system): ", no.in.system)))
        avg.no.in.system <- 0
        print(noquote("--------------------------------------------------------------------"))
      }
      
      if (n == target.no.customers) { 
        print(noquote("------------------------ SIMULATION PERIOD -------------------------"))
        print(noquote(paste("Average Number in System at End of Simulation: ", avg.no.in.system/(time - end.warmup))))
        print(noquote(paste("Time at end of Simulation (time): ", time)))
        print(noquote(paste("Number of Customers Served (n): ", n)))
        print(noquote(paste("Current Number in System (no.in.system): ", no.in.system)))
        avg.no.in.system <- 0
        print(noquote("--------------------------------------------------------------------"))
        }
    }
    
  }
  
}