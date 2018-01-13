## other code
## single.server <- function(beta, a = 0:(length(beta)-1), alpha, c, p, mu, javaClass) {
##    treatedStats <- .jcall(javaClass,"[D","singleServer",.jarray(as.numeric(a)),.jarray(as.numeric(beta)), as.numeric(alpha), as.numeric(c), as.numeric(p), as.numeric(mu))
##    return(treatedStats)
## }

## model parameters
## beta - vector of size M containing redispersion rates for each age levels
## a - vector of size M containing age levels
## alpha - rate of infection in attack zone
## c - participation rate
## p - prevalence of infection
## mu - service rate
ActualRate <- function(beta, a=0:(length(beta)-1), alpha, c, p, mu) {
	## initialization
	alphaPrime <- alpha*(1-c)*p
	M <- length(beta) 
	lambda <- beta + alphaPrime

	## a at 1 must be 0
	## note in R, the 1st index is the 0th index given by Brill's notation
	a[1] <- 0

	## initialize ending values of b, B, and M
	b <- rep(0, length(beta))
	b[M] <- 1

	B <- rep(0, length(beta))
	B[M] <- exp(-mu*a[M])/mu

	G <- rep(0, length(beta))
	G[M] <- exp(-(mu-lambda[M])*a[M])/(mu-lambda[M])

	## recurrence loop
	for(j in (M-1):1) {

		b[j] <- (-mu*(lambda[j+1]-lambda[j])/(lambda[j+1]-lambda[j]+mu)*exp((lambda[j+1]-lambda[j]+mu)*a[j+1])*B[j+1])
    b[j] <- b[j] + exp((lambda[j+1]-lambda[j])*a[j+1])*b[j+1]

		if(j+2<=M)
			for(k in (j+2):M) {
				b[j] <- b[j] + mu*((((lambda[k]-lambda[j+1])/(lambda[k]-lambda[j+1]+mu))-((lambda[k]-lambda[j])/(lambda[k]-lambda[j]+mu)))
								*exp((lambda[k]-lambda[j]+mu)*a[j+1])*B[k])
			}
	
		B[j] <- b[j]*(exp(-mu*a[j])-exp(-mu*a[j+1]))/mu
		
		for(k in (j+1):M) {
			B[j] <- B[j] + mu*(((exp((lambda[k]-lambda[j])*a[j+1])-exp((lambda[k]-lambda[j])*a[j]))/(lambda[k]-lambda[j]+mu))*B[k]) 
		}
		
		G[j] <- b[j]*(exp(-(mu-lambda[j])*a[j])-exp(-(mu-lambda[j])*a[j+1]))/(mu-lambda[j])
		
		for(k in (j+1):M) {
			G[j] <- G[j] + mu*((((lambda[k]-lambda[j])*(exp(lambda[k]*a[j+1])-exp(lambda[k]*a[j])))/((lambda[k]-lambda[j]+mu)*lambda[k]))*B[k])
		}
	}

	## calculating the expected value
	cMInverse <- 0

	for(j in 1:M) {
		cMInverse <- cMInverse + mu*B[j]/lambda[j] + G[j]
	}


	cM <- 1/cMInverse
	eTreated <- 0
  
	P=0
	for(j in 1:M) {
	  P = P + B[j]/lambda[j]*cM*mu
	}

	for(j in 1:M) {
		eTreated <- eTreated + lambda[j]*G[j]*(cM/(1-P))
	}

    variables <- data.frame(a=a, b=b, B=B, G=G)
    #print(variables)

	return(eTreated)
}

## initialize the java QueueModels framework
## library("rJava")
## .jinit(".")
##queueModelsClass <- .jnew("QueueModels")

## beta values from ages 0 to M (10 years)
#beta <- c(0.03151058, 0.2264334, 0.8135697, 1.8652304, 2.6427220, 2.9153079, 2.9816359, 2.9961008, 2.9991759, 2.9998260, 2.9999633)
#beta <- c(rep(0, 5), rep(0, 6))
beta <- c(0.2426025, 1.170722, 3.233667, 5.9878464, 8.9380404, 11.9286022, 14.9268377, 17.9265086, 20.9264473, 
 23.9264359)

## need to reassess these values but the code appears to be working!
alpha <- 150
c <- 0.10
p <- 0.20
mu <- 50

## expectedTreated1 <- ActualRate(beta=beta, alpha=alpha, c=c, p=p, mu=mu, javaClass=queueModelsClass)
expectedTreated2 <- ActualRate(beta=beta, alpha=alpha, c=c, p=p, mu=mu)
## print(expectedTreated1)
print(expectedTreated2)


d<-(1:10)
d<- d/10
print(d)
result<-c()
for (j in 1:10){

   expectedTreated2 <- ActualRate(beta=beta, alpha=alpha, c=d[j], p=p, mu=mu)

   result<-c(result, expectedTreated2)

}
print(result)
plot(result~d,xlab="Participation rate", ylab="Expected Number of Treated Houses",type="l")

