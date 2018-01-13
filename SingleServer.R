## Brill's algorithm for a single server queue with a composite arrival stream
## Based on Algorithm writeup MTR 09/29/2014

## model parameters
## beta - vector of size M containing redispersion rates for each age levels
## a - vector of size M containing age levels
## alpha - rate of infection in attack zone
## c - participation rate
## p - prevalence of infection
## mu - service rate
ActualRate <- function(beta, a=0:(length(beta)-1), alpha, c, p, mu) {
	## initialization
  ## alphaPrime = residual (external) arrival rate
  ## lambda = vector of combined arrival rates (external + redispersion rate)
	alphaPrime <- alpha*(1-c)*p
	M <- length(beta)
	lambda <- beta + alphaPrime

	## a at 1 must be 0
	## note in R, the 1st index is the 0th index given by Brill's notation
	a[1] <- 0

	## Step [1]: initialize ending values of b, B, and M
	b <- rep(0, length(beta))
	b[M] <- 1

	B <- rep(0, length(beta))
	B[M] <- exp(-mu*a[M])/mu

	G <- rep(0, length(beta))
	G[M] <- exp(-(mu-lambda[M])*a[M])/(mu-lambda[M])

	## Step [2]: recurrence loop
	for(j in (M-1):1) {

		b[j] <- -mu*(lambda[j+1]-lambda[j])/(lambda[j+1]-lambda[j]+mu)*exp((lambda[j+1]-lambda[j]+mu)*a[j+1])*B[j+1]
				+exp((lambda[j+1]-lambda[j])*a[j+1])*b[j+1]

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

	## Step [3]: calculating the normalization constant
	cMInverse <- 0

	for(j in 1:M) {
		cMInverse <- cMInverse + mu*B[j]/lambda[j] + G[j]
	}


	cM <- 1/cMInverse
	expTreated <- 0

	for(j in 1:M) {
		expTreated <- expTreated + lambda[j]*G[j]*cM
	}

    variables <<- data.frame(a=a, b=b, B=B, G=G)
    print(variables)


	return(expTreated)
}

## Provide input to program:

## beta values from ages 0 to M; 
## note index in program runs 1 to M, where M=length(beta)
## our first example: 10 years; M=11
beta <- c(0.03151058, 0.2264334, 0.8135697, 1.8652304, 2.6427220, 2.9153079, 2.9816359, 2.9961008, 2.9991759, 2.9998260, 2.9999633)
## beta values from ages 0 to M (10 years)
## beta <- c(0.03151058, 0.2264334, 0.8135697, 1.8652304, 2.6427220, 2.9153079, 2.9816359, 2.9961008, 2.9991759, 2.9998260, 2.9999633, 2.9999922, 2.9999984,
          ## 2.9999997, 2.9999999, 3.0000000, 3.0000000, 3.0000000, 3.0000000, 3.0000000, 3.0000000)
##beta <- c(rep(0, 5), rep(1, 6))

alpha <- 150
##c <- 0.10
p <- 0.20
mu <- 20

d<-(1:9)
d<- d*0.1
print(d)
result<-c()
for (j in 1:9)
{
  
  expectedTreated2 <- ActualRate(beta=beta, alpha=alpha, c=d[j], p=p, mu=mu)
  
  result<-c(result, expectedTreated2)
  
}
print(result)

## Plot of expected number of houses per year as function of participation rate, c
variable.participation <- data.frame(c=d, result)
plot(variable.participation)
title("Participation Rate vs. Expected Number of Houses 
      per year")

## Plot of expected number of houses per year as function of service rate, mu
variable.service <- data.frame(mu, result)
plot(variable.service)
title("Service Rate vs. Expected Number of Houses 
      per year")




##Plot output of a versus b, B, G when using constant rates of p, mu, c, alpha
# Install and call ggplot2
# install.packages("ggplot2")
library(ggplot2)
library(grid)
source("http://peterhaschke.com/Code/multiplot.R")

## renames and set dataframe variable
variables.plot <- variables 

## Plot output of a versus b
P1 <- ggplot(variables.plot, aes(a, y = b, color = variable)) + geom_line(aes(y = b, col = "b"), 
            colour = "deepskyblue3") 

## Plot output of a versus B 
P2 <- ggplot(variables.plot, aes(a, y = B, color = variable)) + geom_line(aes(y = B, col = "B"),
            colour = "forestgreen")

## Plot output of a versus G
P3 <- ggplot(variables.plot, aes(a, y = G, color = variable)) + geom_line(aes(y = G, col = "G"),
            colour = "tomato3")

## Plot output of graphes onto one page
multiplot(P1, P2, P3, cols= 1)