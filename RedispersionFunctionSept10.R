## Task: Plot B[j] as a function of virtual wait only 
## Result: estimate for rate of redispersion 

# alpha - rate of infection in attack zone
# c - participation rate
# p - prevalence of infection
alpha <- 27
c <- 0.10
p <- 0.20 

##residual infestation rate
alphaPrime <- alpha*(1-c)*p

## Integrate the infestivity function as a function of t (z)
#Logistic (infestivity) function
infestivityAPrime <- function(k,y0,r,t,alphaPrime) {
  out <- k/(1+((k-y0)/y0)*exp(-r*t))
  outAP <- (out * alphaPrime)
  return(outAP)
}

#set parameters of infestivty function 
k <- 3 # carrying capacity 3 infestations per house per year
y0 <- 0.1 # initial infestivity (0.1 infestations per house per year)
r <- 1.68

#Set time interval - ten years 
t <- 1:1000/100

#integrate infestivity function 
upperCaseGammaAP <- function(t) {
  integrate(infestivityAPrime,lower=0,upper=t, k=k, y0=y0, r=r, alphaPrime=alphaPrime)$value
}
# vectorize results of integration function - input t in function 
v.upperCaseGammaAP <- Vectorize(upperCaseGammaAP)

## Plot the result of the upperCaseGammaAP (Eq. 4 from paper)
plot(v.upperCaseGammaAP(t)~t,xlab="Time Within Queue (Years)", ylab="Rate of Redispersion",type="l")
title("Redispersion Rate (Eq. 4)")

## Redispersion rate disregarding alphaPrime (Eq. 5 from paper)
#Logistic (infestivity) function
infestivity <- function(k,y0,r,t) {
  out <- k/(1+((k-y0)/y0)*exp(-r*t))
  return(out)
}

#integrate infestivity function 
upperCaseGamma <- function(t) {
  integrate(infestivity,lower=0,upper=t, k=k, y0=y0, r=r)$value
}
# vectorize results of integration function - input t in function 
v.upperCaseGamma <- Vectorize(upperCaseGamma)

## Plot the result of the upperCaseGamma (Eq. 5 from paper)
plot(v.upperCaseGamma(t)~t,xlab="Time Within Queue (Years)", ylab="Rate of Redispersion",type="l")
title("Redispersion Rate (Eq. 5")

par(mfrow=c(1,2))
plot(v.upperCaseGammaAP(t)~t,xlab="Time Within Queue (Years)", ylab="Rate of Redispersion",type="l")
plot(v.upperCaseGamma(t)~t,xlab="Time Within Queue (Years)", ylab="Rate of Redispersion",type="l")





 
