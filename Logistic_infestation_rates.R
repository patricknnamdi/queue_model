#logistic function
#input: t (in years)
#output: infestivity (infestations per house per year)
getgr<-function(k,y0,r,t) {
	out<-k/(1+((k-y0)/y0)*exp(-r*t))
	return(out)
}

k<-3 # carrying capacity 3 infestations per house per year
y0<-0.1 # initial infestivity (0.1 infestations per house per year)

r<-1.68
integrate(getgr,lower=0,upper=2, k=k, y0=y0, r=r)

groptim<-function(r,k,y0,estimate,lowerbound,upperbound) {
	integrategr<-integrate(getgr,lower=lowerbound,upper=upperbound,k=k,y0=y0,r=r)
	integrategr<-as.numeric(integrategr[1])
	error<-(integrategr-estimate)*(integrategr-estimate)
	return(error)
}

#this is from Corentin and Karthiks paper, though is estimated assuming constant rate of migration. Were working on relaxing that assumption.

estimate<-1.04  
roptim<-optim(1.68, fn=groptim, k=k, y0=y0, estimate=estimate, lowerbound=0, upperbound=2)
r<-roptim$par

#test out values in a plot
t<-1:1000/100
PRED<-getgr(k,y0,r,t)
plot(PRED~t,xlab="Time Since Infestation (Years)", ylab="Expected Infestivity (Additional Infestations Per House Per Year)",type="l")

#bin by ranges of integration
tmax<-10
bin<-c(1:tmax)
for(i in 1:tmax){
	temp<-integrate(getgr,lower=i-1,upper=i, k=k, y0=y0, r=r)
	bin[i]<-temp[[1]]/1 #the division by one is mathematically redundant but gives us infestivity again
	}
barplot(bin, add=F)

#plot the function and bins
par(mfrow=c(1,2))
plot(PRED~t,xlab="Time Since Infestation (Years)", ylab="Expected Infestivity (Additional Infestations Per House Per Year)",type="l")
barplot(bin, names.arg =1:10,xlab="Time Since Infestation (Years)", ylab="Average Infestivity (Additional Infestations Per House Per Year)", add=F)