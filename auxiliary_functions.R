#####################################################
# Auxiliary functions for chapter 4 (Willekens)
# Just run the script to load the functions
# in Environment to later use in 
# chapter4_waiting_time.R file
#####################################################


#### Get hazard rates at durations in vector t: function h.pw ####
h.pw <- function(t, lower, upper, coeff){
  # Get the hazard rates at durations t
  lent <- length(t)
  h <- vector (mode="numeric",length=lent)
  for (i in 1:lent) {
    h[i] <- max(coeff * as.numeric (((t[i]-lower)>=0) & ((t[i]-upper)<=0)))
  }
  return(h)
}



#### Compute cumulative hazards at duration in vector t  ####
H.pw <- function(t, lower, upper, coeff){ 
  lent <- length(t)
  ch <- vector (mode="numeric",length=lent)
  for (i in 1:lent)	 
  { # Determine the intervals covered
    z <-  pmax((t[i]-lower), 0)
    # Number of intervals covered
    nn <- length(z[z>0])
    exposure <-  pmin(z, upper-lower)  
    # exposures= durations in each interval
    ch[i] <- sum(coeff*exposure) # rates * exposures
    # ch =  cumulative hazard = exposure * hazard rates
  }
  return(ch) 
}



####  Simulate lifespans under piecewise-cte exponential model ####


##### Method A  uniroot  #####

# Find event (failure) time associated with given survival probability  (H-inversion)
# Define a function of t for which the root must be determined 
pw.root <- function(t, low, up, coef, uu){
  aa <- H.pw(t, lower=low, upper=up, coeff=coef) + log(uu)
  # Cum hazard rate should be equal to - log(u), with u a value of survival function
  return (aa)
}


##### Get value t for which survival function is equal to a given value u ##### 
# try:  http://www.endmemo.com/r/try.php

r.pw_exp <- function(n, lower, upper,rates,u){
  times <- rep(NA, n)
  # uniroot searches the interval from lower to upper for a root (i.e., zero) of the function f with respect to its first argument.
  for(i in 1:n){
    out <- NULL
    try(out <- uniroot(f=pw.root,
                       interval=c(lower[1],upper[length(upper)]),  
                       low=lower,up=upper, 
                       rates=rates,
                       u=u[i]) )
    if (!is.null(out)) times[i] <- out$root else times[i] <- NA
  }
  aa <- list (u=u,times=times,NAs_count=length(which(is.na(times))),NAs_ID=which(is.na(times)))
  return(aa)
}



#####   Method B  msm  ##### 
# Output: survival distribution, given piecewise constant rates

require (msm)
rpexp(n=1,rates,breakpoints[-length(breakpoints)])

nsample <- 10000
x <- rpexp(n=nsample,rates,breakpoints[-length(breakpoints)])
hist (x,breaks=50,freq=FALSE,xlim=c(0,max(my.upper)),las=1, xlab="Duration",cex.axis=0.8,cex.main=0.8,main="")
curve (dpexp(x,rates,breakpoints[-length(breakpoints)]),add=TRUE,lwd=2,col="red")
box()
title(main="Frequency distribution of waiting times (times to event)")