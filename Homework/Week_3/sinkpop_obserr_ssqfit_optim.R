# This program fits the sink model to data with observation error by optimizing
# the minimum sum of squares.
# Brett Melbourne
# 29 Oct 17
#
# Model fitting has three components:
# 1) A function for the biological model.
# 2) A function to calculate SSQ.
# 3) The call to optim.
#
# The main difference for the observation error fit is that the biological model
# is now deterministic through time, and we have to estimate the initial number
# of individuals.


#----Function definitions----------------------------------------

#----sink_ts() ------------------------------
# Returns the time series of the sink model
#    N[t+1] = i + sN[t]
# N_init: Initial population size
# i:      Number of immigrants
# s:      Survival probability
# maxt:   Calculated time points will be 0:maxt
#
sink_ts <- function( N_init, i, s, maxt ) {

#	Initialize population vector
    N <- rep(NA,maxt+1)
    N[1] <- N_init

#	Iterate the model through time
    for ( t in 1:maxt ) {
        N[t+1] <- i + s * N[t]
    }
    return(N)
}


#----ss_sink_obserr() -----------------------------
# Returns the sum of squares for the sink model with
# observation error. This is set up for use with
# optim.
# p:    Vector of initial values for N_init,i,s
# Nobs: The data, time series of observed abundances
# maxt: Passed to sink_ts().
#
ssq_sink_obserr <- function( p, Nobs, maxt ) {
    N <- sink_ts(N_init=p[1],i=p[2],s=p[3],maxt)
    d <- Nobs - N
    ssq <- sum(d^2)
    return(ssq)
}


#----Main Program----------------------------------------------

#----First plot the data

sinkdata <- read.csv("sinkdata.csv")    #Data from D2L
time <- sinkdata$t
Nobs <- sinkdata$Nt

# We have an extra datapoint also from the Ntp1 column, so add that
time <- c(time,100)
Nobs <- c(Nobs,sinkdata$Ntp1[length(sinkdata$Ntp1)])

#windows()
plot(time,Nobs,xlab="Years",ylab="N")
lines(time,Nobs)


#----Fit the model to the data by using optim()

# Initialize parameters: ****TRY DIFFERENT STARTING VALUES TOO****
maxt <- 100    #Maximum time for the time series
N_init <- 513
i <- 82
s <- 0.89

# Optimization by Nelder-Mead: finding the minimum SSQ
par <- c(N_init,i,s)  #Put the starting parameters in a vector
fit <- optim( par, ssq_sink_obserr, Nobs=Nobs, maxt=maxt )
fit   #Note: check convergence code = 0.

# Calculate fitted model dynamics for best parameter values.
# Also called "fitted values".
N <- sink_ts(N_init=fit$par[1],i=fit$par[2],s=fit$par[3],maxt)

# Plot fitted model over the data (with deviations if you like).
# The model represents "truth", so deviations represent measurement error.
plot(time,Nobs,xlab="Years",ylab="N",main="Observation error fit of sink model")
lines(time,N,col="red")
segments(time,N,time,Nobs,col="green")
