# This program fits the sink model to data with process error by optimizing the 
# minimum sum of squares.
# Brett Melbourne
# 25 Oct 2017
#
# Model fitting has three components:
# 1) A function for the biological model.
# 2) A function to calculate SSQ.
# 3) The call to optim.


#----Function definitions------------------------------------------------------

#----sink_Ntp1()-----------------------
# A function for the biological model.
# Returns a vector of N[t+1] for the sink model.
#    N[t+1] = b + sN[t]
# b:   Number of immigrants
# m:   Survival probability
# x:  N[t], number of individuals at time t (vector)
#
sink_Ntp1 <- function(b,m,x) {
    Ntp1 <- b + m * x
    return(Ntp1)
}


#----ss_sink_procerr() ----------------
# Returns the sum of squares for the sink model with process error. This is set
# up for use with optim.
# p:     Vector of initial values for b and m
# Ntp1:  Vector of data for N[t+1]
# x:    Vector data for N[t]
#
ssq_sink_procerr <- function(p,Ntp1,x) {
    #In the next line we refer to the parameters in p by name so that the code
    #is self documenting
    Ntp1_pred <- sink_Ntp1(b=p[1],m=p[2],x)
    d <- Ntp1 - Ntp1_pred
    ssq <- sum(d^2)
    return(ssq)
}


#----Main Program--------------------------------------------------------------


#----Read in the data

# Data columns are:
# t - time
# x - number at time t
# Ntp1 - number at time t+1

sinkdata <- read.csv("sinkdata.csv") #Data from D2L
time <- sinkdata$t
x <- sinkdata$x
Ntp1 <- sinkdata$Ntp1


#----Plot the data

# Linear view - we'll use this to fit the model
windows() #mac: quartz()
xlb <- expression(italic(N)[italic(t)])    #Make nice axis labels
ylb <- expression(italic(N)[italic(t)+1])
plot(x,Ntp1,xlab=xlb,ylab=ylb)


#----Fit the model to the data by Nelder-Mead descent method
# We have already looked at the SSQ surface using grid search. See 
# sinkpop_procerr_ssqfit.r. The grid search helps to find reasonable starting
# values for the parameters.

# Initialize parameters: ****TRY DIFFERENT STARTING VALUES TOO****
b <- 82
m <- 0.89

# Optimization: finding the minimum SSQ
par <- c(b,m)  #Put the starting parameters in a vector
fit <- optim( par, ssq_sink_procerr, Ntp1=Ntp1, x=x )
fit   #Note: check convergence code = 0.

# Calculate fitted model dynamics for best parameter values.
# Also called "fitted values".
Ntp1_pred <- sink_Ntp1( b=fit$par[1], m=fit$par[2], x )

# Plot fitted model over the data
points(x,Ntp1_pred,col="red")
abline(fit$par[1],fit$par[2],col="red")
segments(x,Ntp1_pred,x,Ntp1,col="green") #deviations if you want to be fancy

# We already know that we've reinvented linear regression. Here's how it is done
# with R's linear regression tool.
lmfit <- lm(Ntp1~x)
lmfit #Estimated coefficients are the same
anova(lmfit) #See Sum Sq for Residuals (same as our min ssq)
windows() #mac: quartz()
plot(x,Ntp1)
abline(lmfit,col="red")
