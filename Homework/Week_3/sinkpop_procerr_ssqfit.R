# This program fits the sink model to data with process error using a grid 
# search algorithm. The criterion for model fit is the sum of squares.
# Brett Melbourne
# 18 Oct 2017


#----Read in the data

# Data columns are:
# t - time
# Nt - number at time t
# Ntp1 - number at time t+1

# Read from file
sinkdata <- read.csv("sinkdata.csv")

# Copy data to vectors
time <- sinkdata$t
Nt <- sinkdata$Nt
Ntp1 <- sinkdata$Ntp1

#----Plot the data

# Temporal view
plot(time,Nt,xlab="Years",ylab="N")
lines(time,Nt)

# Linear view - we'll use this to fit the model
xlb <- expression( italic(N)[italic(t)] )    #Make nice axis labels
ylb <- expression( italic(N)[italic(t)+1] )  #See ?plotmath
plot(Nt,Ntp1,xlab=xlb,ylab=ylb)


#----Fit the model to the data by grid search

# Initialize parameters
i_range <- seq(150,700,length.out=200)
s_range <- seq(0,1,length.out=200)

# Initialize storage objects (matrix, vector, and scalar)
n <- length(i_range)*length(s_range)
results <- matrix(NA,n,3)  #columns: i, s, ssq
colnames(results) <- c("i","s","ssq")
p_best <- NA
ssq_best <- Inf

# Grid search over the two parameters: i and s
j <- 1 #Row index for results matrix
for ( i in i_range ) {
    for ( s in s_range ) {

    #   Calculate model predictions
        Ntp1_pred <- i + s*Nt

    #   Calculate deviations and sum of squares
        d <- Ntp1 - Ntp1_pred
        ssq <- sum(d^2)

    #   Keep the results
        results[j,] <- c(i,s,ssq)
        j <- j + 1

    #   Record current best solution
        if ( ssq < ssq_best ) {
            ssq_best <- ssq
            p_best <- cbind(i,s) #cbind automatically gives labels
        }
    }
#   Monitor progress
    print( paste(round(100*j/n),"%",sep=""), quote=FALSE )
}


# Print the minimum sum of squares and best parameter values
ssq_best
p_best

# The solution above integrates finding the best fit within the nested for
# loops. You could also do this afterward quite simply, either by using a for
# loop to find the minimum sum of squares, or using the specialized R function
# which.min().

# The for loop approach:
ssq_best2 <- Inf
for ( row in 1:nrow(results) ) {
    if ( results[row,3] < ssq_best2 ) {
        ssq_best2 <- results[row,3]
        ssqbestrow <- row
    }
}
results[ssqbestrow,] #We found the same best fit as above

# The whichmin approach:
ssqbestrow <- which.min(results[,3])
results[ssqbestrow,] #We also find the same best fit as above


#----Plot sum of squares profiles

# Basic plot - what is going on?
plot(results[,"i"],results[,"ssq"],main="SSQ profile for i")

# Nicer plot with a scaling control for zooming
windows(width=10,height=5)  #Mac: quartz(width=10,height=5)
par(mfrow=c(1,2))
scale <- 1.2  #This is a zoom setting. Must be > 1. Smaller is zooming in.
plot(results[,1],results[,3],xlab="i",ylab="Sum of squares",
     ylim=c(ssq_best,scale*ssq_best),col="blue")
plot(results[,2],results[,3],xlab="s",ylab="Sum of squares",
     ylim=c(ssq_best,scale*ssq_best),col="blue")
mtext("Sum of squares profiles - grid search",outer=TRUE,line=-2.5)


#---Plot the fitted model with the data
windows() #Mac: quartz()
plot(Nt,Ntp1,xlab=xlb,ylab=ylb)
Ntp1_pred <- p_best[1] + p_best[2] * Nt  #i=p_best[1], s=p_best[2]
points(Nt,Ntp1_pred,col="red")
abline(p_best[1],p_best[2],col="red")
segments(Nt,Ntp1_pred,Nt,Ntp1,col="green") #deviations if you want to be fancy

#Or plot the fitted model on the time scale
windows()
plot(time,Nt,xlab="Years",ylab="N")
points(time+1,Ntp1_pred,col="red")
lines(time+1,Ntp1_pred,col="red")
segments(time+1,Ntp1_pred,time+1,Ntp1,col="green") #deviations
