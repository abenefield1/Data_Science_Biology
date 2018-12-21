# Optim for basic linear regression

#nectar.ul
y <- c(160.44, 41.85, 238.72, 383.67, 84.53, 360.34, 285.57, 267.2, 79.92, 201.09, 189.23, 200.1, 248.79, 378.07, 209.76, 206.58, 66.49, 263.33, 0.19, 191.95, 28.81, 246.87, 297.63, 252.93, 242.9, 393.72, 322.86, 256.12, 300.55, 281.77, 208.44, 200.32, 205.19, 51.89, 336.75, 34.77, 161.78, 96.41, 134.38, 236.24, 73.64, 166.8, 255.32, 185.87, 35.18, 135.43, 124.7, 126.5, 212.36, 126.93, 192.28, 213.94, 281.01, 158.78, 242.59, 237.37, 112.79, 242.23, 119.35, 193.43, 224.37, 184.48, 296.33, 10.63)

#lesions
x <- c(51, 25, 64, 98, 24, 79, 67, 75, 40, 36, 77, 69, 88, 87, 62, 70, 5, 42, 16, 50, 24, 33, 99, 89, 46, 77, 100, 46, 70, 67, 66, 70, 9, 39, 74, 8, 37, 53, 11, 34, 20, 48, 76, 51, 24, 65, 23, 32, 74, 23, 58, 78, 98, 41, 86, 80, 13, 97, 28, 40, 36, 53, 43, 43)

sink_y <- function(b,m,x) {
  y <- b + m * x
  return(y)}

#----ssq_procerr() ----------------
# Returns the sum of squares for the  model with process error. This is set up for use with optim.
# p:     Vector of initial values for b and m
# y:  Vector of data for y
# x:    Vector data for x

ssq_sink_procerr <- function(p,y,x) {
  #In the next line we refer to the parameters in p by name so that the code
  #is self documenting
  y_pred <- sink_y(b=p[1],m=p[2],x)
  d <- y - y_pred
  ssq <- sum(d^2)
  return(ssq)}

# Linear view - we'll use this to fit the model
plot.new()
plot(x,y,pch=19, main="Number of Lesions vs. Nectar Volume",xlab="Lesions",ylab="Nectar Volume (microliters)")

# Initialize parameters: ****TRY DIFFERENT STARTING VALUES TOO****
b <- 50
m <- 2

# Optimization: finding the minimum SSQ
par <- c(b,m)  #Put the starting parameters in a vector
fit <- optim( par, ssq_sink_procerr, y=y, x=x )
fit   #Note: check convergence code = 0.

# Calculate fitted model dynamics for best parameter values.
# Also called "fitted values".
y_pred <- sink_y( b=fit$par[1], m=fit$par[2], x )

# Plot fitted model over the data
points(x,y_pred,col="red")
abline(fit$par[1],fit$par[2],col="red")
segments(x,y_pred,x,y,col="green") #deviations if you want to be fancy

lmfit<-lm(y~x)
lmfit
anova(lmfit)
plot.new()
plot(x,y)
abline(lmfit,col="red")
