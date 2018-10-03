# y
nectar_vol <- c(160.44, 41.85, 238.72, 383.67, 84.53, 360.34, 285.57, 267.2, 79.92, 201.09, 189.23, 200.1, 248.79, 378.07, 209.76, 206.58, 66.49, 263.33, 0.19, 191.95, 28.81, 246.87, 297.63, 252.93, 242.9, 393.72, 322.86, 256.12, 300.55, 281.77, 208.44, 200.32, 205.19, 51.89, 336.75, 34.77, 161.78, 96.41, 134.38, 236.24, 73.64, 166.8, 255.32, 185.87, 35.18, 135.43, 124.7, 126.5, 212.36, 126.93, 192.28, 213.94, 281.01, 158.78, 242.59, 237.37, 112.79, 242.23, 119.35, 193.43, 224.37, 184.48, 296.33, 10.63)

#x
lesions <- c(51, 25, 64, 98, 24, 79, 67, 75, 40, 36, 77, 69, 88, 87, 62, 70, 5, 42, 16, 50, 24, 33, 99, 89, 46, 77, 100, 46, 70, 67, 66, 70, 9, 39, 74, 8, 37, 53, 11, 34, 20, 48, 76, 51, 24, 65, 23, 32, 74, 23, 58, 78, 98, 41, 86, 80, 13, 97, 28, 40, 36, 53, 43, 43)


############################################################################################
##LM to fit a linear reg
############################################################################################
# lm(y~x)
LesionVsNectar_lm<-lm(nectar_vol~lesions) # linear regression using lm

summary(LesionVsNectar_lm)
# Lesions are significant (p-value = 6.86e-12 ***) R-squared medium (0.5271). The number of lesions predict nectar volume. As lesions increase by one, nectar volume increases by 2.651 microliters. Also, nectar volume increases as the number of lesions increase:

plot(LesionVsNectar_lm)
plot(lesions,nectar_vol,pch=19, main="Number of Lesions vs. Nectar Volume",xlab="Lesions",ylab="Nectar Volume (microliters)")
abline(LesionVsNectar_lm)

pred.frame<-data.frame(lesions=0:100)
pred.c<-predict(LesionVsNectar_lm,int="confidence",newdata=pred.frame)
LesionVsNectar_lm$fitted.values
matlines(pred.frame,pred.c,col=c("black","red","red"),lwd=3,lty=c(1,2,2))

confint(LesionVsNectar_lm)

A<-anova(LesionVsNectar_lm)
summary(A)
pval = pt(8.439, df=(length(LesionVsNectar_lm)-1), lower.tail=FALSE) ##not multiplied by two, because only interested in upper-tail of t-test.
pval

# If want to generate 95% confidence interval around the fitted values of a plant with specified # of lesions (62):
pred.frame1<-data.frame(lesions=62)#
pred.p<-predict(LesionVsNectar_lm,int="prediction",newdata=pred.frame1)# p is prediction PREDICTED INDIVIDUALS
pred.p
abline(v=62)
abline(h=pred.p,col=c("black","red","red"),lty=c(1,2,2))


############################################################################################
## Use optim to fit a linear regression model 
############################################################################################
#nectar.ul
y <- c(160.44, 41.85, 238.72, 383.67, 84.53, 360.34, 285.57, 267.2, 79.92, 201.09, 189.23, 200.1, 248.79, 378.07, 209.76, 206.58, 66.49, 263.33, 0.19, 191.95, 28.81, 246.87, 297.63, 252.93, 242.9, 393.72, 322.86, 256.12, 300.55, 281.77, 208.44, 200.32, 205.19, 51.89, 336.75, 34.77, 161.78, 96.41, 134.38, 236.24, 73.64, 166.8, 255.32, 185.87, 35.18, 135.43, 124.7, 126.5, 212.36, 126.93, 192.28, 213.94, 281.01, 158.78, 242.59, 237.37, 112.79, 242.23, 119.35, 193.43, 224.37, 184.48, 296.33, 10.63)

#lesions
x <- c(51, 25, 64, 98, 24, 79, 67, 75, 40, 36, 77, 69, 88, 87, 62, 70, 5, 42, 16, 50, 24, 33, 99, 89, 46, 77, 100, 46, 70, 67, 66, 70, 9, 39, 74, 8, 37, 53, 11, 34, 20, 48, 76, 51, 24, 65, 23, 32, 74, 23, 58, 78, 98, 41, 86, 80, 13, 97, 28, 40, 36, 53, 43, 43)

sink_y <- function(b,m,x) {
  y <- b + m * x
  return(y)}

#----ssq_procerr() ----------------
# Returns the sum of squares for the  model with process error. This is set up for use with optim.
# p:     Vector of initial values for i and s
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


## NLL Function
# Returns the negative log likelihood for the model with observation and process error from a normal distribution. 
# Definitions:
# p: Vector of initial values for beta_0, beta_1, sd #b0 = intercept
#b1 = slope
# x: The data, number of lesions
# y: The data, nectar volume

lmod <- function(b0,b1,x){
  return( b0 + b1*x )
}
#changed mu to y_pred
lm_nll <- function(p,y,x) {
  y_pred <- lmod(b0=p[1],b1=p[2],x) #call the linear model
  nll <- -sum(dnorm(y,mean=y_pred,sd=p[3],log=TRUE)) #-1 * sum of log-likelihoods 
  return(nll)
}
# p<-(b0, b1, sd)
# For sd, a good estimate is half the range of the data through any slice of x, so: 
250/2

p<-c(50,2,125)
lm_nll(p,y,x)

fitlm<-optim(p, lm_nll,y=y,x=x)
fitlm
# So, optim converged (=0), parameter estimates are: 54.16 for intercept (b0), 2.65 for slope (b1), and 64.25 for sigma (std error)
# For later convienence:
parameters<-data.frame("Intercept_b0"=fitlm$par[1], "Slope_b1"=fitlm$par[2], "Standard_deviation_sigma"=fitlm$par[3])
parameters

# For inference algorithm to compare likelihood ratios, we need at least two models. So, we'll code one that holds the slope, b1 fixed: NULL MODEL
lm_slopefixed_nll <- function(p,b1,y,x) {
  y_pred <- lmod(b0=p[1],b1,x) #call the linear model
  nll <- -sum(dnorm(y,mean=y_pred,sd=p[2],log=TRUE)) #-1 * sum of log-likelihoods 
  return(nll)
}

# b1, intercept estimate = 50, sd estimate = 125
fitlm_slopenull <- optim(p=c(50,125),lm_slopefixed_nll,b1=0,y=y,x=x)
fitlm_slopenull

#The likelihood ratio for the full maximum likelihood model versus the null slope model is
exp(-fitlm$value)/exp(-fitlm_slopenull$value)
# Thus, slope of 2.65 is 42,671,453,677 is almost 43 billion times more likely than a slope of zero.


### So, testing this against multiple values of slope parameter:
nll_b1 <- rep(NA,50) #empty vector to hold nll for 50 different values of beta_1
b1_range <- seq(1.5,3.5,length.out=length(nll_b1)) #grid of 50 values from 6 to 12
par <- c(54,64)  #starting values for beta_0 and sigma (I used the MLE here)
i <- 1 #to index the rows of nll_b1
for ( b1 in b1_range ) {
  nll_b1[i] <- optim(p=par,lm_slopefixed_nll,b1=b1,y=y,x=x)$value #b1 is set on each loop
  i <- i + 1 #next row
}
likprof_b1 <- exp(-nll_b1)
likratio_b1 <- exp(fitlm$value-nll_b1) #likprof_b1 / exp(-fitlm$value)
# Plot the profiles
plot.new()
par(mfrow=c(1,3))
plot(b1_range,nll_b1,xlab=expression(beta[1]),ylab="Negative Log-Lik",col="#56B4E9")
plot(b1_range,likprof_b1,xlab=expression(beta[1]),ylab="Likelihood",col="#56B4E9")
plot(b1_range,likratio_b1,xlab=expression(beta[1]),ylab="Likelihood Ratio",col="#56B4E9")
abline(h=1/8,col="#E69F00")
text(2.25,1/8,"1/8",pos=3)
abline(h=1/32,col="#E69F00")
text(2.25,1/32,"1/32",pos=3)
df<-data.frame(b1_range,likratio_b1)
points(df$b1_range[which.max(df$likratio_b1)],max(likratio_b1), col="red")
abline(v=df$b1_range[which.max(df$likratio_b1)], col="red", lty=2)
text(2.25,0.99,round(df$b1_range[which.max(df$likratio_b1)],digits=3),col="red")

#Slope
MLE_b1<-df$b1_range[which.max(df$likratio_b1)]
MLE_b1

parameters
plot.new()
source("source/likint.R")
par(mfrow=c(2,2),mar=c(5, 4, 0, 2) + 0.1 )
beta0_int <- likint( fitlm,profpar=1,lm_nll,plim=c(4,104),pname=expression(beta[0]),y=y,x=x )
beta1_int <- likint( fitlm,profpar=2,lm_nll,plim=c(MLE*0.625,MLE*1.375),pname=expression(beta[1]),y=y,x=x )
sigma_int <- likint( fitlm, profpar=3,lm_nll,plim=c(parameters[,3]-20,parameters[,3]+20),pname=expression(sigma),y=y,x=x )

# Estimates for slope b1
likint( fitlm,profpar=2,lm_nll,plim=c(MLE*0.625,MLE*1.375),likratio=1/32,n=100,do_plot=FALSE,y=y,x=x )

