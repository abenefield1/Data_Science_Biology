library(rstan)
library(rstanarm)
library(rethinking)

set.seed(4.6) #make example reproducible
n <- 30 #size of dataset
b0 <- 50
b1 <- 10
s <- 40
x <- runif(n,min=0,max=25)
y <- b0 + b1 * x + rnorm(n,sd=s)
plot(x,y)

lmod <- function(b0,b1,x){
  return( b0 + b1*x )
}

lmod(b0=300,b1=-9,x)

ysim <- function(mu,sigma) {
  return(rnorm(n=length(mu),mean=mu,sd=sigma))
}

par(mfrow=c(2,2),mar=c(5, 4, 0, 2) + 0.1)
ylim <- c(0,400)
for ( run in 1:4 ) {
  yout <- ysim( mu=lmod(b0=300,b1=-9,x), sigma=30 )
  plot(x,yout,ylim=ylim,ylab="y")
}

lm_nll <- function(p,y,x) {
  mu <- lmod(b0=p[1],b1=p[2],x) #call the linear model
  nll <- -sum(dnorm(y,mean=mu,sd=p[3],log=TRUE)) #-1 * sum of log-likelihoods 
  return(nll)
}


p <- c(70,8,30)
lm_nll(p,y,x)

fitlm <- optim(p=c(50,200/25,75/2),lm_nll,y=y,x=x)
fitlm

lm_slopefixed_nll <- function(p,b1,y,x) {
  mu <- lmod(b0=p[1],b1,x) #call the linear model
  nll <- -sum(dnorm(y,mean=mu,sd=p[2],log=TRUE)) #-1 * sum of log-likelihoods 
  return(nll)
}

fitlm_slopenull <- optim(p=c(200,75),lm_slopefixed_nll,b1=0,y=y,x=x)
fitlm_slopenull


exp(-fitlm$value)/exp(-fitlm_slopenull$value)
log(2998558063)/log(2)

nll_b1 <- rep(NA,50) #empty vector to hold nll for 50 different values of beta_1
b1_range <- seq(6,12,length.out=length(nll_b1)) #grid of 50 values from 6 to 12
par <- c(74,35)  #starting values for beta_0 and sigma (I used the MLE here)
i <- 1 #to index the rows of nll_b1
for ( b1 in b1_range ) {
  nll_b1[i] <- optim(p=par,lm_slopefixed_nll,b1=b1,y=y,x=x)$value #b1 is set on each loop
  i <- i + 1 #next row
}
likprof_b1 <- exp(-nll_b1)
likratio_b1 <- exp(fitlm$value-nll_b1) #likprof_b1 / exp(-fitlm$value)
# Plot the profiles
par(mfrow=c(1,3))
plot(b1_range,nll_b1,xlab=expression(beta[1]),ylab="Negative Log-Lik",col="#56B4E9")
plot(b1_range,likprof_b1,xlab=expression(beta[1]),ylab="Likelihood",col="#56B4E9")
plot(b1_range,likratio_b1,xlab=expression(beta[1]),ylab="Likelihood Ratio",col="#56B4E9")
abline(h=1/8,col="#E69F00")
text(8,1/8,"1/8",pos=3)
abline(h=1/32,col="#E69F00")
text(8,1/32,"1/32",pos=3)

source("source/likint.R")
par(mfrow=c(2,2),mar=c(5, 4, 0, 2) + 0.1 )
beta0_int <- likint( fitlm,profpar=1,lm_nll,plim=c(35,115),pname=expression(beta[0]),y=y,x=x )
beta1_int <- likint( fitlm,profpar=2,lm_nll,plim=c(6.5,11.5),pname=expression(beta[1]),y=y,x=x )
sigma_int <- likint( fitlm, profpar=3,lm_nll,plim=c(25,50),pname=expression(sigma),y=y,x=x )

likint( fitlm,profpar=2,lm_nll,plim=c(6,12),likratio=1/32,n=100,do_plot=FALSE,y=y,x=x )
confint(lm(y ~ x))


#dividing everything by max likelihood to rescale axis. Leads to lieklihood ratio
