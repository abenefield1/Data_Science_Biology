
######################################################################################################
# 4H1:
######################################################################################################
#Prediction Intervals:
library(rethinking)

sampost<-function( flist, data, n=10000 ) {
  quadapprox<-map(flist,data) 
  posterior_sample<-extract.samples(quadapprox,n) 
  return(posterior_sample)}


data(Howell1)
d2 <- Howell1
# Linear model makes most sense with only ages above 18
d <- d2[ d2$age >= 18 , ]

N <- length(d[,1])
dN <- d[ 1:N , ]
mN <- sampost(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=dN )

###REVISIT

#mean(mN$a + mN$b * 46.95)
w <- c(46.95, 43.72, 64.78, 32.59, 54.63) #Individual heights to make predictions about
n <- length(w)
pred_hpdi_m <- matrix(NA,nrow=n,ncol=2) #matrix to store hpdi values
colnames(pred_hpdi_m) <- c("low89","high89")
for ( i in 1:n ) {
  mu <- mN$a + mN$b * w[i] #the posterior sample of mu at weight w
  newdat <- rnorm(n=length(mu),mu,sd=mN$sigma )
  pred_hpdi_m[i,] <- HPDI( newdat, prob=0.89 ) #hpdi of the sample
}

# 89% HPDIs
pred_hpdi_m
pred_height<-(pred_hpdi_m[,1] + pred_hpdi_m[,2])/2
pred_height



plot( height ~ weight, data=d, col = "blue", xlim=c(30, 65), ylim=c(135, 185))
points(w,pred_height, col="red", pch=19)
arrows(w, pred_height,w,pred_hpdi_m, length=0.1, angle=90)
abline( a=mean(mN[,"a"]) , b=mean(mN[,"b"]) )
lines(w,pred_hpdi_m[,"low89"],col="grey")
lines(w,pred_hpdi_m[,"high89"],col="grey")


######################################################################################################
# 4H2
######################################################################################################
#a.
d <- d2[ d2$age <18 , ]
N <- length(d[,1])
dN <- d[ 1:N , ]
mN <- sampost(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=dN )

precis(mN)
# So, the intercept is 58.25 cm. The slope is 2.72  So, for every 10 kg of weight, height increases by 27.2 cm.

# b. : Plotting
# Credible intervals
w <- seq( from=0, to=50, by=1 ) #or 25:70
n <- length(w)
hpdi_m <- matrix(NA,nrow=n,ncol=2) #matrix to store hpdi values
colnames(hpdi_m) <- c("low89","high89") #optional but nice
for ( i in 1:n ) {
  mu <- mN$a + mN$b * w[i] #the posterior sample of mu at weight w
  hpdi_m[i,] <- HPDI( mu, prob=0.89 ) #hpdi of the sample
}
hpdi_m

# Prediction Intervals
w <- seq( from=0, to=50, by=1 ) #or 25:70
n <- length(w)
pred_hpdi_m <- matrix(NA,nrow=n,ncol=2) #matrix to store hpdi values
colnames(pred_hpdi_m) <- c("low89","high89")
for ( i in 1:n ) {
  mu <- mN$a + mN$b * w[i] #the posterior sample of mu at weight w
  newdat <- rnorm(n=length(mu),mu,sd=mN$sigma )
  pred_hpdi_m[i,] <- HPDI( newdat, prob=0.89 ) #hpdi of the sample
}

plot(height ~ weight, data=d, col = "blue")#, xlim=c(0, 50))
abline( a=mean(mN[,"a"]) , b=mean(mN[,"b"]) )
lines(w,hpdi_m[,"low89"],col="grey")
lines(w,hpdi_m[,"high89"],col="grey")
lines(w,pred_hpdi_m[,"low89"],col="grey")
lines(w,pred_hpdi_m[,"high89"],col="grey")
grid(col="black")

# c. The data doesn't look linear. A log regression.


######################################################################################################
# 4H3
######################################################################################################
plot( height ~ log(weight) , data=Howell1 , col=col.alpha(rangi2,0.4) )

d <- Howell1
N <- length(d[,1])
dN <- d[ 1:N , ]
mN <- sampost(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*log(weight) ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=dN )

precis(mN)


# b. : Plotting
# Credible intervals
w <- seq( from=0, to=50, by=1 ) #or 25:70
n <- length(w)
hpdi_m <- matrix(NA,nrow=n,ncol=2) #matrix to store hpdi values
colnames(hpdi_m) <- c("low97","high97") #optional but nice
for ( i in 1:n ) {
  mu <- mN$a + mN$b * w[i] #the posterior sample of mu at weight w
  hpdi_m[i,] <- HPDI( mu, prob=0.97 ) #hpdi of the sample
}
hpdi_m

# Prediction Intervals
w <- seq( from=0, to=50, by=1 ) #or 25:70
n <- length(w)
pred_hpdi_m <- matrix(NA,nrow=n,ncol=2) #matrix to store hpdi values
colnames(pred_hpdi_m) <- c("low97","high97")
for ( i in 1:n ) {
  mu <- mN$a + mN$b * w[i] #the posterior sample of mu at weight w
  newdat <- rnorm(n=length(mu),mu,sd=mN$sigma )
  pred_hpdi_m[i,] <- HPDI( newdat, prob=0.97 ) #hpdi of the sample
}

plot( height ~ log(weight) , data=Howell1 , col=col.alpha(rangi2,0.4) )
abline( a=mean(mN[,"a"]) , b=mean(mN[,"b"]) )
lines(w,hpdi_m[,"low97"],col="lightgreen")
lines(w,hpdi_m[,"high97"],col="lightgreen")
lines(w,pred_hpdi_m[,"low97"],col="grey")
lines(w,pred_hpdi_m[,"high97"],col="grey")
grid(col="darkgrey")

precis(mN)
#For each 1% increase in weight, there is an increase in height of 0.47 cm