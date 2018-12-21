#' ---
#' output: github_document
#' ---
library(lme4)
library(rstanarm)
options(mc.cores = parallel::detectCores())

# Parameters for simulated data
vy <- 2.22   #Variance of y, \sigma_y^2 sigma 
va <- 2.59   #Variance at group level, \sigma_{\alpha}^2: sigma_{\s}^2: Groups = states: Sigma[State_Name:(Intercept),(Intercept)]
mubar <- 1.09e+01 #Overall mean among groups, \bar{\mu}
ng <- 49    #Number of groups in data: 50 or 4? States
n <- 4     #Number of data points within a group

# Simulate group level (vectorized)
muj <- rnorm(ng,mubar,sqrt(va)) #Simulate mu_j (one per group)

# Simulate data level (vectorized)    
muj_v <- rep(muj,each=n)        #Expand groups to vector format
#y
log_PA_ha<- rnorm(n*ng,muj_v,sqrt(vy)) #Simulate y depending on group means

# Compile into a dataframe
dat <- data.frame(y=log_PA_ha,group=factor(rep(1:ng,each=n)))
head(dat)

baysfit <- stan_lmer(y ~ 1 + (1|group), data=dat)

summary(baysfit)[c(1:6,51,52,53),c(1,3,10,9)] #a sample of the full output
cbind(intercept=mubar,sigma=sqrt(vy),Sigma_group=va)


# Setup: Commented Out to Avoid a Lengthy Knit, but output file is on GitHub
# reps <- 100 #Number of replicate simulations
# keep <- matrix(NA,nrow=reps,ncol=6)
# 
# system.time(
#   for ( i in 1:reps ) {
#     
#     #   Simulate group level
#     muj <- rnorm(ng,mubar,sqrt(va)) #Simulate mu_j (one per group)
#     
#     #   Simulate data level (vectorized)    
#     muj_v <- rep(muj,each=n)        #Expand groups to vector format
#     y <- rnorm(n*ng,muj_v,sqrt(vy)) #Simulate y
#     
#     #   Fit model to simulated data
#     dat <- data.frame(y=y,group=factor(rep(1:ng,each=n)))
#     mlfit <- lmer(y ~ 1 + (1|group), data=dat, REML=FALSE)
#     baysfit <- stan_lmer(y ~ 1 + (1|group), data=dat)
#     
#     #   Record results (VarCorr extracts the variance estimates)
#     keep[i,] <- c(fixef(mlfit), as.data.frame(VarCorr(mlfit))[,4],
#                   fixef(baysfit), as.data.frame(VarCorr(baysfit))[,4] )
#     
#     #   Tidy up
#     rm(muj,muj_v,y,dat,mlfit,baysfit)
#     
#     #   Monitoring progress   
#     if ( i %% 10 == 0 ) {
#       print(i) 
#     }
#   }
# )
# save(keep,file="sim_output.RData")

load(file="sim_output.RData")
head(keep)

ml_mubar <- mean(keep[,1])
ml_va <- mean(keep[,2])
ml_vy <- mean(keep[,3])
b_mubar <- mean(keep[,4])
b_va <- mean(keep[,5])
b_vy <- mean(keep[,6])

cbind(mubar,ml_mubar,b_mubar)
cbind(vy,ml_vy,b_vy)
cbind(va,ml_va,b_va)
# Parameter estimates have mostly converged to the "true" parameters.

par(mfrow=c(2,2))

# Compare methods for residual variance
plot(keep[,3],keep[,6],xlab="ML",ylab="Bayes",
     main="Compare methods for residual variance")
abline(a=0,b=1,col="red")
hist(keep[,6]-keep[,3], xlab="Deviation of Bayes from ML")

# Compare methods for group variance
plot(keep[,2],keep[,5],xlab="ML",ylab="Bayes",
     main="Compare methods for group variance")
abline(a=0,b=1,col="red")
hist(keep[,5]-keep[,2],xlab="Deviation of Bayes from ML")


par(mfrow=c(2,2))

# Compare to truth for residual variance
hist(keep[,3]-vy,xlab="Deviation of ML from truth",
     main="Residual variance")
abline(v=0,col="red")

hist(keep[,6]-vy,xlab="Deviation of Bayes from truth",
     main="Residual variance")
abline(v=0,col="red")

# Compare to truth for group variance
hist(keep[,2]-va,xlab="Deviation of ML from truth",
     main="Group variance")
abline(v=0,col="red")

hist(keep[,5]-va,xlab="Deviation of Bayes from truth",
     main="Group variance")
abline(v=0,col="red")

