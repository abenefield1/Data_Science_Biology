Simulation.R
================
Amy
Fri Dec 21 02:21:55 2018

``` r
library(lme4)
```

    ## Loading required package: Matrix

``` r
library(rstanarm)
```

    ## Loading required package: Rcpp

    ## rstanarm (Version 2.17.4, packaged: 2018-04-13 01:51:52 UTC)

    ## - Do not expect the default priors to remain the same in future rstanarm versions.

    ## Thus, R scripts should specify priors explicitly, even if they are just the defaults.

    ## - For execution on a local, multicore CPU with excess RAM we recommend calling

    ## options(mc.cores = parallel::detectCores())

    ## - Plotting theme set to bayesplot::theme_default().

``` r
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
```

    ##           y group
    ## 1 10.243865     1
    ## 2 10.557818     1
    ## 3 11.777750     1
    ## 4 10.524641     1
    ## 5  9.734652     2
    ## 6 11.262598     2

``` r
baysfit <- stan_lmer(y ~ 1 + (1|group), data=dat)

summary(baysfit)[c(1:6,51,52,53),c(1,3,10,9)] #a sample of the full output
```

    ##                                             mean         sd      Rhat
    ## (Intercept)                          10.86764496 0.20442015 1.0032650
    ## b[(Intercept) group:1]               -0.07299321 0.67216241 0.9996234
    ## b[(Intercept) group:2]                0.23036739 0.65678400 0.9997122
    ## b[(Intercept) group:3]               -0.98467249 0.68632744 0.9993911
    ## b[(Intercept) group:4]                0.81447825 0.68826317 1.0001270
    ## b[(Intercept) group:5]               -0.25191533 0.66813182 0.9994557
    ## sigma                                 1.61048604 0.09525579 1.0013016
    ## Sigma[group:(Intercept),(Intercept)]  1.39873472 0.45709114 1.0010832
    ## mean_PPD                             10.86789333 0.16675766 1.0005531
    ##                                      n_eff
    ## (Intercept)                           1410
    ## b[(Intercept) group:1]                4000
    ## b[(Intercept) group:2]                4000
    ## b[(Intercept) group:3]                4000
    ## b[(Intercept) group:4]                4000
    ## b[(Intercept) group:5]                4000
    ## sigma                                 2491
    ## Sigma[group:(Intercept),(Intercept)]  1380
    ## mean_PPD                              4000

``` r
cbind(intercept=mubar,sigma=sqrt(vy),Sigma_group=va)
```

    ##      intercept    sigma Sigma_group
    ## [1,]      10.9 1.489966        2.59

``` r
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
```

    ##          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]
    ## [1,] 10.62741 3.049994 1.963523 10.60926 3.229046 1.995545
    ## [2,] 10.67128 3.023494 2.521631 10.65967 3.233300 2.559150
    ## [3,] 11.12894 3.015672 2.827233 11.13762 3.218815 2.868882
    ## [4,] 11.12466 3.192508 2.672807 11.12102 3.369243 2.713598
    ## [5,] 11.16818 1.889794 1.883806 11.16945 2.008533 1.917203
    ## [6,] 10.76502 2.787712 2.079798 10.76736 2.944687 2.109889

``` r
ml_mubar <- mean(keep[,1])
ml_va <- mean(keep[,2])
ml_vy <- mean(keep[,3])
b_mubar <- mean(keep[,4])
b_va <- mean(keep[,5])
b_vy <- mean(keep[,6])

cbind(mubar,ml_mubar,b_mubar)
```

    ##      mubar ml_mubar  b_mubar
    ## [1,]  10.9 10.92503 10.92383

``` r
cbind(vy,ml_vy,b_vy)
```

    ##        vy    ml_vy     b_vy
    ## [1,] 2.22 2.281299 2.318339

``` r
cbind(va,ml_va,b_va)
```

    ##        va    ml_va     b_va
    ## [1,] 2.59 2.527796 2.679924

``` r
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
```

![](Simulation_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
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
```

![](Simulation_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->
