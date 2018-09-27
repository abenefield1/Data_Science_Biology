#Stopped @ line 110. Did I lose cauchy? On first medium question?

sampost<-function( flist, data, n=10000 ) {
  quadapprox<-map(flist,data) 
  posterior_sample<-extract.samples(quadapprox,n) 
  return(posterior_sample)}
library(rstan)
library(rstanarm)
library(rethinking)

######################################################################################################################
# 8E1: Which of the following is a requirement of the simple Metropolis algorithm? (3) The proposal distribution must be symmetric.
######################################################################################################################

######################################################################################################################
#8E2: Gibbs sampling is more efficient than the metropolis algorithm, because we can see an equally accurate view of the posterior distribution in fewer steps. This is due to "adaptive proposals in which the distribution of proposed parameter values adjusts itself intelligently, depending upon the parameter values at the moment." Gibbs sampling is limited to circumstances in which it makes sense to use conjugate priors. Additionally, it becomes very clunky as model complexity and number of parameters increases.
######################################################################################################################

######################################################################################################################
#8E3. Which sort of parameters can Hamiltonian Monte Carlo not handle? Can you explain why?
# They can't handle discreet parameters. This is because the output of the model depends upon a rate (in his examples, speed at which the king travels and population growth rate), and rates can never be discreet. Also, because the model is, "memoryless," it is impossible for it to jump across missing values.
######################################################################################################################

######################################################################################################################
#8E4. Explain the difference between the effective number of samples, n_eff as calculated by Stan, and the actual number of samples. The effective number of samples are those portion of the posterior distribution that are not autocorrelated. They are the independent samples.
######################################################################################################################

######################################################################################################################
#8E5. Which value should Rhat approach, when a chain is sampling the posterior distribution correctly?
# It should approach 1.00
######################################################################################################################

######################################################################################################################
#8E6. Sketch a good trace plot for a Markov chain, one that is effectively sampling from the posterior distribution. What is good about its shape? Then sketch a trace plot for a malfunctioning Markov chain. What about its shape indicates malfunction?
######################################################################################################################
y <- c(-1,1) 
m8.2 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha) ,
data=list(y=y) , start=list(alpha=0,sigma=1) , 
chains=2 , iter=4000 , warmup=1000 )
precis(m8.2)
plot(m8.2)


#####################################################################################################
#8M1:
#####################################################################################################
rm(list=ls())
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd.trim <- dd[ , c("log_gdp", "rugged", "cont_africa")]

#Uniform prior for sigma
m8.1.unif <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data=dd.trim )
m8.1.unif.stan<-stan_lm(log_gdp ~ rugged + cont_africa + rugged:cont_africa, data=dd.trim, prior=NULL)
summary(m8.1.unif.stan)
summary(m8.1.unif)
  # So, not super different, but the log-posterior is all messed up
precis(m8.1.unif)

# put an exponential prior on sigma
m8.1.exp <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data=dd.trim )
m8.1.exp.stan<-stan_glm(log_gdp ~ rugged + cont_africa + rugged:cont_africa, family = gaussian(), data=dd.trim, prior = normal(), prior_aux = exponential())
summary(m8.1.exp)
summary(m8.1.exp.stan)


# visualize each prior
dev.off()
plot.new()
curve(dcauchy(x, 0, 2), from = 0, to = 10, xlab = "sigma", ylab = "Density", ylim = c(0, 1))
curve(dunif(x, 0, 10), from = 0, to = 10, add = TRUE, col = "blue")
curve(dexp(x, 1), from = 0, to = 10, add = TRUE, col = "red")

# plot the posterior for sigma for each model
sigma_unif <- extract.samples(m8.1.unif,pars="sigma")
sigma_exp <- extract.samples(m8.1.exp,pars="sigma")
dens(sigma_unif[[1]], xlab="sigma", xlim=c(0.5,1.5), col="red")
dens(sigma_exp[[1]], add=TRUE, col="blue")

grid(col="darkgray")
pairs(m8.1.exp)
pairs(m8.1.unif)
plot(m8.1.exp)
plot(m8.1.unif)
precis(m8.1.exp)
precis(m8.1.unif)


####################################################################################################
#8M2:
####################################################################################################
rm(list=ls())
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd.trim <- dd[ , c("log_gdp", "rugged", "cont_africa")]

# fit models with a cauchy prior on sigma for varying scale parameter values
m8.2.cauchy.10 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 10)
  ), data=dd.trim )

m8.2.cauchy.1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1)
  ), data=dd.trim )

m8.2.cauchy.point.1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, .1)
  ), data=dd.trim )


# fit models with an exponential prior on sigma for varying scale parameter values
m8.2.exp.10 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dexp(10)
  ), data=dd.trim )

m8.2.exp.1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data=dd.trim )

m8.2.exp.point.1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dexp(.1)
  ), data=dd.trim )

# plot the posterior distribution for sigma under the cauchy priors
sigma.cauchy.10 <- extract.samples(m8.2.cauchy.10, pars="sigma")
sigma.cauchy.1 <- extract.samples(m8.2.cauchy.1, pars="sigma")
sigma.cauchy.point.1 <- extract.samples(m8.2.cauchy.point.1, pars="sigma")
dev.off()
plot.new
dens(sigma.cauchy.10[[1]], xlab="sigma",ylim=c(0,9), col="red")
dens(sigma.cauchy.1[[1]], add=TRUE, col="blue")
dens(sigma.cauchy.point.1[[1]], add=TRUE, col="darkgreen")

# plot the posterior distribution for sigma under the exponential priors
sigma.exp.10 <- extract.samples(m8.2.exp.10, pars="sigma")
sigma.exp.1 <- extract.samples(m8.2.exp.1, pars="sigma")
sigma.exp.point.1 <- extract.samples(m8.2.exp.point.1, pars="sigma")
dens(sigma.exp.10[[1]], xlab="sigma",ylim=c(0,10) ,col="red")
dens(sigma.exp.1[[1]], add=TRUE, col="blue")
dens(sigma.exp.point.1[[1]], add=TRUE, col="green")

####################################################################################################
#8M2:
####################################################################################################
rm(list=ls())
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd.trim <- dd[ , c("log_gdp", "rugged", "cont_africa")]
dev.off()
plot.new()

m <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ), data=dd.trim )

m.warmup.1 <- map2stan(m, chains = 4, cores = 4, warmup = 1, iter = 1000)
m.warmup.5 <- map2stan(m, chains = 4, cores = 4, warmup = 5, iter = 1000)
m.warmup.10 <- map2stan(m, chains = 4, cores = 4, warmup = 10, iter = 1000)
m.warmup.50 <- map2stan(m, chains = 4, cores = 4, warmup = 50, iter = 1000)
m.warmup.100 <- map2stan(m, chains = 4, cores = 4, warmup = 100, iter = 1000)
m.warmup.500 <- map2stan(m, chains = 4, cores = 4, warmup = 500, iter = 1000)
m.warmup.1000 <- map2stan(m, chains = 4, cores = 4, warmup = 1000, iter = 1000)

precis(m.warmup.1)
precis(m.warmup.5)
precis(m.warmup.10)
precis(m.warmup.50)
precis(m.warmup.100)
precis(m.warmup.500)
precis(m.warmup.1000)

