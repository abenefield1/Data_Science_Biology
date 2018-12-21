# --- GLM frequentist Poisson analysis cheatsheet
# for one continuous (x1) + one categorical (x2) variable with 2 levels ("cat1",
# "cat2")
dat #a dataframe with x1, x2, y
plot(dat$x1,dat$y,pch=dat$x2)
fit <- glm( y ~ x1 + x2 + x1:x2, family=poisson(link="log"), data=dat )
summary(fit) #Estimates and standard errors, etc
plot(fit,1:6,ask=FALSE) #Diagnostic plots
confint(fit) #confidence intervals for parameters
cov2cor(vcov(fitHxL)) #Correlation matrix for parameters
logLik(fitHxL)  #The log likelihood

newd <- data.frame(x1 = rep(seq(min(dat$x1), max(dat$x1), length.out=100),2), 
                   x2 = factor(rep(c("cat1","cat2"),each=100)))

# For GLMs, there is no ' interval = "confidence" ' option, so we have to
# construct intervals from the standard errors. This is approximate. We use 
# 2 * s.e. here. More accurate intervals are obtained by parametric bootstrap.
# We will cover bootstrap later.
preds <- predict(fit,newdata=newd,se.fit=TRUE)
mlp <- preds$fit         #mean of the linear predictor
selp <- preds$se.fit     #se of the linear predictor
cillp <- mlp - 2 * selp  #lower of 95% CI for linear predictor
ciulp <- mlp + 2 * selp  #upper
cilp <- exp(cillp)       #lower of 95% CI for response scale
ciup <- exp(ciulp)       #upper
mp <- exp(mlp)           #mean of response scale

preds <- cbind(newd,preds,cilp,ciup,mp)
preds

# Base plot. Try to do the same in ggplot.
plot(dat$x1,dat$y,pch=dat$x2)
lty <- c(2,1)
cat <- c("cat1","cat2")
for ( i in 1:2 ) {
  subd <- subset(preds,x2==cat[i])
  lines(subd$x1,subd$mp,lty=lty[i])
  lines(subd$x1,subd$cilp,lty=lty[i],col="gray")
  lines(subd$x1,subd$ciup,lty=lty[i],col="gray")
}

# For prediction intervals, we have to simulate the full fitted model. Again,
# this is most accurate as a parameteric bootstrap. Later.



# --- GLM Bayesian Poisson analysis cheatsheet for rstanarm
library(rstan) #for extract function
library(rstanarm)
theme_set(theme_grey()) #rstanarm overrides default ggplot theme: set it back

fit <- stan_glm( y ~ x1 + x2 + x1:x2, family=poisson(link="log"), data=dat )
# Default priors are weakly informative

# The samples themselves
# You can do anything with these, as explained in McElreath, including
# everything in the convenience functions below.
samples <- extract(fit$stanfit)
class(samples)
str(samples)
names(samples)
hist(samples$beta[,1]) #e.g. histogram of \beta_1

# Convenience functions that estimate things from the samples
methods(class="stanreg") 
?coef.stanreg #see esp. rstanarm::stanreg-methods here
summary(fit,digits=4) #Estimates and standard errors, etc
launch_shinystan(fit) #Diagnostic plots
posterior_interval(fit,prob=0.95) #central posterior intervals, nb default=0.9
vcov(fit,correlation=TRUE) #Correlation matrix

# Plotting regression intervals (credible intervals and prediction intervals)
# Use the samples, as you learned from McElreath




