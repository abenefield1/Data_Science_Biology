# --- GLM frequentist Poisson analysis cheatsheet
# for one continuous (x1) + one categorical (x2) variable with 2 levels ("cat1",
# "cat2")
library(car)

dat<-data.frame(ants$latitude,ants$habitat, ants$richness)
colnames(dat)<-c("latitude","habitat","richness")
head(dat)
dat #a dataframe with x1, x2, y
plot(dat$latitude,dat$richness,pch=dat$habitat)
plot(dat$latitude,dat$richness)
fit <- glm(richness ~ latitude + habitat + latitude:habitat, family=poisson(link="log"), data=dat )
summary(fit) #Estimates and standard errors, etc
plot(fit,1:6,ask=FALSE) #Diagnostic plots
confint(fit) #confidence intervals for parameters
cov2cor(vcov(fit)) #Correlation matrix for parameters
logLik(fit)  #The log likelihood

fitHxL<-glm(richness~habitat+latitude+habitat:latitude, family=poisson, data=ants)
plot(fitHxL,1:5,ask=FALSE)
confint(fitHxL)
cov2cor(vcov(fitHxL))
logLik(fitHxL)

newd <- data.frame(latitude = rep(seq(min(ants$latitude), max(ants$latitude), length.out=100),2), 
                   habitat = factor(rep(c("bog","forest"),each=100)))

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
plot(ants$latitude,ants$richness)
lty <- c(2,1)
cat <- c("bog","forest")
for ( i in 1:2 ) {
  subd <- subset(preds,habitat==cat[i])
  lines(subd$latitude,subd$mp,lty=lty[i])
  lines(subd$latitude,subd$cilp,lty=lty[i],col="gray")
  lines(subd$latitude,subd$ciup,lty=lty[i],col="gray")
}

# For prediction intervals, we have to simulate the full fitted model. Again,
# this is most accurate as a parameteric bootstrap. Later.



# --- GLM Bayesian Poisson analysis cheatsheet for rstanarm
library(rstan) #for extract function
library(rstanarm)

fit <- stan_glm( richness ~ latitude + habitat + latitude:habitat, family=poisson(link="log"), data=dat )
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
methods(class="stanreg") #see esp. rstanarm::stanreg-methods here
?coef.stanreg #see esp. rstanarm::stanreg-methods here
summary(fit) #Estimates and standard errors, etc
launch_shinystan(fit) #Diagnostic plots
posterior_interval(fit,prob=0.95) #central posterior intervals, nb default=0.9
vcov(fit,correlation=TRUE) #Correlation matrix

# Plotting regression intervals (credible intervals and prediction intervals)
# Use the samples, as you learned from McElreath


library(ggplot2)
ggplot()+
  geom_point(mapping=aes(x=latitude, y=richness,col=habitat),data=ants)


ggplot()+
  geom_ribbon(mapping=aes(x=latitude,ymin=cilp,ymax=ciup,fill=habitat),alpha=0.2,data=preds)+
  geom_point(mapping=aes(x=latitude, y=richness, colour=habitat),data=ants)+
geom_line(mapping=aes(x=latitude,y=mp,col=habitat),data=preds)

#In class:
fit<-glm(richness~habitat+latitude+habitat:latitude, family=poisson(link="log"),data=ants)
summary(fit)
model.matrix(fit)
explanation<-model.matrix(fit)
colnames(explanation)<-c("beta0_intercept","beta1_forest","beta2_latitude","beta3_forestXlatitude")
head(explanation)
sum(explanation[,2])
length(explanation[,2])
stepAIC(fit)

#Without interaction term:
fit1<-glm(richness~habitat+latitude, family=poisson(link="log"),data=ants)
summary(fit1)
