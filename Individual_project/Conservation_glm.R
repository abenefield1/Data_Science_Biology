#' ---
#' output: github_document
#' ---

library(ggplot2)
library(tidyr)
library(tidyverse)
library(arm)
library(gridExtra) #To allow arranging multiple plots
library(dplyr)
library(rstan) #for extract function
library(rstanarm)
options(mc.cores = parallel::detectCores())
theme_set(theme_grey()) #rstanarm overrides default ggplot theme: set it back


### Data Cleaning

Conservation_data<-read.csv("Conservation_data.csv",header=TRUE)
#names(Conservation_data)
dat<-Conservation_data[,c(1,2,3,5,6,11,13,15,17)]
head(dat)
#View(dat)
colnames(dat)[6:9]<-c("G1","G2","G3","G4") # To simplify and limit typing
head(dat)
dat[,6:9]<-round(dat[,6:9],digits=3) # Any protected area with less than 0.001 hectares will become zero
dat[, 6:9][dat[, 6:9] == 0] <- NA # Zeros become NA to remove easily when stacked with melt
#head(dat)

#' Sanity check for when NA's are removed. Calculating the NA's for Gap_Status1:4 (columns 6:9)
n<-NULL
  for (i in 6:9) {
    n[i]<-length(dat[,i][!is.na(dat[,i])])
  }
  sum<-sum(n,na.rm=TRUE)
sum

#' Length should be:
Correct_length<-paste("Length Should Be:",sum)
Correct_length
#' Stacking the data:
library(reshape2)
dat1 <- melt(dat, id.vars=1:5, na.rm=TRUE, value.name="PA_ha")
head(dat1)
#View(dat1)
names(dat1)
colnames(dat1)[6]<-"Gap_Status"
head(dat1)
length(dat1$Gap_Status)
Correct_length
#View(dat1)

#' Truncating data at 1 hectare, because less than one is probably an error or unimportant:
data<-filter(dat1, PA_ha>=1) # Sanity check
sc<-filter(dat1, PA_ha<1)
(length(data$PA_ha) + length(sc$PA_ha))==length(dat1$PA_ha)
Correct_length2<-paste("Length Should Be:",length(data$PA_ha))
rm(data,sc) # Cleaning up
dat1<-filter(dat1, PA_ha>=1)
length(dat1$PA_ha)
Correct_length2

rm(i,n,sum,Correct_length,Correct_length2,dat) # cleaning up

#' Plots all states with hectares of protected area by gap status
dat1 %>%
  mutate(yjit=jitter(0*PA_ha)) %>%
  ggplot() +
  geom_point(mapping = aes(x=PA_ha,col=Gap_Status,y=yjit),shape=1,alpha=0.5) +
  facet_wrap(facets = ~ State_Name) +
  scale_x_continuous(breaks=c(0,1000000,2000000),limits=c(0,3000000)) +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),panel.grid=element_blank())

#' How many samples per gap status?
n<-NULL
for (i in 1:4) {
  n[i]<-length(dat1$Gap_Status[dat1$Gap_Status==paste("G",i,sep="")])
}
names(n)<-c("G1","G2","G3","G4")
n
head(dat1)
dat1 %>%
  group_by(State_Name) %>%
  summarize(sample_size=n()) %>%
  arrange(-sample_size)

rm(i,n) #cleaning up

#' We see 8 State with more than 300 protected areas sampled. Let's extract these
#' (using `filter()`) to get a better sense of how area of PA distributions within
#' State might vary between State. We'll also restrict to data from the largest gap status = G2
#' names(dat1)
#' lrgst8State_G2 <- dat1 %>%
#'   group_by(State_Name) %>%
#'   filter(n() > 300,Gap_Status=="G2")
#' #' Histograms for the eight states.
#' ggplot(data=lrgst8State_G2) +
#'   geom_histogram(mapping = aes(x=PA_ha,y=stat(density)),bins=36) +
#'   facet_wrap(facets = ~ State_Name)
#' 
#' # All Gap statuses
#' names(dat1)
#' lrgst8State_all <- dat1 %>%
#'   group_by(State_Name) %>%
#'   filter(n() > 300)
#' #' Histograms for the eight states.
#' ggplot(data=lrgst8State_all) +
#'   geom_histogram(mapping = aes(x=PA_ha,y=stat(density)),bins=36) +
#'   facet_wrap(facets = ~ State_Name)
#' # pretty concentrated around small protected areas
#' 
#' # Overlapping histograms:
#' ggplot(data=lrgst8State_all) +
#'   geom_histogram(mapping = aes(x=PA_ha,y=stat(density),fill=State_Name),
#'                  position="identity",bins=36,alpha=0.6)
#' #Density plot:
#' ggplot(data=lrgst8State_all) +
#'   geom_density(mapping = aes(x=PA_ha,col=State_Name))
#' 
#' ########## All those plots with log transformation:#########################
#' #' Histograms for the eight states.
#' ggplot(data=lrgst8State_all) +
#'   geom_histogram(mapping = aes(x=log(PA_ha),y=stat(density)),bins=36) +
#'   facet_wrap(facets = ~ State_Name)
#' # Overlapping histograms:
#' ggplot(data=lrgst8State_all) +
#'   geom_histogram(mapping = aes(x=log(PA_ha),y=stat(density),fill=State_Name),
#'                  position="identity",bins=36,alpha=0.6)
#' #Density plot:
#' ggplot(data=lrgst8State_all) +
#'   geom_density(mapping = aes(x=log(PA_ha),col=State_Name))
#' 

# So, definitely want to log transform:
dat1<-mutate(dat1,log_PA_ha=log(PA_ha))
head(dat1)

#' Take a look at the transformed data in those 8 states
#' First make a data frame with the estimated normal distribution for the 8 states
#' Summary statistics for 8 states 
#' summ8State <-
#'   dat1 %>%
#'   group_by(State_Name) %>%
#'   filter(n() > 300,Gap_Status=="G2") %>%
#'   summarize(mean=mean(log_PA_ha),sd=sd(log_PA_ha),min=min(log_PA_ha),max=max(log_PA_ha))
#' 
#' #' Normal fitted for 5 counties and collected into a dataframe.
#' norm_df <- NULL
#' for ( i in 1:8 ) {
#'   x <- seq(summ8State$min[i],summ8State$max[i],length.out = 100)
#'   y <- dnorm(x, summ8State$mean[i], summ8State$sd[i])
#'   norm_df <- rbind(norm_df,data.frame(x,y,State_Name=summ8State$State_Name[i]))
#' }
#' rm(x,y) #clean up
#' 
#' #' Now plot the transformed data with density smoother (blue) and fitted normal
#' #' (red). The log transformation seems very good:
#' dat1 %>%
#'   group_by(State_Name) %>%
#'   filter(n() > 300,Gap_Status=="G2") %>%
#'   ggplot() +
#'   geom_histogram(mapping = aes(x=log_PA_ha,y=stat(density)),bins=25) +
#'   geom_density(mapping = aes(x=log_PA_ha),col="blue") +
#'   geom_line(data=norm_df, mapping = aes(x=x,y=y),col="red") +
#'   facet_wrap(facets = ~ State_Name)

#' And now for the whole dataset split by gap status It is simple to modify the
#' above code to group by gap status instead.
summ_byGap_Status <-
  dat1 %>%
  group_by(Gap_Status) %>%
  summarize(mean=mean(log_PA_ha),sd=sd(log_PA_ha),min=min(log_PA_ha),max=max(log_PA_ha))
#' Normal fitted for 4 Gap Statuses
norm_df <- NULL
for ( i in 1:4 ) {
  x <- seq(summ_byGap_Status$min[i],summ_byGap_Status$max[i],length.out = 100)
  y <- dnorm(x, summ_byGap_Status$mean[i], summ_byGap_Status$sd[i])
  norm_df <- rbind(norm_df,data.frame(x,y,Gap_Status=summ_byGap_Status$Gap_Status[i]))
}
rm(x,y)

#' Now plot the transformed data with density smoother (blue) and fitted normal
#' (red).
dat1 %>%
  group_by(Gap_Status) %>%
  ggplot() +
  geom_histogram(mapping = aes(x=log_PA_ha,y=stat(density)),bins=25) +
  geom_density(mapping = aes(x=log_PA_ha),col="blue") +
  geom_line(data=norm_df, mapping = aes(x=x,y=y),col="red") +
  facet_wrap(facets = ~ Gap_Status)

# Log Transformation works well

#' Start by converting State_Name to a factor
dat1 <- mutate(dat1,State_Name=factor(State_Name))

#' #### Complete pooling
#'
#' In this case, complete pooling is just the overall mean. That is, we omit any
#' data structure or grouping variables.
poolmean <- mean(dat1$log_PA_ha)
poolmean
cp_pred_df <- data.frame(poolmean) #df for use with ggplot

#' #### No pooling 

#' You can think of **no pooling** as separately calculating an estimate of the
#' mean for each State_Name. For example, tabulate the means for each State_Name (and sd
#' and se):
lnPA_mean_var <- 
  dat1 %>%
  group_by(State_Name) %>%
  summarize(sample_size=n(),ST_mn=mean(log_PA_ha),ST_sd=sd(log_PA_ha)) %>%
  mutate(ST_se=ST_sd/sqrt(sample_size)) %>%
  mutate(sample_size_jit=jitter(sample_size)) #jitter added for plotting

#' Looking at the whole result to check that everything makes sense and scan for problems.
print(lnPA_mean_var,n=Inf) #n=Inf to print all rows
#View(lnPA_mean_var)
ggplot(data=lnPA_mean_var) +
  geom_hline(mapping=aes(yintercept=poolmean),data=cp_pred_df,col="blue") +
  geom_point(mapping=aes(x=sample_size_jit,y=ST_mn)) +
  geom_linerange(mapping=aes(x=sample_size_jit,ymin=ST_mn-ST_se,ymax=ST_mn+ST_se)) +
  scale_x_continuous(trans="log",breaks=c(7,20,55,148,400)) +
  labs(x="Sample size in State j",y="mean ln(PA hectares) in State j",
       title="No pooling: separate analyses by State")

#The blue line is the completely pooled estimate (the overall mean). Some of the standard
#' errors are large, because we have calculated them independently for each state.

#' The **no pooling** analysis for the state means. This analysis does not pool information about the **means** but it does pool information about the uncertainty (the error of each observation contributes to an estimate of the mean residual error). This is sometimes called the **fixed effects model**, where here `state` is the fixed effect. To fit this model in a frequentist paradigm we can use `lm()`, which is implicitly a GLM with Normal distribution and identity link. We fit `state` as a categorical variable, which gives us estimated means for each state where the maximum likelihood estimate is just the mean of the within-state samples. We use the means parameterization (i.e without the intercept, thus "-1"):

npfit <- lm( log_PA_ha ~ -1 + State_Name, data=dat1 )

#' Check the fitted model (diagnostic plots)
#+ warning=FALSE
plot(npfit,1:5,ask=FALSE)
#' The extended left tail, including the 0 + 0.1 hack, is evident in the QQ plot
#' but otherwise the diagnostics look good. Let's also look at a residuals
#' histogram compared to the Normal distribution:
r <- residuals(npfit)
x <- seq(min(r),max(r),length.out=100)
y <- dnorm(x,mean(r),sd(r))
res_df <- data.frame(residuals=r)
norm_df <- data.frame(x=x,y=y)
rm(r,x,y)
ggplot() +
  geom_histogram(mapping=aes(x=residuals,y=stat(density)),data=res_df,bins=60) +
  geom_line(mapping=aes(x=x,y=y),col="red",data=norm_df)

#' So, Normal looks like a good approximation for the errors.
#' Plot the fitted model.
np_pred_df <- data.frame(coef(summary(npfit))[,1:2],lnPA_mean_var$sample_size_jit)
names(np_pred_df) <- c("ST_mn","ST_se","sample_size_jit")
plotONE <- 
  ggplot(data=np_pred_df) +
  geom_hline(mapping=aes(yintercept=poolmean),data=cp_pred_df,col="blue") +
  geom_point(mapping=aes(x=sample_size_jit,y=ST_mn)) +
  geom_linerange(mapping=aes(x=sample_size_jit,ymin=ST_mn-ST_se,ymax=ST_mn+ST_se)) +
  scale_x_continuous(trans="log",breaks=c(7,20,55,148,400)) +
  ylim(4,14) +
  labs(x="Sample size in State_Name j",y="mean ln(PA_ha) in State_Name j",
       title="No pooling: estimates from linear model fit")
plotONE
#The blue line is the complete pooling model (i.e. the overall mean).

#' #### Partial pooling & shrinkage in multilevel model
#'
#' In the **complete pooling** model (i.e. the overall mean) we did not include
#' variation among states, while in the **no pooling** model, we estimated the
#' state means separately, whether literally by separate analyses or in the
#' fixed effects model. In the **partial pooling** model the estimates for the
#' mean in each state are a balance between the information in a state sample
#' and information from other states. To achieve this, we formulate a
#' **multilevel model**. In the multilevel model we consider two levels for
#' means: an overall mean and means for states. Each of the two levels of
#' these means has an associated stochastic process so that there are two
#' variance components, a between-state variance associated with the overall
#' mean and a within-state variance associated with the state means. To fit
#' this model in a frequentist paradigm we can use `lmer()` from the package
#' `lme4`. This model is implicitly a generalized linear mixed model (GLMM) with
#' Normal distribution, identity link, and two levels of stochasticity:

ppfit <- lmer( log_PA_ha ~ 1 + (1|State_Name), REML=FALSE, data=dat1 )

#' The `1` part of the above model specifies the overall mean (the intercept
#' term) while the `+ (1|state)` part specifies that the intercepts for each
#' state should be random variables (more specifically the deviations, or
#' "random effects", of state means from the overall mean will be modeled as a
#' Normally distributed random variable). `REML=FALSE` says to fit by ordinary
#' maximum likelihood rather than the default residual maximum likelihood.
#'
#' By default, we get limited diagnostics for `lmer()`. Just residuals vs
#' fitted. The residual plot looks good though. We will later explore some other
#' diagnostic options.
plot(ppfit)

#' In the summary we now see estimates for two levels (or strata) of variance,
#' state (among states) and residual (among counties within states):
summary(ppfit)

#' Plot the fitted model
pp_pred_df <- data.frame(coef(ppfit)$State_Name,se.ranef(ppfit)$State_Name[,1],lnPA_mean_var$sample_size_jit)
names(pp_pred_df) <- c("ST_mn","ST_se","sample_size_jit")
pp_mean_df <- data.frame(ovrl_mn=summary(ppfit)$coefficients[1],ovrl_se=summary(ppfit)$coefficients[2])
plotTWO <- 
  ggplot(data=pp_pred_df) +
  geom_hline(mapping=aes(yintercept=poolmean),data=cp_pred_df,col="blue") +
  geom_hline(mapping=aes(yintercept=ovrl_mn),data=pp_mean_df,col="blue",lty=2) +
  geom_point(mapping=aes(x=sample_size_jit,y=ST_mn)) +
  geom_linerange(mapping=aes(x=sample_size_jit,ymin=ST_mn-ST_se,ymax=ST_mn+ST_se)) +
  scale_x_continuous(trans="log",breaks=c(7,20,55,148,400)) +
  ylim(4,14) +
  labs(x="Sample size in State_Name j",y="mean ln(PA_ha) in State_Name j",
       title="Partial pooling: multilevel model, max likelihood")
plotTWO


#' Add a reference point to no pooling and partial pooling plots
plotONE_ref <- plotONE + 
  geom_point(mapping=aes(x=sample_size_jit,y=ST_mn),data=np_pred_df[8,],pch=1,cex=10,col="red")


plotTWO_ref <- plotTWO + 
  geom_point(mapping=aes(x=sample_size_jit,y=ST_mn),data=pp_pred_df[8,],pch=1,cex=10,col="red")

#' Plot side by side
grid.arrange(plotONE_ref, plotTWO_ref, nrow = 1) 

#' The right panel is the fitted multilevel model compared to our previous fit
#' of the no pooling model in the left panel. In the multilevel model the
#' estimates for the mean in each state are a balance between the sample mean
#' and the overall mean, depending on the within-state sample size. That is,
#' the information in a particular state is pooled with the information from
#' other states. You can see how this works by comparing the multilevel
#' (partial pooling) model in the right panel to the no pooling model in the
#' left panel. If there are more observations for a given state, there is more
#' information at the state level, so the estimate of the state mean in the
#' multilevel model remains close to the sample mean. If there are fewer
#' observations, information from the other states will pull an estimate for a
#' particular state toward the overall mean, like state 8 (DC), which is circled
#' in red. This is called **shrinkage**. In this case, it moves upward when shrinking, because it is lower than the overall mean. The other thing to note is the dashed blue line. This is the estimated overall mean from the multilevel model, which is also a balance of the
#' information at different levels. You can see that it is higher than simply
#' the overall mean of the data (solid blue line).

#' #### Partial pooling, Bayesian fit of multilevel model

#'Compared to the maximum likelihood model we just
#' fitted, this model had flat priors for the three model parameters (overall
#' mean and the two variances). The Bayesian version of this model is
#' accomplished easily with ' the `stan_glm()` function of `rstanarm`. 

ppfit_bayes <- stan_lmer( log_PA_ha ~ 1 + (1|State_Name), data=dat1 )
print(summary(ppfit_bayes)[,c(1,3,9,10)],digits=3)

#' Diagnostics
#+ eval=FALSE
#launch_shinystan(ppfit_bayes)

########################## Here

#' Extract posterior samples
detach("package:tidyverse", unload=TRUE) # extract name conflict
samples <- extract(ppfit_bayes$stanfit)
names(samples)
str(samples$alpha) #Samples of overall mean. Matrix: samples by row,  1 col, 4000 rows
str(samples$b) #Samples of state deviations. Matrix: samples by row, 50 cols, 4000 rows
#Reload:
library(tidyr)
library(tidyverse)
#' Algorithm for posterior samples of the state means. We merely need to add
#' the samples for the overall mean (`alpha`) to the samples for the state
#' deviations (`b`).
State_samples <- samples$b[,1:49] * NA
for ( i in 1:49 ) {
  State_samples[,i] <- samples$b[,i] + samples$alpha
}

# Now calculate mean and standard deviation of the posterior distributions for
# the state means.
statepostmns <- rep(NA,49)
statepostses <- rep(NA,49)
for ( i in 1:49 ) {
  statepostmns[i] <- mean(State_samples[,i])
  statepostses[i] <- sd(State_samples[,i])
}

#' Plot of posterior means and standard deviations
ppbayes_pred_df <- data.frame(statepostmns,statepostses,lnPA_mean_var$sample_size_jit)
names(ppbayes_pred_df) <- c("ST_mn","ST_se","sample_size_jit")
ppbayes_mean_df <- data.frame(ovrl_mn=mean(samples$alpha),ovrl_se=sd(samples$alpha))
plot_bayes <-
  ggplot(data=ppbayes_pred_df) +
  geom_hline(mapping=aes(yintercept=ovrl_mn),data=ppbayes_mean_df,col="blue",lty=2) +
  geom_point(mapping=aes(x=sample_size_jit,y=ST_mn)) +
  geom_linerange(mapping=aes(x=sample_size_jit,ymin=ST_mn-ST_se,ymax=ST_mn+ST_se)) +
  scale_x_continuous(trans="log",breaks=c(7,20,55,148,400)) +
  ylim(4,14) +
  annotate("text", label = paste(round(ppbayes_mean_df$ovrl_mn,digits=2), "ln(ha)"),x = 6.5, y = ppbayes_mean_df$ovrl_mn+0.3, size = 3.5, colour= "red")+
  annotate("text", label = paste("e^8.11=",round(exp(ppbayes_mean_df$ovrl_mn),digits=2),"ha"),x = 7, y = ppbayes_mean_df$ovrl_mn-0.3, size = 3.5, colour= "darkgreen")+
  labs(x="Sample size in state j",y="mean ln(PA hectares) in state j",
       title="Partial pooling: multilevel model, Bayesian")
grid.arrange(plotONE, plot_bayes, nrow = 1)
#7.8
plot_bayes

################################################################################
# Part II: County Level Predictor
################################################################################

#' This is part II. In part I, we did EDA and considered a variance components
#' model for counties within states, introducing the concept of partial pooling
#' and shrinkage (G&H 12.2). Here, we will consider a county-level (G&H 12.3-4)
#' predictor of hectares of protected area. In part III, we will consider a State-level predictor.
#' 
#' The following data cleaning and manipulation is **Uneccessary** unless starting from here. In that case:
#'   rm(list=ls())
library(lme4)      #max lik multilevel: lmer(), glmer() etc
library(arm)       #for se.ranef()
library(ggplot2)
library(gridExtra) #arranging multiple plots
library(dplyr)

#' Read in data and manipulate as required for analysis
Conservation_data<-read.csv("Conservation_data.csv",header=TRUE)
dat<-Conservation_data[,c(1,2,3,5,6,11,13,15,17)]
#colnames(dat)[6:9]<-c("G1","G2","G3","G4") # To simplify and limit typing
colnames(dat)[6:9]<-c(1,2,3,4) # To simplify and limit typing
dat[,6:9]<-round(dat[,6:9],digits=3) # Any protected area with less than 0.001 hectares will become zero
dat[, 6:9][dat[, 6:9] == 0] <- NA # Zeros become NA to remove easily when stacked with melt
#' Stacking the data:
library(reshape2)
dat1 <- melt(dat, id.vars=1:5, na.rm=TRUE, value.name="PA_ha")
colnames(dat1)[6]<-"Gap_Status"
#' Truncating data at 1 hectare, because less than one is probably an error or unimportant:
dat1<-filter(dat1, PA_ha>=1)
# Log Transform PA_ha:
dat1<-mutate(dat1,log_PA_ha=log(PA_ha))
head(dat1)


#' ### G&H 12.3. Analysis with predictor (Gap_Status) at the county level
#'
#' "county level" means county **scale**. This is the spatial scale at which the
#' predictor applies. The predictor at the county level is *Gap_Status* (G1:G4). There is no "county" identifier in the dataset, so we are assuming
#' that a county was either measured in the basement *or* on the first Gap_Status but
#' never both. Thus, there were no within-county measurements and the Gap_Status is an
#' attribute of the county that was measured (akin to county color, or county
#' size). If multiple measurements had been made within a county, we would need
#' to include another grouping variable, *county*, in the model.
#'
#' G&H p255 include *Gap_Status* as a continuous variable. It makes no difference
#' whether it is continuous or a factor for this particular model, it is just a
#' different parameterization. We will follow their analysis.
#'
#' Create the continuous variable - call it `Gap_Status_x`` to be consistent with
#' p255.
#dat1$Gap_Status_x <- ifelse(dat1$Gap_Status=="G2",0,1) # Is G2 = 0, Is not = 1

#' #### Complete pooling
#'
#' Complete pooling is the overall regression mean. Again, we omit any
#' data structure or grouping variables.
poolfit <- lm(log_PA_ha ~ Gap_Status, data=dat1)

#' Check the fitted model (diagnostic plots)
#+ warning=FALSE
plot(poolfit,1:5,ask=FALSE)
plot(poolfit)
#' No problems here that we haven't already discovered.
summary(poolfit)
#' Since "GS1" is coded as 0, the `(Intercept)` is the estimated mean hectares of protected area of Gap Status 1. Since "Gap Status 2" is 1, the slope `Gap_Status_2` (or $\beta_1$)
#' here is estimating how hectares of protected area in the second Gap_Status differs from the first GS We
#' see that the second Gap_Status 
#' 
#' on average has log PA_ha 0.8 **higher** than the
#' first Gap Status.
#'
#' Save $\beta$s in dataframe for use with ggplot:
cp_pred_df <- data.frame(ovrl_b0=coef(poolfit)[1],ovrl_b1=coef(poolfit)[2])

#' #### No pooling
#'
#' In the no pooling analysis we will first investigate a model that allows the
#' mean hectares of protected area to vary among states but assumes that the relationship between
#' basement and first Gap_Status stays the same among states. This model is the
#' **fixed effects** model, similar to the previous analysis we did without the
#' *Gap_Status* predictor but now we include `Gap_Status_x`. We use the means
#' parameterization (i.e. -1 to remove the overall intercept, which by default
#' would otherwise arbitrarily be hectares of protected area in the basement of the first State_Name,
#' sorted alphabetically).
npfit <- lm( log_PA_ha ~ -1 + Gap_Status + State_Name, data=dat1 )

plot(npfit,1:5,ask=FALSE)
#' Nothing terribly concerning in the diagnostics. If we were going to use this
#' model in a critical application we ought to investigate the high leverage
#' points but for now we'll note the warnings and wait to see if the problem
#' persists as we close in on a good model.

summary(npfit)
#' As in the complete pooling model, the slope (`Gap_Status_x`, or $\beta_1$) is
#' negative, indicating lower hectares of protected area on the first Gap_Status. The estimate is similar
#' to, but a little different from, the complete pooling model. The difference
#' is because we've included another variable (*State_Name*) in the model, so the
#' new estimate is after adjusting for hectares of protected area levels among states. The `State_Name`
#' estimates are the mean hectares of protected area level in the basement for each State_Name. In this
#' parameterization, the `State_Name` estimates are the y-intercept for each State_Name.
#' The *p*-values are meaningless because they are testing a hypothesis we have
#' no interest in (i.e. log(PA_ha) = 0, or PA_ha = 1, in State_Name basements).
#' 

#' Plot the fitted model (G&H Fig. 12.2) for 8 selected states
np_pred_df <- data.frame(coef(summary(npfit))[-1,1:2],
                         rep(coef(npfit)[1],85),
                         unique(dat1$State_Name))
names(np_pred_df) <- c("st_b0","st_b0_se","b1","State_Name")
display8 <- c("LAC QUI PARLE","AITKIN","KOOCHICHING","DOUGLAS","CLAY","STEARNS",
              "RAMSEY","ST LOUIS")
dat1 %>%
  filter(State_Name %in% display8) %>%
  ggplot() +
  geom_abline(mapping=aes(slope=b1,intercept=st_b0),
              data=filter(np_pred_df,State_Name %in% display8),
              col="blue") +
  geom_point(mapping=aes(x=jitter(Gap_Status,0.2),y=log_PA_ha)) +
  scale_x_continuous(breaks=c(0,1)) +
  facet_wrap(facets = ~ State_Name,ncol=4) +
  labs(x="Gap_Status",
       y="ln(PA_ha)",
       title="No pooling: estimates from linear model fit")
#' The features to notice here are that the slope (measuring the difference in
#' hectares of protected area between Gap_Statuss) is the same across states but the estimated intercept
#' differs.
#' 

#' Plot the estimated intercepts (G&H Fig. 12.3a)\
#' We will save this plot to compare to the partial pooling model. We first need
#' a jittered sample-size variable for plotting.
sample_size_df <- 
  dat1 %>%
  group_by(State_Name) %>%
  summarize(sample_size=n()) %>%
  mutate(sample_size_jit=jitter(sample_size)) #jitter added for plotting
np_pred_df <- cbind(np_pred_df,sample_size_df[,-1])

gh12.3a <- 
  ggplot(data=np_pred_df) +
  geom_hline(mapping=aes(yintercept=ovrl_b0),data=cp_pred_df,col="blue") +
  geom_point(mapping=aes(x=sample_size_jit,y=st_b0)) +
  geom_linerange(mapping=aes(x=sample_size_jit,ymin=st_b0-st_b0_se,ymax=st_b0+st_b0_se)) +
  scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
  ylim(-0.1,3.5) +
  labs(x="Sample size in State_Name j",y="Estimated intercept in State_Name j",
       title="No pooling: estimates from linear model fit")
gh12.3a


#' #### Partial pooling: multilevel model
#'
#' In the multilevel model, we model the variation among states in the
#' intercept:
ppfit <- lmer( log_PA_ha ~ Gap_Status + (1|State_Name), REML=FALSE, data=dat1 )
#' The deviations of the State_Name intercept from the mean intercept will be modeled as a
#' Normally distributed random variable.
#' 

#' Residual plot looks fine:
plot(ppfit)

#` Examine the parameter estimates:
summary(ppfit)
#' As in the model without a predictor, in the summary under `Random effects` we
#' now see estimates for two levels (or scales) of variance, `State_Name` (among
#' states) and `Residual` (among counties within states). Under
#' `Fixed effects` we have the estimate for the common slope `Gap_Status_x`, while
#' the `(Intercept)` is the estimated mean hectares of protected area at the State_Name scale (see below
#' for more discussion of exactly how to interpret this mean).
#' 

#' Plot the fitted model (G&H Fig. 12.4) for 8 selected states
pp_pred_df <- data.frame(coef(ppfit)$State_Name,
                         se.ranef(ppfit)$State_Name[,1],
                         sample_size_df$sample_size_jit,
                         unique(dat1$State_Name))
names(pp_pred_df) <- c("st_b0","b1","st_b0_se","sample_size_jit","State_Name")
pp_mean_df <- data.frame(ovrl_b0=fixef(ppfit)[1],
                         ovrl_b1=fixef(ppfit)[2])
dat1 %>%
  filter(State_Name %in% display8) %>%
  ggplot() +
  #no pooling line
  geom_abline(mapping=aes(slope=b1,intercept=st_b0),
              data=filter(np_pred_df,State_Name %in% display8),
              col="blue") +
  #partial pooling line
  geom_abline(mapping=aes(slope=b1,intercept=st_b0),
              data=filter(pp_pred_df,State_Name %in% display8),
              col="blue",
              lty=2) +
  #complete pooling line
  geom_abline(mapping=aes(slope=ovrl_b1,intercept=ovrl_b0),
              data=cp_pred_df,
              col="red") +
  #data
  geom_point(mapping=aes(x=jitter(Gap_Status,0.2),y=log_PA_ha)) +
  scale_x_continuous(breaks=c(0,1)) +
  facet_wrap(facets = ~ State_Name,ncol=4) +
  labs(x="Gap_Status",y="ln(PA_ha)",
       title="Partial pooling (dashed): multilevel model, max likelihood estimates")
#' Partial pooling estimates (dashed blue line) compared to the no pooling
#' estimates (solid blue line), and the complete pooling estimate (red line).
#' The thing to notice here is that the partial pooling estimate is shrunk away
#' from the no pooling estimate toward the complete pooling estimate. This
#' shrinkage is greatest when there are few data points in a State_Name (e.g. Lac
#' Qui Parle). When there are lots of data within a State_Name, the partial pooling
#' estimate remains close to the no pooling estimate (e.g. St Louis).
#'

#' Plot the the estimated intercepts (G&H Fig 12.3).\
#' Plot for partial pooling estimates:
gh12.3b <- 
  ggplot(data=pp_pred_df) +
  geom_hline(mapping=aes(yintercept=ovrl_b0),
             data=cp_pred_df,
             col="blue") +
  geom_hline(mapping=aes(yintercept=ovrl_b0),
             data=pp_mean_df,
             col="blue",
             lty=2) +
  geom_point(mapping=aes(x=sample_size_jit,y=st_b0)) +
  geom_linerange(mapping=aes(x=sample_size_jit,
                             ymin=st_b0-st_b0_se,
                             ymax=st_b0+st_b0_se)) +
  scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
  ylim(-0.1,3.5) +
  labs(x="Sample size in State_Name j",
       y="Estimated intercept in State_Name j",
       title="Partial pooling: multilevel model, max likelihood")

#' Plot no pooling vs partial pooling side by side
#+ fig.width=14, fig.height=7
grid.arrange(gh12.3a, gh12.3b, nrow = 1) 
#' The estimates for the State_Name intercepts are shrunk toward the State_Name-scale
#' mean (dashed blue line). The State_Name-scale mean is the mean intercept among
#' states estimated by the multilevel model. Notice that the State_Name mean is
#' higher than the complete pooling intercept (solid blue line). This is because
#' the complete pooling intercept is influenced by the six or so most-sampled
#' states (they make up a large amount of the total dataset used to fit the
#' complete pooling model), which happen to have lower hectares of protected area than the mean
#' across states. In other words, the complete pooling estimate is weighted by
#' the number of samples in a State_Name, whereas the partial pooling estimate is
#' not. This brings up an interesting situation: what if we wanted to estimate
#' the mean hectares of protected area level for counties in Minnesota? The multilevel model allows us
#' to do that. It would be the weighted mean across states, where the weights
#' are the number of counties in a State_Name (not the number of sampled counties).
#' states with more counties contribute more to the mean hectares of protected area level in
#' Minnesota counties. The State_Name-scale mean is not the "grand mean" or the mean
#' for Minnesota counties, it is the mean among Minnesota states.
#' 

#' #### Partial pooling, Bayesian fit
#'
#' The Bayesian fit of the multilevel model using `stan_lmer()` is
#' straightforward and gives almost the same result as `lmer()`. The code is
#' substantially similar to the situation we consider next - adding a
#' State_Name-level predictor - so we'll just skip right to that. See Part III.
#'



library(lme4)      #max lik multilevel: lmer(), glmer() etc
library(arm)       #for se.ranef()
library(ggplot2)
library(gridExtra) #arranging multiple plots
library(dplyr)
library(rstan)     #for extract()
library(rstanarm)  #Bayesian multilevel: stan_lmer(), stan_glmer() etc
options(mc.cores = parallel::detectCores())
theme_set(theme_grey()) #rstanarm overrides default ggplot theme: set it back

gdp<-read.csv("State_GDP.csv")
gdp$logGDP <- log(gdp$GDP_md)
head(gdp)

for ( i in 1:nrow(dat1) ) {
  dat1$logGDP[i] <- gdp$logGDP[gdp$State_Name==dat1$State_Name[i]]
}
dat1[sample(1:nrow(dat1),50),]

ppfit_bayes <- stan_lmer( log_PA_ha ~ Gap_Status + logGDP + (1|State_Name), data=dat1 )
print(summary(ppfit_bayes)[,c(1,3,9,10)],digits=3)
launch_shinystan(ppfit_bayes)
samples <- extract(ppfit_bayes$stanfit)
names(samples)
str(samples$alpha) ##Samples of overall mean. Matrix: samples by row, 1 col
str(samples$b) #Samples of state deviations. Matrix: samples by row, 50 cols
str(samples$beta) #Samples of $\beta$s. Matrix: samples by row, 2 cols.
# State mean hectares for GAP Status 1 protected areas  are $$\alpha +b + \beta_{2} * logGDP.
#First derive posterior samples for the state mean and then calculate summaries of their posterior distributions:
# Derive posterior samples for state means
StateSamples <- samples$b[,1:49] * NA
for ( i in 1:49 ) {
  StateSamples[,i] <- samples$alpha + samples$b[,i] + samples$beta[,2] * gdp$logGDP[i]
}
# Now calculate mean and standard deviation of the posterior distributions for
# the state means.
StatePostmns <- rep(NA,49)
StatePostses <- rep(NA,49)
for ( i in 1:49 ) {
  StatePostmns[i] <- mean(StateSamples[,i])
  StatePostses[i] <- sd(StateSamples[,i])
}

#Plot of posterior means and standard deviations (and compare to the maximum likelihood fit):

ppbayes_pred_df <- data.frame(ST_mn=StatePostmns,ST_se=StatePostses)
ppbayes_pred_df <- cbind(ppbayes_pred_df,gdp[,-1]) #Add U to the dataframe

gh12.6_bayes <-
  ggplot(data=ppbayes_pred_df) +
  geom_abline(intercept=mean(samples$alpha),
              slope=mean(samples$beta[,2]),
              col="blue",
              lty=2) +
  geom_point(mapping=aes(x=logGDP,
                         y=ST_mn)) +
  geom_linerange(mapping=aes(x=logGDP,
                             ymin=ST_mn-ST_se,
                             ymax=ST_mn+ST_se)) +
  #ylim(0.5,2.03) +
  labs(x="GDP of State (ln(GDP))",
       y="mean ln(hectares of protected area) in State j",
       title="Partial pooling: multilevel model, Bayesian")
gh12.6_bayes
grid.arrange(gh12.6, gh12.6_bayes, nrow = 1)
