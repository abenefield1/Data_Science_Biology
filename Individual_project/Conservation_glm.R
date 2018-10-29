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



Conservation_data<-read.csv("Conservation_data.csv",header=TRUE)
names(Conservation_data)
dat<-Conservation_data[,c(1,2,3,5,6,11,13,15,17)]
head(dat)
#View(dat)
colnames(dat)[6:9]<-c("G1","G2","G3","G4")
head(dat)
dat[,6:9]<-round(dat[,6:9],digits=3)
dat[, 6:9][dat[, 6:9] == 0] <- NA
head(dat)

###################################################################here: Should be 9327
n<-NULL
  for (i in 6:9) {
    n[i]<-length(dat[,i][!is.na(dat[,i])])
  }
  sum<-sum(n,na.rm=TRUE)
sum

#Length should be:
Correct_length<-paste("Length Should Be:",sum)
Correct_length
#Stacking
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

# Truncating data at 1 hectare, because less than one is probably an error or unimportant:
data<-filter(dat1, PA_ha>=1) # Sanity check
sc<-filter(dat1, PA_ha<1)
(length(data$PA_ha) + length(sc$PA_ha))==length(dat1$PA_ha)
Correct_length2<-paste("Length Should Be:",length(data$PA_ha))
rm(data,sc) # Cleaning up
dat1<-filter(dat1, PA_ha>=1)
length(dat1$PA_ha)
Correct_length2

# Plots all states with hectares of protected area by gap status
dat1 %>%
  mutate(yjit=jitter(0*PA_ha)) %>%
  ggplot() +
  geom_point(mapping = aes(x=PA_ha,col=Gap_Status,y=yjit),shape=1,alpha=0.5) +
  facet_wrap(facets = ~ State_Name) +
  scale_x_continuous(breaks=c(0,20,40),limits=c(0,50)) +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),panel.grid=element_blank())

# How many samples per gap status?
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

#' We see 8 State with more than 300 protected areas sampled. Let's extract these
#' (using `filter()`) to get a better sense of how area of PA distributions within
#' State might vary between State. We'll also restrict to data from the largest gap status = G2
names(dat1)
lrgst8State_G2 <- dat1 %>%
  group_by(State_Name) %>%
  filter(n() > 300,Gap_Status=="G2")
#' Histograms for the eight states.
ggplot(data=lrgst8State_G2) +
  geom_histogram(mapping = aes(x=PA_ha,y=stat(density)),bins=36) +
  facet_wrap(facets = ~ State_Name)

# All Gap statuses
names(dat1)
lrgst8State_all <- dat1 %>%
  group_by(State_Name) %>%
  filter(n() > 300)
#' Histograms for the eight states.
ggplot(data=lrgst8State_all) +
  geom_histogram(mapping = aes(x=PA_ha,y=stat(density)),bins=36) +
  facet_wrap(facets = ~ State_Name)
# pretty concentrated around small protected areas

# Overlapping histograms:
ggplot(data=lrgst8State_all) +
  geom_histogram(mapping = aes(x=PA_ha,y=stat(density),fill=State_Name),
                 position="identity",bins=36,alpha=0.6)
#Density plot:
ggplot(data=lrgst8State_all) +
  geom_density(mapping = aes(x=PA_ha,col=State_Name))

########## All those plots with log transformation:#########################
#' Histograms for the eight states.
ggplot(data=lrgst8State_all) +
  geom_histogram(mapping = aes(x=log(PA_ha),y=stat(density)),bins=36) +
  facet_wrap(facets = ~ State_Name)
# Overlapping histograms:
ggplot(data=lrgst8State_all) +
  geom_histogram(mapping = aes(x=log(PA_ha),y=stat(density),fill=State_Name),
                 position="identity",bins=36,alpha=0.6)
#Density plot:
ggplot(data=lrgst8State_all) +
  geom_density(mapping = aes(x=log(PA_ha),col=State_Name))


# So, definitely want to log transform:
dat1<-mutate(dat1,log_PA_ha=log(PA_ha))
head(dat1)

#' Take a look at the transformed data in those 8 states
#' First make a data frame with the estimated normal distribution for the 8 states
#' Summary statistics for 8 states 
summ8State <-
  dat1 %>%
  group_by(State_Name) %>%
  filter(n() > 300,Gap_Status=="G2") %>%
  summarize(mean=mean(log_PA_ha),sd=sd(log_PA_ha),min=min(log_PA_ha),max=max(log_PA_ha))

#' Normal fitted for 5 counties and collected into a dataframe.
norm_df <- NULL
for ( i in 1:8 ) {
  x <- seq(summ8State$min[i],summ8State$max[i],length.out = 100)
  y <- dnorm(x, summ8State$mean[i], summ8State$sd[i])
  norm_df <- rbind(norm_df,data.frame(x,y,State_Name=summ8State$State_Name[i]))
}
rm(x,y) #clean up

#' Now plot the transformed data with density smoother (blue) and fitted normal
#' (red). The log transformation seems very good:
dat1 %>%
  group_by(State_Name) %>%
  filter(n() > 300,Gap_Status=="G2") %>%
  ggplot() +
  geom_histogram(mapping = aes(x=log_PA_ha,y=stat(density)),bins=25) +
  geom_density(mapping = aes(x=log_PA_ha),col="blue") +
  geom_line(data=norm_df, mapping = aes(x=x,y=y),col="red") +
  facet_wrap(facets = ~ State_Name)

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
  scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
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
  scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
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
  scale_x_continuous(trans="log",breaks=c(1,3,10,30,100)) +
  labs(x="Sample size in State_Name j",y="mean ln(radon) in State_Name j",
       title="Partial pooling: multilevel model, max likelihood")
plotTWO


#' Add a reference point to no pooling and partial pooling plots
plotONE_ref <- plotONE + 
  geom_point(mapping=aes(x=sample_size_jit,y=ST_mn),data=np_pred_df[8,],pch=1,cex=10,col="red")


plotTWO_ref <- plotTWO + 
  geom_point(mapping=aes(x=sample_size_jit,y=ST_mn),data=pp_pred_df[8,],pch=1,cex=10,col="red")

#' Plot side by side
#+ fig.width=14, fig.height=7
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