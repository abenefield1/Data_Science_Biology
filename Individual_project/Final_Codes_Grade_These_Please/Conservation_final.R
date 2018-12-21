#' ---
#' output: github_document
#' ---

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(arm)
library(gridExtra) #To allow arranging multiple plots
library(rstan) #for extract function
library(rstanarm)
library(reshape2)
options(mc.cores = parallel::detectCores())
theme_set(theme_grey()) #rstanarm overrides default ggplot theme: set it back


## Data Cleaning
Conservation_data<-read.csv("Conservation_data.csv",header=TRUE)
names(Conservation_data)
dat<-Conservation_data[,c(1,2,3,5,6,11,13,15,17)]
head(dat)
colnames(dat)[6:9]<-c(1,2,3,4) # To simplify and limit typing
dat[,6:9]<-round(dat[,6:9],digits=3) # Any protected area with less than 0.001 hectares will become zero
dat[, 6:9][dat[, 6:9] == 0] <- NA # Zeros become NA to remove easily when stacked with melt
head(dat)

#' Sanity check for when NA's are removed. Calculating the NA's for Gap_Status1:4 (columns 6:9)
n<-NULL
for (i in 6:9) {
  n[i]<-length(dat[,i][!is.na(dat[,i])])
}
sum<-sum(n,na.rm=TRUE)
sum
rm(n)
#' Length should be:
Correct_length<-paste("Length Should Be:",sum)
Correct_length

#' Stacking the data:
dat1 <- melt(dat, id.vars=1:5, na.rm=TRUE, value.name="PA_ha") # Data stacked by county identifiers. Gap Status columns now stacked into two columns: Gap Status type and hectares of protected area
colnames(dat1)[6]<-"Gap_Status"
head(dat1)
length(dat1$Gap_Status)
Correct_length
rm(sum,i,dat,Correct_length) # clean up
#' Truncating data at 1 hectare, because less than one is probably an error or unimportant:
dat1<-filter(dat1, PA_ha>=1)

#' ### Exploratory Data Analysis:
#' Plots all states with hectares of protected area by gap status
dat1 %>%
  mutate(yjit=jitter(0*PA_ha)) %>%
  ggplot() +
  geom_point(mapping = aes(x=PA_ha,col=Gap_Status,y=yjit),shape=1,alpha=0.5) +
  facet_wrap(facets = ~ State_Name) +
  scale_x_continuous(breaks=c(0,1000000,2000000),limits=c(0,3000000)) +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),panel.grid=element_blank())

#' How many samples are there per gap status?
percent<-NULL
for (i in 1:4) {
  percent[i]<-((length(dat1$Gap_Status[dat1$Gap_Status==i]))/length(dat1$Gap_Status))*100
}
names(percent)<-c("G1","G2","G3","G4")
count<-(percent/100)*length(dat1$County_ID_FIPS)
totals<-rbind(percent,count)
totals
rm(percent,count,i) # clean up

dat1 %>%
  group_by(State_Name) %>%
  summarize(sample_size=n()) %>%
  arrange(-sample_size)

#' Eight states have more than 300 protected areas sampled. Extracting these gives us an idea of how the distributions of protected area hectares within state might vary between state. Data limited to the largest gap status = G2
names(dat1)
lrgst8State_G2 <- dat1 %>%
   group_by(State_Name) %>%
   filter(n() > 300,Gap_Status==2)
#' Histograms for the eight states.
 ggplot(data=lrgst8State_G2) +
   geom_histogram(mapping = aes(x=PA_ha,y=stat(density)),bins=36) +
   facet_wrap(facets = ~ State_Name)

 #' Now Looking at All Gap statuses
  lrgst8State_all <- dat1 %>%
    group_by(State_Name) %>%
    filter(n() > 300)
#' Histograms for the eight states.
ggplot(data=lrgst8State_all) +
  geom_histogram(mapping = aes(x=PA_ha,y=stat(density)),bins=36) +
  facet_wrap(facets = ~ State_Name)
 #' The data is pretty concentrated around small protected areas
 
#' Overlapping histograms:
ggplot(data=lrgst8State_all) +
   geom_histogram(mapping = aes(x=PA_ha,y=stat(density),fill=State_Name),
                  position="identity",bins=36,alpha=0.6)
  # Not very helpful
#' Density plot:
ggplot(data=lrgst8State_all) +
   geom_density(mapping = aes(x=PA_ha,col=State_Name))
# Again, this isn't very informative, but a log transformation seems like a good idea

#' All of those plots now with log transformation:
#' Histograms for the eight states.
ggplot(data=lrgst8State_all) +
   geom_histogram(mapping = aes(x=log(PA_ha),y=stat(density)),bins=36) +
   facet_wrap(facets = ~ State_Name)
#' Overlapping histograms:
ggplot(data=lrgst8State_all) +
  geom_histogram(mapping = aes(x=log(PA_ha),y=stat(density),fill=State_Name),
                  position="identity",bins=36,alpha=0.6)
#' Density plot:
ggplot(data=lrgst8State_all) +
   geom_density(mapping = aes(x=log(PA_ha),col=State_Name))

# So, it definitely makes sense to log transform:
dat1<-mutate(dat1,log_PA_ha=log(PA_ha))
head(dat1)


#' Analysis with predictor (Gap_Status) at the county level. County level" means county **scale**. This is the spatial scale at which the predictor applies. The predictor at the county level is *Gap_Status* (G1:G4). #' Complete pooling: the overall regression mean. Omitting any data structure or grouping variables:
poolfit <- lm(log_PA_ha ~ Gap_Status, data=dat1)
#' Diagnostic Plots:
plot(poolfit,1:5,ask=FALSE)
#' No major problems, but the it is important to note the tails of the Q-Q plot
summary(poolfit)
#' The `(Intercept)` is the estimated mean log hectares of protected area for Gap Status 1 (6.93). The slope `Gap_Status_2` (or $\beta_1$) estimates how log hectares of protected area in the Gap Status 2 differ from Gap Status 1. Gap Status 2 on average has log_PA_ha 0.8 **higher** than Gap Status 1.
#' Saving $\beta$s in dataframe for use with ggplot:
cp_pred_df <- data.frame(ovrl_b0=coef(poolfit)[1],ovrl_b1=coef(poolfit)[2]) # overall slope and intercept

#' No pooling:
#' Mean hectares of protected area to will vary among states but the relationship between the Gap_Statuses (the slope) stays the same among states. This is the **fixed effects** model, similar to the analysis without the *Gap_Status* predictor, but now `Gap_Status_x` is included. This uses the means parameterization (i.e. -1) to remove the overall intercept, which by default would arbitrarily be the log hectares of protected area of Gap Status 1 of the first State.
npfit <- lm( log_PA_ha ~ -1 + State_Name, data=dat1 )
plot(npfit,1:5,ask=FALSE)
#' The leverage plot is a bit concerning, but I'll move on for now.
summary(npfit)
#' The slope (`Gap_Status 1`, or $\beta_1$) estimate is similar the complete pooling model. The difference is due to the new variable (*State_Name*) in the model. The new estimate is made after adjusting for hectares of protected area levels among states. The `State_Name` estimates are the mean hectares of protected area of Gap Status 1 for each State. In this parameterization, the `State_Name` estimates are the y-intercept for each State.

#' Plot the fitted model for 8 selected states
np_pred_df <- data.frame(coef(summary(npfit))[,1:2],
                          Slope=rep(coef(npfit)[1],49))

head(np_pred_df)
SN<-row.names(np_pred_df)
SN2<-sapply(strsplit(SN, split='State_Name', fixed=TRUE), `[`, 2) ## Output a vector (even though a list is also a vector)
head(SN2)
np_pred_df <- data.frame(np_pred_df, SN2)
names(np_pred_df) <- c("st_b0","st_b0_se","b1","State_Name")
head(np_pred_df)
display8 <- c("Georgia","Illinois","Iowa","Missouri","North Carolina","Ohio","Texas","Virginia")

dat1$Gap_Status<-as.numeric(dat1$Gap_Status)
str(dat1)
dat1 %>%
   filter(State_Name %in% display8) %>%
   ggplot() +
   geom_abline(mapping=aes(slope=b1,intercept=st_b0),
               data=filter(np_pred_df,State_Name %in% display8),
               col="blue") +
   geom_point(mapping=aes(x=jitter(Gap_Status),y=log_PA_ha)) +
   #scale_x_continuous(breaks=c(0,1)) +
  facet_wrap(facets = ~ State_Name,ncol=4) +
  labs(x="Gap_Status",
        y="ln(PA_ha)",
        title="No pooling: estimates from linear model fit")
#' The slope (measuring the difference in hectares of protected area between Gap_Statuses) is the same across states but the estimated intercept differs.

#' Plot the estimated intercepts: Saving this plot to compare to the partial pooling model. Need a jittered sample-size variable for plotting.
sample_size_df <- 
   dat1 %>%
   group_by(State_Name) %>%
   summarize(sample_size=n()) %>%
   mutate(sample_size_jit=jitter(sample_size)) #jitter added for plotting
np_pred_df <- cbind(np_pred_df,sample_size_df[,-1])
head(np_pred_df)

No_Pooling_Plot <- 
   ggplot(data=np_pred_df) +
   geom_hline(mapping=aes(yintercept=ovrl_b0),data=cp_pred_df,col="blue") +
   geom_point(mapping=aes(x=sample_size_jit,y=st_b0)) +
   geom_linerange(mapping=aes(x=sample_size_jit,ymin=st_b0-st_b0_se,ymax=st_b0+st_b0_se)) +
  scale_x_continuous(trans="log",breaks=c(1,10,30,100,500)) +
  labs(x="Sample size in State_Name j",y="Estimated intercept in State_Name j",
        title="No pooling: estimates from linear model fit")
No_Pooling_Plot

#' #### Partial pooling: multilevel model
#' In the multilevel model, the variation among states is modelled in the intercept:
ppfit <- lmer( log_PA_ha ~ Gap_Status + (1|State_Name), REML=FALSE, data=dat1 )
#' The deviations of the State intercept from the mean intercept will be modeled as a Normally distributed random variable.
#' Residual plot looks fine:
plot(ppfit)
#' Examine the parameter estimates:
summary(ppfit)
#' As in the model without a predictor, in the summary under `Random effects` there are now estimates for two levels (or scales) of variance, `State_Name` (among states) and `Residual` (among counties within states). Under `Fixed effects` we have the estimate for the common slope `Gap_Status_x`, while the `(Intercept)` is the estimated mean hectares of protected area at the State scale.
#' #' #' Plot the fitted model (G&H Fig. 12.4) for 8 selected states
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
  facet_wrap(facets = ~ State_Name,ncol=4) +
  labs(x="Gap_Status",y="ln(PA_ha)",
       title="Partial pooling (dashed): multilevel model, max likelihood estimates")
#' Partial pooling estimates (dashed blue line) compared to the no pooling estimates (solid blue line), and the complete pooling estimate (red line).

#Plot the the estimated intercepts 
#' Plot for partial pooling estimates:
Partial_Pooling_Plot <-
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
  scale_x_continuous(trans="log",breaks=c(1,10,30,100,500)) +
  #ylim(-0.1,3.5) +
  labs(x="Sample size in State_Name j",
       y="Estimated intercept in State_Name j",
       title="Partial pooling: multilevel model, max likelihood")
Partial_Pooling_Plot
No_Pooling_Plot_ref <- No_Pooling_Plot + 
  geom_point(mapping=aes(x=sample_size_jit,y=st_b0),data=np_pred_df[8,],pch=1,cex=10,col="red")
Partial_Pooling_Plot_ref <- Partial_Pooling_Plot + 
  geom_point(mapping=aes(x=sample_size_jit,y=st_b0),data=pp_pred_df[8,],pch=1,cex=10,col="red")
#' Plot no pooling vs partial pooling side by side
grid.arrange(No_Pooling_Plot_ref, Partial_Pooling_Plot_ref, nrow = 1)

#' The estimates for the State intercepts are shrunk toward the State-scale mean (dashed blue line). The State-scale mean is the mean intercept among states estimated by the multilevel model. The State mean is higher than the complete pooling intercept (solid blue line), because the complete pooling intercept is influenced by the most-sampled states. The complete pooling estimate is weighted by the number of samples in a State, whereas the partial pooling estimate is not.


#' #### Multilevel Model with State-Level Predictor: GDP
gdp<-read.csv("State_GDP.csv")
gdp$logGDP <- log(gdp$GDP_md)
head(gdp)

for ( i in 1:nrow(dat1) ) {
  dat1$logGDP[i] <- gdp$logGDP[gdp$State_Name==dat1$State_Name[i]]
}
dat1[sample(1:nrow(dat1),50),]
dat1$Gap_Status<-as.factor(dat1$Gap_Status)
ppfit_bayes <- stan_lmer( log_PA_ha ~ Gap_Status + logGDP + (1|State_Name), data=dat1 )
print(summary(ppfit_bayes)[,c(1,3,9,10)],digits=3)
head(gdp)
#launch_shinystan(ppfit_bayes)
samples <- extract(ppfit_bayes$stanfit)
names(samples)
str(samples$alpha) ##Samples of overall mean. Matrix: samples by row, 1 col
str(samples$b) #Samples of state deviations. Matrix: samples by row, 50 cols
str(samples$beta) #Samples of $\beta$s. Matrix: samples by row, 2 cols.
# State mean hectares for GAP Status 1 protected areas  are $$\alpha +b + \beta_{2} * logGDP.
#First derive posterior samples for the state mean and then calculate summaries of their posterior distributions:
pred_df<-NULL
for (j in 1:4){
  # Derive posterior samples for state means
  StateSamples <- samples$b[,1:49] * NA
  for ( i in 1:49 ) {
    StateSamples[,i] <- samples$alpha + samples$b[,i] + samples$beta[,j] * gdp$logGDP[i]
  }
  # Now calculate mean and standard deviation of the posterior distributions for the state means.
  StatePostmns <- rep(NA,49)
  StatePostses <- rep(NA,49)
  for ( i in 1:49 ) {
    StatePostmns[i] <- mean(StateSamples[,i])
    StatePostses[i] <- sd(StateSamples[,i])
  }
  
  #Plot of posterior means and standard deviations (and compare to the maximum likelihood fit):
  Slope<-mean(samples$beta[,j])
  Intercept<-mean(samples$alpha)
  ppbayes_pred_df<- data.frame(ST_mn=StatePostmns,ST_se=StatePostses, Slope=rep(Slope,49), Intercept=rep(Intercept,49), Gap_Status=rep(j,49),gdp)
  pred_df<-rbind(pred_df,ppbayes_pred_df)
}

head(pred_df)
#View(pred_df)


ggplot(data=pred_df) +
  geom_abline(aes(intercept=Intercept, slope=Slope, color =factor(Gap_Status))) +
    geom_point(mapping=aes(x=logGDP,
                         y=ST_mn,
                         colour=factor(Gap_Status))) +
  geom_linerange(mapping=aes(x=logGDP,
                             ymin=ST_mn-ST_se,
                             ymax=ST_mn+ST_se,
                             colour=factor(Gap_Status))) +
  geom_smooth(mapping = aes(x = logGDP, y = ST_mn, colour = factor(Gap_Status)), method=glm)+
  labs(x="GDP of State (ln(GDP - millions USD))",
       y="mean ln(hectares of protected area) in State j",
       title="Partial pooling: multilevel model, Bayesian")
