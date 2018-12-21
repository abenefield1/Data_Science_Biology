#pred_df<-data.frame(gdp[,-1])
pred_df<-NULL
for (j in 1:4){
  # Derive posterior samples for state means
  StateSamples <- samples$b[,1:49] * NA
  for ( i in 1:49 ) {
    StateSamples[,i] <- samples$alpha + samples$b[,i] + samples$beta[,j] * gdp$logGDP[i]
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
  Slope<-mean(samples$beta[,j])
  Intercept<-mean(samples$alpha)
  ppbayes_pred_df<- data.frame(ST_mn=StatePostmns,ST_se=StatePostses, Slope=rep(Slope,49), Intercept=rep(Intercept,49), Gap_Status=rep(j,49),gdp)
  pred_df<-rbind(pred_df,ppbayes_pred_df)
}

head(pred_df)
View(pred_df)

gh12.6_bayes <-
  ggplot(data=pred_df) +
  geom_abline(aes(intercept=Intercept, slope=Slope, color =factor(Gap_Status))) +
  geom_point(mapping=aes(x=logGDP,
                         y=ST_mn,
                         colour=factor(Gap_Status))) +
  geom_linerange(mapping=aes(x=logGDP,
                             ymin=ST_mn-ST_se,
                             ymax=ST_mn+ST_se,
                             colour=factor(Gap_Status))) +
  #geom_ribbon(mapping=aes(x=logGDP,ymin=ST_mn-(2*ST_se),ymax=ST_mn+(2*ST_se),fill=factor(Gap_Status)),alpha=0.2)+
  geom_smooth(mapping = aes(x = logGDP, y = ST_mn, colour = factor(Gap_Status)), method=glm)+
  #geom_smooth(mapping = aes(x = logGDP, y = ST_mn), method=glm, formula=y~x, se=TRUE)+
  #  geom_ribbon(mapping=aes(x=logGDP,ymin=mean(ST_mn-ST_se), ymax=mean(ST_mn+ST_se)),alpha=0.2)+
  #ylim(0.5,2.03) +
  labs(x="GDP of State (ln(GDP - millions USD))",
       y="mean ln(hectares of protected area) in State j",
       title="Partial pooling: multilevel model, Bayesian")
gh12.6_bayes




head(pred_df)
#lower bounds
pred_df$lower<-


mlp<-pred_df$ST_mn
selp<-pred_df$ST_se
cilp <- mlp - 2 * selp  #lower of 95% CI for linear predictor
ciup <- mlp + 2 * selp  #upper
#cilp <- exp(cillp)       #lower of 95% CI for response scale
#ciup <- exp(ciulp)       #upper
#mp <- exp(mlp)  

pred_df$cilp<-cilp
pred_df$ciup<-ciup
