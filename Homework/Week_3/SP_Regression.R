install.packages("ggplot2")
library(ggplot2)
# You'll have to save the data file to your desktop without changing the name for the below command to work. Otherwise, you'll have to change the relative path name.
data<-read.csv("~/Desktop/Sam_P_data.csv", header=TRUE)
head(data)


fit<-glm(Proportion_Bold~Time+Treatment+Time:Treatment, data=data)
newd <- data.frame(Time = rep(seq(min(data$Time), max(data$Time), length.out=100),2),
                   Treatment = factor(rep(c("Heated","Unheated"),each=100)))

# For GLMs, there is no ' interval = "confidence" ' option, so we have to
# construct intervals from the standard errors. This is approximate. We use 
# 2 * s.e. here. More accurate intervals are obtained by parametric bootstrap.
preds <- predict(fit,newdata=newd,se.fit=TRUE)
mlp <- preds$fit         #mean of the linear predictor
selp <- preds$se.fit     #se of the linear predictor
cillp <- mlp - 2 * selp  #lower of 95% CI for linear predictor
ciulp <- mlp + 2 * selp  #upper
#cilp <- exp(cillp)       #lower of 95% CI for response scale
#ciup <- exp(ciulp)       #upper
#mp <- exp(mlp)           #mean of response scale
#cilp <- cillp      #lower of 95% CI for response scale
#ciup <- ciulp     #upper
#mp <- mlp 


preds <- cbind(newd,preds,cillp,ciulp,mlp)
preds
head(preds)

ggplot()+
  geom_ribbon(mapping=aes(x=Time,ymin=cillp,ymax=ciulp, fill=Treatment),alpha=0.25,data=preds)+
  geom_point(mapping=aes(x=Time, y=Proportion_Bold, colour=Treatment), alpha=0.75, data=data)+
  geom_line(mapping=aes(x=Time,y=mlp,col=Treatment),data=preds)+
  xlab("Before and After Treatment")+
  ylab("Proportion Bold")+
  scale_y_continuous(breaks=c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))+
  scale_x_continuous(breaks=c(1,2),labels = c("Before","After"))+
  geom_abline(mapping = NULL, data = NULL, slope=0, intercept=c(0.5), size=0.25, linetype = 'dashed', color="red")+ # If you don't like the red 50% mark, you can change the color here to grey
geom_abline(mapping = NULL, data = NULL, slope=0, intercept=c(0.3, 0.4, 0.6, 0.7, 0.8, 0.9), size=0.25, linetype = 'dashed', color="grey")
