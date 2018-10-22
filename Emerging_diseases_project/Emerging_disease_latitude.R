library(ggplot2)
library(dplyr)
library(tidyverse)

EID<-read.csv("EID_Lat_trimTest.csv", header=TRUE)
head(EID)
#View(EID)
EID[,4]<-paste(EID[,3],sep="_",EID[,4])
head(EID)
length(unique(EID[,4]))
#View(EID)
dat<-EID[,c(4,8,10,12,13,14)]
#View(dat)
length(unique(EID[,1]))
colnames(dat)[3]<-paste("Emerging_Disease")
head(dat)
length(dat$path_species[which(dat$event_year=="1979")])
Morbidity<-rnorm(36,mean=600,sd=200)
dat1<-data.frame("Year"=dat$event_year, "Type"=dat$path_type, "Count"=dat$Emerging_Disease, "Latitude"=dat$Avg_Latitude, "Morbidity"=Morbidity)
head(dat1)

dat1$Latitude<-round(dat1$Latitude,digits=-1)
head(dat1)

#Looking at data
#length(unique(dat1$Type))
#length(dat1$Type=="bacteria")
#length(dat1$Type[which(dat1$Type=="virus")])
#length(dat1$Type[which(dat1$Type=="bacteria")])
#filter(dat1, dat1$Type!="bacteria" & dat1$Type!= "virus")
# So, everything that isn't a bacteria or virus will become "other"
#Have to change to character vector
#str(dat1)
#dat1$Type<-as.character(dat1$Type)
#dat1 <- dat1 %>% mutate(Type = replace(Type, Type!="bacteria" & Type!= "virus", "other"))
#head(dat1)
#dat1
#length(unique(dat1$Type))
#Ordering to make cleaning easier
#dat1 <- dat1[order(dat1$Type),] 

#Aggregating on both year
require(plyr)
d3<-ddply(dat1,.(Year),summarise, Count=sum(Count),Latitude=mean(Latitude))
head(d3)

d2<-ddply(dat1,.(Latitude),summarise, Count=sum(Count))
head(d2)

plot(dat1$Count~dat1$Year)
head(d3)
plot(d3$Count~d3$Year)
library(rstan) #for extract function
library(rstanarm)
#bayesFit<-stan_glm(Count~Year+Latitude+Year:Latitude, family=poisson(link="log"), data=d3)
bayesFit<-stan_glm(Count~Year+Latitude+Year:Latitude, family=poisson(link="log"), data=d3)

bayesFit$coefficients
fit<-glm(Count~Year+Latitude+Year:Latitude, family=poisson(link="log"), data=d3)

newd <- data.frame(Year = rep(seq(min(d3$Year), max(d3$Year), length.out=100),3), 
                   Latitude = rep(seq(min(d3$Latitude), max(d3$Latitude), length.out=100),3))
                   
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
head(preds)

ggplot()+
  geom_ribbon(mapping=aes(x=Year,ymin=cilp,ymax=ciup),alpha=0.2,data=preds)+
  geom_point(mapping=aes(x=Year, y=Count, colour=Latitude), alpha=0.5, data=d3)+
  geom_line(mapping=aes(x=Year,y=mp,col=Latitude),data=preds)


#working on model to generate unique lines
ggplot(data=d3)+
  geom_point(mapping = aes(x = Year, y = Count, col = Type))

geom_abline(mapping=aes(x = Year, y = Count),slope=fit$coefficients[2], intercept=fit$coefficients[1])



#Aggregating only on year: total
require(plyr)
dat2<-ddply(dat1,.(Year),summarise, Total=sum(Count),Type=paste(Type, collapse=","))
head(dat2)
length(dat2$Year)
dat2
View(dat2)
plot(dat2$Total~dat2$Year)

ggplot(data = dat2) + 
  geom_smooth(mapping = aes(x = Year, y = Total))

# Have to write to csv and clean data with grep: Find: "bacteria,bacteria" Replace:"bacteria"
#write.csv(dat2, file="EID_Clean.csv",row.names=FALSE)
#length(unique(dat1$Type))
dd<-read.csv("EID_Clean.csv",header=TRUE)
dd
ggplot(data = dd) + 
  geom_line(mapping = aes(x = Year, y = Total))
fit<-glm(Total~Year,data=dd)
coefficients(fit)
summary(fit)

ggplot(data=dd)+
  geom_point(mapping = aes(x = Year, y = Total, col = Type))+
  geom_abline(mapping=aes(x = Year, y = Total),slope=fit$coefficients[2], intercept=fit$coefficients[1])
