#Treating rickettsia as bacteria

library(ggplot2)
library(dplyr)
library(tidyverse)

EID<-read.csv("Emerging_diseases_project/EID_Lat_trimTest.csv", header=TRUE)
head(EID)
View(EID)
EID[,4]<-paste(EID[,3],sep="_",EID[,4])
head(EID)
length(unique(EID[,4]))
dat<-EID[,c(-2,-10,-13)]
#View(dat)
length(unique(EID[,1]))
colnames(dat)[6]<-paste("Emerging_Disease")
head(dat)
length(dat$path_species[which(dat$event_year=="1979")])
Morbidity<-as.integer(rnorm(36,mean=600,sd=200))
dat1<-data.frame("Year"=dat$event_year, "Type"=dat$path_type, "Latitude"=dat$Avg_Latitude, "Morbidity"=Morbidity)
head(dat1)

#Looking at data
length(unique(dat1$Type))
length(dat1$Type=="bacteria")
length(dat1$Type[which(dat1$Type=="virus")])
length(dat1$Type[which(dat1$Type=="bacteria")])
filter(dat1, dat1$Type!="bacteria" & dat1$Type!= "virus")
# So, everything that isn't a bacteria or virus will become "other"
#Have to change to character vector
dat1$Type<-as.character(dat1$Type)
dat1 <- dat1 %>% mutate(Type = replace(Type, Type=="rickettsia", "bacteria"))
length(unique(dat1$Type))
dat1 <- dat1 %>% mutate(Type = replace(Type, Type!="bacteria" & Type!= "virus", "other"))
head(dat1)
dat1
length(unique(dat1$Type))

#Aggregating on both year and type
library(dplyr)
plot(dat1$Morbidity~dat1$Year)
library(rstan) #for extract function
library(rstanarm)
bayesFit<-stan_glm(Morbidity~Year+Type+Year:Type,family=poisson(link="log"), data=dat1)
bayesFit$coefficients
fit<-glm(Morbidity~Year+Type+Year:Type,family=poisson(link="log"), data=dat1)

newd <- data.frame(Year = rep(seq(min(d3$Year), max(d3$Year), length.out=100),3), 
                   Type = factor(rep(c("bacteria","other","virus"),each=100)))

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
  geom_ribbon(mapping=aes(x=Year,ymin=cilp,ymax=ciup, fill=Type),alpha=0.2,data=preds)+
  geom_point(mapping=aes(x=Year, y=Morbidity, colour=Type), alpha=0.5, data=dat1)+
  geom_line(mapping=aes(x=Year,y=mp,col=Type),data=preds)

