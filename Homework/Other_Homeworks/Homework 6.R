#Return to eebbiostatistics@gmail.com by 1:25pm, Friday September 30.
#   on the subject line put    YOURLASTNAME_EXERCISE

#Name:Amy Benefield

# Does learning a second language change brain structure? Machelli et al. (2004) tested 22 native Italian speakers who had learned English as a second language. Proficiencies in reading, writing, and speech were assessed by a proficiency score. Gray matter density was measured in the left inferior parietal region of the brain using a neuroimaging technique, as mm^3 of gray matter per voxel. Read in the Brains.csv table:
brains<-read.csv("Brains.csv")
pro<-brains[,1]
gray<-brains[,2]

#1.  Calculate the correlation between second language proficiency and gray matter density.
r<-cor(gray,pro)
r

#2. Test the null hypothesis when theta = 0
t<-0
n<-22
df<-n-2
SE_r<- sqrt((1-r^2)/df)
t_r<-(r-t)/SE_r
p_value<-pt(t_r,df,lower.tail=FALSE)*2
p_value
  ##P-value is way less than 0.05, so significant. Reject the null hypothesis that theta=0

#3. What is the 83% confidence around your estimate of rho?
CI<-cor.test(pro, gray,conf.level = 0.83)
CI['conf.int']

#4. Build a general linear model of the data where proficency score predicts brain matter. Which factor is all of the error confined to?
model<-lm(gray~pro)
summary(model)
plot(gray~pro)
abline(model)
  ##Slope = 0.03024.  So as proficency score increases by one, gray brain matter increases by 0.03024
  ## Error confined to gray matter?

#5. Theory predicts that the slope should be 0.04. Test this statistical hypothesis.


#6. How much variation in gray matter density does your model explain?

    ## Between 65.31% and 66.96% of the gray matter density is explained by the model - depending on whether you want the normal or adjusted r-squared value.
