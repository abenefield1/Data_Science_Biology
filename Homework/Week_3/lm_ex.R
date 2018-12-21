#4. Below are data on total leaf lesions due to viral infection and average nectar volume of flowers (microliters) of deadly nightshade (Atropa belladona). 

#A. Make a pretty figure showing the data, your results, and the 95% confidence interval around the fitted values. Use these data to address the following questions.

nectar.ul <- c(160.44, 41.85, 238.72, 383.67, 84.53, 360.34, 285.57, 267.2, 79.92, 201.09, 189.23, 200.1, 248.79, 378.07, 209.76, 206.58, 66.49, 263.33, 0.19, 191.95, 28.81, 246.87, 297.63, 252.93, 242.9, 393.72, 322.86, 256.12, 300.55, 281.77, 208.44, 200.32, 205.19, 51.89, 336.75, 34.77, 161.78, 96.41, 134.38, 236.24, 73.64, 166.8, 255.32, 185.87, 35.18, 135.43, 124.7, 126.5, 212.36, 126.93, 192.28, 213.94, 281.01, 158.78, 242.59, 237.37, 112.79, 242.23, 119.35, 193.43, 224.37, 184.48, 296.33, 10.63)

lesions <- c(51, 25, 64, 98, 24, 79, 67, 75, 40, 36, 77, 69, 88, 87, 62, 70, 5, 42, 16, 50, 24, 33, 99, 89, 46, 77, 100, 46, 70, 67, 66, 70, 9, 39, 74, 8, 37, 53, 11, 34, 20, 48, 76, 51, 24, 65, 23, 32, 74, 23, 58, 78, 98, 41, 86, 80, 13, 97, 28, 40, 36, 53, 43, 43)

NL<-lm(nectar.ul~lesions)
summary(NL)
plot.new()
plot(lesions,nectar.ul,col=c("magenta","blue"),pch=19, main="Number of Lesions vs. Nectar Volume",xlab="Lesions",ylab="Nectar Volume (microliters)")
abline(NL)
legend(5,345,c("Lesions","Nectar"),col=c("magenta","blue"),pch=19,text.width=15)
pred.frame<-data.frame(lesions=0:100)
pred.c<-predict(NL,int="confidence",newdata=pred.frame)
NL$fitted.values
matlines(pred.frame,pred.c,col=c("black","red","red"),lwd=3,lty=c(1,2,2))

confint(NL)

#B. Do the number of lesions predict nectar volume?

lm(nectar.ul~lesions)
# Yes, as lesions increase by one, nectar volume increases by 2.651 microliters

#C. Theory predicts that nectar volume should INCREASE as the number of lesions increases. Test this hypothesis.
A<-anova(NL)
summary(A)
pval = pt(8.439, df=(length(NL)-1), lower.tail=FALSE) ##not multiplied by two, because we are only interested in upper-tail of t-test.
pval

#D.  Based on your parameter estimates, what is the 95% confidence interval around the fitted values of a plant with 62 lesions? 

pred.frame1<-data.frame(lesions=62)#
pred.p<-predict(NL,int="prediction",newdata=pred.frame1)# p is prediction PREDICTED INDIVIDUALS
pred.p
abline(v=62)
abline(h=pred.p,col=c("black","red","red"),lty=c(1,2,2))

##################################################################
#5. Below is data on a species of wild mustard. Leaf glucosinolate (ug/mg dry weight) and isothiocyanates (ug/mg dry weight) have been implicated in herbivore resistence. Leaf thickness (mm) is the average thickness of leaves based on the average of 10 randomly selected leaves. Using these data, answer the questions below.

leaf.glucosinolate<-c(121.061,151.448,106.522,97.407,110.052,158.652,111.531,109.037,102.374,122.181,144.418,147.539,137.071,124.052,104.414,114.593,108.744,103.743,95.769,76.728,115.18,109.493,88.466,108.89,103.884,113.558,118.682,156.071,129.332,139.775,97.221,146.174,82.809,114.034,107.907,122.952,115.362,94.464,114.42,88.883,102.469,115.418,120.71,123.972,137.832,108.621,90.55,133.821,113.255,104.742,125.003,104.045,124.76,91.614,147.902,112.632,125.443,99.278,116.993,106.76,101.314,100.753,114.221,98.549,92.063,130.005,111.562,98.613,138.934,94.704,110.393,129.742,108.153,63.278,95.036,140.126,129.686,84.854,112.408,130.374,123.597,115.504,92.829,121.988,126.17,126.373,107.206,113.996,102.958,106.608,118.755,96.874,99.928,134.368,95.333,127.166,125.02,105.476,129.876,129.043)

leaf.thickness<-c(1.0822, 0.4109, 0.7796, 0.5038, 0.5937, 1.1615, 0.9131, 0.5326, 0.9548, 0.7634, 0.3503, 0.0282, 0.5981, 0.8078, 0.8208, 0.728, 0.5962, 0.5556, 0.9134, 0.6792, 0.7639, 0.5394, 1.186, 1.098, 1.1487, 0.5253, 0.2114, 0.3222, 0.4034, 0.905, 0.7284, 0.5862, 1.1065, 1.3853, 0.4488, 0.9278, 0.6902, 0.0712, 0.9412, 0.5027, 0.4663, 1.1643, 0.0591, 0.4589, 0.9365, 0.7691, 1.6655, 0.4192, 0.4673, 0.9452, 0.7169, 0.346, 0.5997, 0.4616, 0.6681, 0.324, 0.4646, 1.192, 0.5435, 0.5451, 0.6609, 0.942, 0.926, 0.8393, 0.4795, 0.8782, 0.4605, 0.5265, 0.9938, 0.7201, 0.6742, 0.6268, 0.5456, 0.7823, 1.0237, 0.5825, 0.1897, 1.1867, 0.4108, 0.6239, 0.7908, 0.7012, 1.1844, 0.6381, 0.8714, 0.8657, 1.2253, 0.5606, 0.9779, 0.6655, 1.2221, 0.463, 0.5518, 0.9708, 1.0647, 0.4452, 0.3716, 1.1642, 1.2378, 0.6129)

isothiocyanates<-c(213.87,168.75,158.33,136.02,150.27,185.48,191.24,81.79,145.42,221.28,144.54,111.87,189.54,188.24,237.78,168.22,187.12,167.51,149.08,146.71,202.23,195.73,152.48,265.74,182.03,209.33,63.68,154.61,104.38,244.58,241.03,147.94,127.02,227.57,178.61,169,149.7,130.58,265.17,121.01,69.91,215.77,118.05,178.02,187.34,176.64,187.75,154.19,160.26,210.11,144.93,122.67,171.63,198.49,167.87,139.11,144.35,247.88,140.1,203.57,178.63,225.01,206.39,223.99,113.32,172.42,98.66,235.47,187.04,235.64,117.81,106.95,108.52,169.27,152.54,116.22,142.73,206.96,186.63,169.65,175.01,203.13,200.5,172.03,189.19,252.13,273.45,185.92,195.58,212.39,178.41,145.78,175.61,71.41,107.13,94.92,139.24,265.15,239.5,171.84)

LAR<-c(31.51,18.5,20.88,17.67,18.72,24.11,21.39,2.71,18.39,23.27,25.17,15.52,28.57,24.11,22.19,23.8,25.96,16.45,24.28,16.29,23.67,18.07,21.27,33.05,20.91,25.19,9.92,16.94,8.93,34.68,19.83,20.02,20.2,24.38,21.7,21.46,24.82,10.34,30.77,10.62,12.19,26.69,15.67,20.68,25.7,27.18,25.7,20.11,22.84,31.15,20.12,17.62,16.11,22.67,20.93,15.88,26.24,40.22,23.1,21.78,30.07,26.29,25.68,32.86,8.15,23.85,12.46,36.89,26.06,34.63,8.94,22.9,30.35,25.02,18.78,15.58,16.54,27.3,33.14,24.24,20.33,28.53,24.8,23.7,24.41,37.28,38.63,25.64,27.57,30.38,20.85,13.72,20.59,21.83,19.07,6.75,14.95,34.58,25.64,18.41)

#A. Given the data above of average leaf area removed (mm^2) by caterpillars (LAR), what is the best predictive model of leaf area removed based on the available data?
library(fBasics)
normalTest(leaf.glucosinolate,method="da")
normalTest(leaf.thickness,method="da")
normalTest(LAR,method="da")
## All data are normal, so moving on...
library(car)
mod<-lm(LAR~leaf.thickness+leaf.glucosinolate+isothiocyanates)
vif(mod)
model<-lm.beta(mod)

##no colinearity, sooo moving on...
library(QuantPsyc)
stepAIC(mod)
step(mod) #AIC of 305.2173
extractAIC(mod)
mod1<-lm(LAR~leaf.thickness) #AIC of 368.9545
extractAIC(mod1)
mod2<-lm(LAR~leaf.thickness+leaf.glucosinolate) # AIC of 370.3389
extractAIC(mod2)
mod3<-lm(LAR~leaf.glucosinolate) # AIC of 396.7377
extractAIC(mod3)
mod4<-lm(LAR~leaf.glucosinolate+isothiocyanates) # AIC of 306.6205
extractAIC(mod4)
mod5<-lm(LAR~leaf.thickness+isothiocyanates) # AIC of 303.8411
extractAIC(mod5)
mod6<-lm(LAR~isothiocyanates) # AIC of 304.7932
extractAIC(mod6)
## The model with the lowest AIC was mod5 or: LAR~leaf.thickness+isothiocyanates, but, the next lowest AIC, the full model "mod", was within two points, so it is an equally good model.  I would choose this model.  THe full model is best.

#B. Based on your model, what is the most important predictor for leaf area removed (and on what do you base this)?
Anova(mod)
lm.beta(mod)
## The most important predictors for leaf area removed are isothiocyanates.  The *standardized* regression coefficient is highest for isothiocyanates, and thus it is most important.  Also, it's pretty telling that the models with the lowest AICs all contained that variable.
