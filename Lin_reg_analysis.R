#4. Data on total leaf lesions due to viral infection & avg. nectar volume of flowers (microliters) of deadly nightshade (Atropa belladona). 

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

# The number of lesions predict nectar volume. As lesions increase by one, nectar volume increases by 2.651 microliters. Also, nectar volume increases as the number of lesions increase:
A<-anova(NL)
summary(A)
pval = pt(8.439, df=(length(NL)-1), lower.tail=FALSE) ##not multiplied by two, because only interested in upper-tail of t-test.
pval

# If want to generate 95% confidence interval around the fitted values of a plant with specified # of lesions (62):
pred.frame1<-data.frame(lesions=62)#
pred.p<-predict(NL,int="prediction",newdata=pred.frame1)# p is prediction PREDICTED INDIVIDUALS
pred.p
abline(v=62)
abline(h=pred.p,col=c("black","red","red"),lty=c(1,2,2))
