set.seed(1001)
d <- expand.grid(account_id=LETTERS[1:26],rep=1:4)
d$Revenue2011 <- runif(nrow(d))
d$Revenue2010 <- rnorm(nrow(d))
View(d)

df1<-d[,-2]
View(df1)

require(plyr)
dat<-ddply(df1,.(account_id),summarise, SumRevenue2011=sum(Revenue2011), Number_Donations = length(account_id), VarianceGiftSize = var(Revenue2011))
View(dat)

A<-df1$Revenue2011[which(df1$account_id == "A")]
var(A)


B<-df1$Revenue2011[which(df1$account_id == "B")]
var(B)

G<-df1$Revenue2011[which(df1$account_id == "G")]
var(G)
