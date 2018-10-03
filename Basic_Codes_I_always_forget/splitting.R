## Will work for splitting by year

df1 <- data.frame(place=c("Den","Knox","Boul","Den","Knox","Boul"),alpha=1:6, beta=7:12)
df1
#p<-df1$place
list<-split(df1,df1$place)
list
n<-names(list)



Den<-list$Den
Den

