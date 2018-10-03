df<-data.frame(alpha=1:3,beta=4:6, gamma=7:9, delta=10:12, place=c("Knox","Den","Boulder"))
df
colnames(df)[1:2]<-paste("Names",colnames(df)[1:2],sep="_")
colnames(df)[3:5]<-paste("Example",colnames(df)[3:5],sep="_")
df

colnames(df)[3]<-"Example"
