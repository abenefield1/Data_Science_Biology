## alpha will be ID ###
df1 <- data.frame(what=c("W","U","T"),alpha=1:3, beta=4:6, place=c("Knox","Den","Hampton"),state=c("TN","CO","VA"),gamma=7:9)
df1

df2 <- data.frame(place=c("Knox","Den","Hampton","Moab"),state=c("TN","CO","VA","UT"),alpha=1:4, delta=c("A","B","C","d"), phi=c("D","E","F","g"))
df2

df3<-data.frame(alpha=1:3, zion=c("G","H","I"),place=c("Knox","Den","Hampton"),state=c("TN","CO","VA"), pi=c("J","K","L"))
df3

df4<-Reduce(function(...) merge(..., all=TRUE), list(df1, df2, df3))
df4

df4[,c(1,4:5)]<-df4[,c(1,4:5)]*2
df4
