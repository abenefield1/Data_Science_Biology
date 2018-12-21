colnames(dat)[6:9]<-c("G1","G2","G3","G4")
head(dat)

#Round in select columns
dat[,6:9]<-round(dat[,6:9],digits=3)

#Replace 0 with NA in select multiple columns
dat[, 6:9][dat[, 6:9] == 0] <- NA
head(dat)