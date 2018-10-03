a<-Change

library(stringr)
a$State_FIPS<-str_pad(a$State_FIPS, 2, pad = "0")
View(a)
a$County_FIPS<-str_pad(a$County_FIPS,3,pad="0")
View(a)

name<-within(a, County_ID_FIPS <- paste(State_FIPS, County_FIPS, sep=''))
View(name)
