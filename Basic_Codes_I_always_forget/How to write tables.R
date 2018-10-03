# How to write table

### Set working directory if don't want to name it in file ###
setwd("D:/Database Project/Codes/Explanation Codes")

### Set desired number of digits ###
options(digits=22)

### Generating number vectors ###
column_one<-seq(from = 1, to = 100, by=.5)
column_one
column_two<-seq(from = 101, to = 200, by=.5)
column_two


### Cbinding the two vectors ###
CBOUND<-cbind(column_one, column_two)
CBOUND
head(CBOUND)

### Transforming into Dataframe ###
DATAFRAME<-as.data.frame(CBOUND)
DATAFRAME
head(DATAFRAME)

### Transforming Dataframe into a .csv ###
write.csv(DATAFRAME, file="D:/Database Project/Codes/Explanation Codes/DATAFRAME.csv",row.names=FALSE)
  # if you want first column to be an index of numbers, leave row.names=TRUE, or leave it blank (defaults to   index)
write.csv(DATAFRAME, file="D:/Database Project/Codes/Explanation Codes/DATAFRAME_with_Index.csv")

### And finally, to call back the .csv ###
rm(list=ls())  #removes list

# Non-indexed .csv first:
read.csv("D:/Database Project/Codes/Explanation Codes/DATAFRAME.csv")
  # To add .csv to environment as a dataframe:
RECALL_CSV_NON_INDEX<-read.csv("D:/Database Project/Codes/Explanation Codes/DATAFRAME.csv")
  RECALL_CSV_NON_INDEX
  head(RECALL_CSV_NON_INDEX)

# Now indexed .csv:
read.csv("D:/Database Project/Codes/Explanation Codes/DATAFRAME_with_Index.csv")
  # To add .csv to environment as a dataframe:
  RECALL_CSV_with_INDEX<-read.csv("D:/Database Project/Codes/Explanation Codes/DATAFRAME_with_Index.csv")
  RECALL_CSV_with_INDEX
  head(RECALL_CSV_with_INDEX)
  
# Indexed vs. non-indexed:
head(RECALL_CSV_with_INDEX)
head(RECALL_CSV_NON_INDEX)


### Pasting Identifiers ###
test<-paste(column_one, "C", sep="")
### Cbinding the two vectors ###
CBOUND<-cbind(test, column_two)
CBOUND
head(CBOUND)
View(CBOUND)
#C_infront
test<-paste("C",column_one, sep="")


### Transforming into Dataframe ###
DATAFRAME<-as.data.frame(CBOUND)
DATAFRAME
head(DATAFRAME)
View(DATAFRAME)
