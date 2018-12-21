# Ch 5 Data Transformation:
library(nycflights13)
library(tidyverse)

##################################################################
# Filter rows with filter 5.2
##################################################################
filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)

#To assign a variable and print, wrap assignment in parentheses
(dec25 <- filter(flights, month == 12, day == 25))

# Comparisons
filter(flights, month = 1)
  #Won't work, becuase you need month ==1
filter(flights, month == 1)

# Floating point numbers - issues with approximations:
sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1
  # Both are false, because computer doesn't store infinite digits
#Instead, use near()
near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)

# Boolean operators: and = &, or = |, ! = not
# Number of flights that departed in November or December:
filter(flights, month == 11 | month == 12)
#x %in% y. This will select every row where x is one of the values in y. We could use it to rewrite the code above:
nov_dec <- filter(flights, month %in% c(11, 12))

#Sometimes you can simplify complicated subsetting by remembering De Morgan’s law: !(x & y) is the same as !x | !y, and !(x | y) is the same as !x & !y. For example, if you wanted to find flights that weren’t delayed (on arrival or departure) by more than two hours, you could use either of the following two filters:
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

#If you want to determine if a value is missing, use is.na():
x<-NA
is.na(x)
#filter() only includes rows where the condition is TRUE; it excludes both FALSE and NA values. If you want to preserve missing values, ask for them explicitly:
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

#5.2 Exercises:
##################################################################
# 1.1 Had an arrival delay of two or more hours
filter(flights,arr_delay>=120)

# 1.2 Flew to Houston (IAH or HOU)
filter(flights, dest %in% c("IAH", "HOU")) # or:
filter(flights, dest =="IAH" | dest== "HOU")

# 1.3 Were operated by United, American, or Delta
filter(flights, carrier %in% c("UA", "AA", "DL")) # or:

# 1.4 Departed in summer (July, August, and September)
filter(flights, month %in% c(7:9)) # or:
filter(flights, month %in% c(7, 8, 9)) # or:
filter(flights, month==7 | month==8 | month==9) # or:

# 1.5 Arrived more than two hours late, but didn’t leave late
filter(flights, arr_delay>120 & dep_delay==0)

# 1.6 Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay>=60 & dep_delay - arr_delay > 30)

# 1.7 Departed between midnight and 6am (inclusive)
filter(flights, dep_time==1200 | dep_time<=600)




