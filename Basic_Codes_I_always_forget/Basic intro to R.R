# get R studio.  Not just regular R

# Options for appearance of R script are found under "workspace panes / pane layout." You can change the background color, and whether or not your lines of code wrap.

# Using pound sign converts codes to notes.  This whole file is in note format.

# console: is where the code works, but scripts are where you write code to save as text file.  I always use a script to save changes.

# control l: erases console

# control + enter: enters one line of code to the console at a time (runs that one line of code), and then moves to the next line.

# option + enter: enters one line of code to console, but doesn't move to next line.

# rm(filename):  removes data of particular filename from your environment.  Your environment holds all data that you currently have open in R.  Everything that you could work with.

# rm(list=ls()): removes all data from your environment

# apropos("keyword"): allows you to search for commands based on a keyword.
    # apropos("mean"): generates list of built in functions containing the word mean:
    # ".colMeans"     ".rowMeans"     "colMeans"      "kmeans"        "mean"          "mean.Date"     "mean.default" "mean.difftime" "mean.POSIXct"  "mean.POSIXlt"  "rowMeans"      "weighted.mean"

# ?commandname: opens the help file that explains that command, and lists options that you can use with that command.
  # Ex: ?mean opens the help file for the mean command, and explains the terms and options associated with that command.


#***IMPORTANT NOTE: for all R commands (including opening files), you do not have to explicitly list your options if you want to use the default option.  To determine what the default option is, you can look at the help page for the command

### IMPORTANT NOTE 2: all commands have a specific order in which the options must be written. See the help page for the correct order.

### Important NOTE 3: to create a named anything (vector, matrix, dataframe, datapoint, etc.), you write the name that you want to use, then a backwards arrow, and then the value that you want that data to take.  Ex: pi <- 3.14

### Important NOTE 4:  When naming anything (files or column headers, etc.), stick to simple names (you will have to type them over and over, so you don't want a 30 character name), don't use spaces or special characters.  Only use dots, underscores, or capital letters. Ex: if I would verbally call something, "File Name," I could name it several things in R:
  #FileName
  #File.Name
  #File_Name
  #filename
  # But not: file name, or file, name.

### R is caps sensitive

#################################################################################################
# Opening Files
# there are two ways to open a data file in R.  You can use several types of data files, but I highly reccomend always using .csv files (comma separated values).  You can save an excel file as a .csv. Anyway, to open:

# First way: above the environment pane, there is the, "import dataset" link.You can import directly from an online file, or you can import from a local file on your computer. It opens your file system like any other appication would, and you can navigate to your file. After choosing your file, it opens a second, "options," window. There are many options when opening a file that tell R how to configure your dataset. I will briefly go through those here:
  # Row names: default is automatic, but if your dataset has row names, you can designate the first column as names. Or you can use numbers (indexing) as row names.
  # Headings: yes / no if your data has column headers, click yes.  Otherwise, it will give them column headers : v1, v2, v3, and so on. The defualt depends on the file type.
  # Separator: tells R what separates your values.  For .csv files, that is a comma (default is comma).  There are also tab and whitespace options that are used in text (.txt) files.
  # Decimal tells R what separates the whole number from its decimal points (default is period).  It's almost always a period, but can occasionally be a comma if you're dealing with foreign data (damn Brits! haha).
  # Quote tells R what is text (what should be treated as a character - something that shouldn't be changed). Default is double quote ("), but you can also designate a single quote, (').
# na.strings tells R how to handle NA values or missing values. Defualt is "NA", but you can write whatever you want.
# Strings as factors: yes/no tells R if it should treat your data as factors of the first column or if it should treat your data as linked values that aren't dependent on one another. Default is yes.

# Second way: write a code that specifies all of the same formatting options as listed above in the first way to open a file. To designate the file, you have to type the full pathname ***IMPORTANT NOTE: for all R commands (including opening files), you do not have to explicitly list your options if you want to use the default option.  To determine what the default option is, you can look at the help page for the command.  For opening a data file, the command is: read.csv (for help file, type: ?read.csv).  For the sake of comparison, I will type two codes below that are both correct, but one code will list all of the default options that I don't have to type. The file that I'm opening is a csv with column headers, called: Data_1_County.csv.  The data are factors. I will name my data, "Open_Data".

# THe long version with unneccessary stuff:
Open_Data<-read.csv("D:/Database Project/Data Files/Data_1_County.csv", header = TRUE, sep = ",",quote = "\"", dec = ".", na.strings="NA", stringsAsFactors = TRUE)

# The short version assuming defaults:
Open_Data<- read.csv("D:/Database Project/Data Files/Data_1_County.csv")

# Pros/Cons of two methods to open files.  First way is simple, feels more like what you're used to. Second way is definitely more complicated, but it is really nice to have that code written if you intend to use the same code and file on multiple occasions.  That way, you don't have to go through the options each time.  You can just hit control + enter on the line of code to open it.  What I do, is, the first time I open the file, I use the browser option. Then, I copy that output code in the console to the top of my script.


##################################################################################################
Logicals



 creating vectors / matrices / dataframes, packages, logicals






