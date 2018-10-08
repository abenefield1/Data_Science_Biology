# Ch 3 R for Data Science
library(ggplot2)
library(dplyr)
library(tidyverse)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# Reusable template:
ggplot(data = <DATA>) + 
  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

ggplot(data=mpg)
str(mpg)
length(mpg)
# 11 columns, 234 observations (rows)
head(mpg)
View(mpg)
# drv is rear, front wheel, or 4 wheel drive
# Scatterplot of hwy vs. cyl (y vs. x)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))

# Scatterplot of class vs. drive:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = class))
# Not useful, because we're comparing two categorical variables

# Organizing points by color. To identify outliers
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
  # 2 seaters, red, are outliers

# Same idea, but by size. Size not reccomended for categorical variables:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

#Transparency:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

#Shapes:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
  # No SUVS b/c ggplot will only use 6 shapes at a time

# To change color of points:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")


########################################################################
# 3.3.1 Exercises:
########################################################################
# 1.What’s gone wrong with this code? Why are the points not blue?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
  # Colors aren't blue, because we've left the color code inside the aesthetics parentheses. Aesthetics only control variables other than x and y.

# 2. Which variables in mpg are categorical? Which variables are continuous? (Hint: type ?mpg to read the documentation for the dataset). How can you see this information when you run mpg?
?mpg
head(mpg)
  # Categorical = manufacturer, model, year, cylinder, transmission, drive, fl (fuel type), and class. Continuous = displ (engine displacement in liters), cty (city miles per gallon), and hwy (highway miles per gallon)

# 3. Map a continuous variable to color, size, and shape. How do these aesthetics behave differently for categorical vs. continuous variables?
# Color:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty))
# Size:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = cty))
# Shape:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty))
  # Color and size work. Shape doesn't work, because shapes are categorical. You can't map a continuous variable with a categorical symbol. Color can be both. For continuous variables, it uses a color scale. Size is best for continuous, and the points again are on a scale.

# 4. What happens if you map the same variable to multiple aesthetics?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty, size=cty))
  # It works, but it's a bit redundant

# 5. What does the stroke aesthetic do? What shapes does it work with? (Hint: use ?geom_point)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, stroke = cty))
# Stroke is sort of like size

# 6. What happens if you map an aesthetic to something other than a variable name, like aes(colour = displ < 5)?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty<5))
  # with a logical, it plots those in which it is false?


########################################################################
# Facets:
########################################################################
# On one variable
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
  # facet wrap variable should be discrete
# On 2 variables:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# 3.5.1 Exercises:
########################################################################
# 1. What happens if you facet on a continuous variable?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cty, nrow = 2)
  # It tries to turn the continuous variable into a categorical variable

# 2. What do the empty cells in plot with facet_grid(drv ~ cyl) mean? How do they relate to this plot?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
   geom_point(mapping = aes(x = drv, y = cyl))
# The plots are empty, because there are no variables the fall in those categories. For example, there are no 7 cylinder vehicles with 4 wheel drive. There are no rear wheel vehicles with 7 cylinders

# 3. What plots does the following code make? What does . do?
ggplot(data = mpg) + 
   geom_point(mapping = aes(x = displ, y = hwy)) +
   facet_grid(drv ~ .)
 
 ggplot(data = mpg) + 
   geom_point(mapping = aes(x = displ, y = hwy)) +
   facet_grid(. ~ cyl)
# They just create plots according to that one variable. The only difference is whether the plots are separated on the y axis (drv~.) or the x axis (.~drv)
 
# 4. Take the first faceted plot in this section:
 ggplot(data = mpg) + 
   geom_point(mapping = aes(x = displ, y = hwy)) + 
   facet_wrap(~ class, nrow = 2)
# What are the advantages to using faceting instead of the colour aesthetic? What are the disadvantages? How might the balance change if you had a larger dataset?
  # For a small number of variables, this is fine. It is more informative and easier to read than   on one plot. For a large number of variables, this wouldn't work. You wouldn't want to look     through 20 different plots. Advantages: more descriptive by variable. Disadvantage: more complex
  
# 5. Read ?facet_wrap. What does nrow do? What does ncol do? What other options control the layout of the individual panels? Why doesn’t facet_grid() have nrow and ncol arguments?
  # nrow/ncol = number of rows and columns in the figure. You can fix the scales. Important for not misleading readers. The number of rows and columns can allow you to manipulate the overall structure of your plot.
 
# 6. When using facet_grid() you should usually put the variable with more unique levels in the columns. Why?
 ggplot(data = mpg) + 
   geom_point(mapping = aes(x = displ, y = hwy)) + 
   facet_grid(drv ~ manufacturer)
  #Because it's easier to keep track of the x axis?
 
 
 ########################################################################
 # Geometric Objects:
 ########################################################################
 ggplot(data = mpg) + 
   geom_point(mapping = aes(x = displ, y = hwy))
 # To display smooth trend
 ggplot(data = mpg) + 
   geom_smooth(mapping = aes(x = displ, y = hwy))
 
 #Aesthetics to change line type
 ggplot(data = mpg) + 
   geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

 # Groups:
 ggplot(data = mpg) +
   geom_smooth(mapping = aes(x = displ, y = hwy))
 
 ggplot(data = mpg) +
   geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
 
 ggplot(data = mpg) +
   geom_smooth(
     mapping = aes(x = displ, y = hwy, color = drv),
     show.legend = FALSE
   )
# To add multiple geoms:
 ggplot(data = mpg) + 
   geom_point(mapping = aes(x = displ, y = hwy)) +
   geom_smooth(mapping = aes(x = displ, y = hwy))
 # To avoid duplication in code and hold the same values:
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
   geom_point() + 
   geom_smooth()
# TO display different aesthetics in different layers:
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
   geom_point(mapping = aes(color = class)) + 
   geom_smooth()

 # TO override the global data object for one layer:
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
   geom_point(mapping = aes(color = class)) + 
   geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

  # 3.6.1 Exercises:
 ########################################################################
 # 1. What geom would you use to draw a line chart? A boxplot? A histogram? An area chart?
  # geom_abline, geom_boxplot, geom_bar
 
 # 2. Run this code in your head and predict what the output will look like. Then, run the code in R and check your predictions.
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
    geom_point() + 
    geom_smooth(se = FALSE)
 
 # 3. What does show.legend = FALSE do? What happens if you remove it? Why do you think I used it earlier in the chapter?
 ggplot(data = mpg) +
   geom_smooth(
     mapping = aes(x = displ, y = hwy, color = drv)
   ) 
    # The default for color is to show a legend, so to hide it, you have to explicitly tell the code so. Earlier in the chapter, we were comparing color to group. Group's default is to not show a legend
 
 #  4. What does the se argument to geom_smooth() do?
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
   geom_point() + 
   geom_smooth(se = FALSE)
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
   geom_point() + 
   geom_smooth(se = TRUE)
  # The se argument tells ggplot whether or not to display the standard error as a shaded area around the line
 
 #  5. Will these two graphs look different? Why/why not?
 ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
    geom_point() + 
    geom_smooth()
  
  ggplot() + 
    geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
    geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
  # They look the same. The first code is, "better," because it is more universal. It lists the mapping command in the ggplot parentheses. The second code lists it in the aesthetics. They would be different if the variables were different.
  
# 6. Recreate the R code necessary to generate the following graphs.
  ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) + 
    geom_point() +
    geom_smooth(se=FALSE)
 
  ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
    geom_point() +
    geom_smooth(mapping = aes(x = displ, y = hwy, group = drv),se=FALSE)
 
  ggplot(data = mpg, mapping = aes(x=displ, y=hwy, color=drv)) +
    geom_point() +
    geom_smooth(mapping = aes(x = displ, y = hwy, group = drv),se=FALSE)
 
  ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
    geom_point(data = mpg, mapping = aes(x = displ, y = hwy, color=drv)) +
    geom_smooth(se=FALSE)
  
  
  ggplot(data = mpg, mapping = aes(x=displ, y=hwy, color=drv, linetype=drv)) +
    geom_point() +
    geom_smooth(mapping = aes(x = displ, y = hwy, group = drv),se=FALSE)
  
  ggplot(data = mpg, mapping = aes(x=displ, y=hwy, color=drv)) +
    geom_point()

  
  ########################################################################
  # Statistical Transformations:
  ########################################################################
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut))
#can use geoms and stats interchangeably:
  ggplot(data = diamonds) + 
    stat_count(mapping = aes(x = cut))

  # To override default stat
  demo <- tribble(
    ~cut,         ~freq,
    "Fair",       1610,
    "Good",       4906,
    "Very Good",  12082,
    "Premium",    13791,
    "Ideal",      21551
  )
  
  ggplot(data = demo) +
    geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")  

  # For proportions:
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
  