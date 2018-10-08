# Ch 3 R for Data Science
library(ggplot2)
library(dplyr)
library(tidyverse)
library(maps)
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

  # To summarize stats:
  ggplot(data = diamonds) + 
    stat_summary(
      mapping = aes(x = cut, y = depth),
      fun.ymin = min,
      fun.ymax = max,
      fun.y = median
    )
  
  # 3.7.1 Exercises:
  ########################################################################
# 1. What is the default geom associated with stat_summary()? How could you rewrite the previous plot to use that geom function instead of the stat function?
  #The default geom is geom_pointrange. the default stat for geom_pointrange() is identity(), so use geom_pointrange(stat = "summary").
  ggplot(data = diamonds) +
    geom_pointrange(
      mapping = aes(x = cut, y = depth),
      stat = "summary",
    )
  
  ggplot(data = diamonds) +
    geom_pointrange(
      mapping = aes(x = cut, y = depth),
      stat = "summary",
      fun.ymin = min,
      fun.ymax = max,
      fun.y = median
    )
  
  # 2. What does geom_col() do? How is it different to geom_bar()?
  ggplot(data = diamonds) + 
    geom_col(mapping = aes(x = cut, y=depth))

  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut))
    #geom_col uses the data as is, using stat_identity. It expects the data to be organized already (i.e., means). The heights of the bars represent values in the data for geom_col. geom_bar makes the height of the bar proportional to the number of cases in each group. It uses stat_count by default: it counts the number of cases at each x position.
    
# 3. Most geoms and stats come in pairs that are almost always used in concert. Read through the documentation and make a list of all the pairs. What do they have in common?
  #geom_bar uses stat_count
  #geom_col uses stat_identity
  #geom_abline
  #geom_bin2d uses stat_bin_2d
  #geom_boxplot uses stat_boxplot
  #geom_contour uses stat_contour
  #geom_count uses stat_sum
  #geom_density uses stat_density (same with 2d)
  #geom_hex uses stat_bin_hex
  #geom_freqpoly and geom_histogram both use stat_bin
  # geom_qq_line uses stat_qq_line
  #geom_qq uses stat_qq
  #geom_quantile uses stat_quantile
  #geom_smooth uses stat_smooth
  # Most stat pairs have the same name as their geom
  
# 4. What variables does stat_smooth() compute? What parameters control its behaviour?
    #Computed variables
  #y: predicted value
  #ymin: lower pointwise confidence interval around the mean
  #ymax: upper pointwise confidence interval around the mean
  #se: standard error
 #It is controlled by the smoothing method used. Can be lm, glm, gam, loess. Auto means smoothing method is chosen based on the size of the largest group
  
# 5. In our proportion bar chart, we need to set group = 1. Why? In other words what is the problem with these two graphs?
    ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, y = ..prop..))
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))
  #Group needs to be defined, because geom_bar will assume the group proportions all equal one. In other words, the proportions are calculated within groups instead of across them. This leaves all bars ==1.

  
  ########################################################################
  # Postition Adjustments:
  ########################################################################
  # Using fill instead of color:
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, colour = cut))
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = cut))

  #But, if we wanted to look at a different variable:
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = clarity))
  # Becomes stacked barplot
  # Using identity, places graph exactly where it belongs. Not useful for bars, because overlap. So, must make transparent:
  ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
    geom_bar(alpha = 1/5, position = "identity")
  ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
    geom_bar(fill = NA, position = "identity")
  
  #position = "fill" works like stacking, but makes each set of stacked bars the same height. This makes it easier to compare proportions across groups.
 ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
#position = "dodge" places overlapping objects directly beside one another. This makes it easier to compare individual values.
 ggplot(data = diamonds) + 
   geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")  
 
 #To avoid missing points on scatterplot due to overlapping, use jitter. This applies small amount of random noise to points, which separates them:
 ggplot(data = mpg) + 
   geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

 # 3.8 Exercises:
 ########################################################################
# 1. What is the problem with this plot? How could you improve it?
   ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
   geom_point()
  # Overplotting at each combination of cty and hwy has made the plot look off. Use jitter
 ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
   geom_point(position = "jitter")

# 2. What parameters to geom_jitter() control the amount of jittering?
 # width: Amount of vertical and horizontal jitter. The jitter is added in both positive and negative directions, so the total spread is twice the value specified here. If omitted, defaults to 40% of the resolution of the data: this means the jitter values will occupy 80% of the implied bins. Categorical data is aligned on the integers, so a width or height of 0.5 will spread the data so it's not possible to see the distinction between the categories.
 #height: same idea as width
 
# 3. Compare and contrast geom_jitter() with geom_count().
 ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
   geom_jitter()
 ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
   geom_count()
 # They roughly achieve the same idea. jitter moves the points slightly to allow visualization of all points (regardless of overlap). In doing so, it loses accuracy. So, it's ok at a large scale, but you lose information on the small scale. geom_count resizes points depending upon number of points in that location. So, you won't see all of the individual points, but if a location has 3 observations, it will have a point 3x larger than a location with only one observation. It has a downside in that large points can cause overlap as well. See below:
 ggplot(data = mpg, mapping = aes(x = cty, y = hwy, color = class)) +
   geom_jitter()
 ggplot(data = mpg, mapping = aes(x = cty, y = hwy, color = class)) +
   geom_count()
 #You can reduce this issue using alpha to create transparency
 ggplot(data = mpg, mapping = aes(x = cty, y = hwy, color = class)) +
   geom_count(alpha=1/2)
 
# 4. What’s the default position adjustment for geom_boxplot()? Create a visualisation of the mpg dataset that demonstrates it.
 # The default is dodge
 ggplot(data = mpg, aes(x = drv, y = hwy, colour = class)) +
   geom_boxplot()
# You can tell it is dodge, because each value of drv has a boxplot for each class. If it were identity, they would overlap:
 ggplot(data = mpg, aes(x = drv, y = hwy, colour = class)) +
   geom_boxplot(position = "identity")
 
 
 ########################################################################
 # Coordinate Systems:
 ########################################################################
 ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
   geom_boxplot()
 # Flip x and y axes
 ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
   geom_boxplot() +
   coord_flip()
# Setting aspect ratio using coord_quickmap:
 nz <- map_data("nz")
 
 ggplot(nz, aes(long, lat, group = group)) +
   geom_polygon(fill = "white", colour = "black")
 
 ggplot(nz, aes(long, lat, group = group)) +
   geom_polygon(fill = "white", colour = "black") +
   coord_quickmap()

 #Polar coordinates:
 bar <- ggplot(data = diamonds) + 
   geom_bar(
     mapping = aes(x = cut, fill = cut), 
     show.legend = FALSE,
     width = 1
   ) + 
   theme(aspect.ratio = 1) +
   labs(x = NULL, y = NULL)
 
 bar + coord_flip()
 bar + coord_polar()

 # 3.9 Exercises:
 ########################################################################
 # 1. Turn a stacked bar chart into a pie chart using coord_polar().
 ggplot(data = diamonds) + 
   geom_bar(mapping = aes(x = cut, fill=cut), position = "fill", , width=1)+
   coord_polar()

 #2. What does labs() do? Read the documentation.
 # Allows you to add labels
 
# 3. What’s the difference between coord_quickmap() and coord_map()?
   
   What does the plot below tell you about the relationship between city and highway mpg? Why is coord_fixed() important? What does geom_abline() do?
   
   ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
   geom_point() + 
   geom_abline() +
   coord_fixed() 
 