#' ---
#' title: "R Tutorial-1 for Statistics"
#' author:
#' - affiliation: Northeastern University
#'   name: Rohit Appandaraju
#' - affiliation: Northeastern University
#'   name: Dehgani Mohammad
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output:
#'   word_document: default
#'   #word_document: null
#' keywords: Data Manipultaion, Plots
#' thanks: Replication files are available on the author's Github account...
#' abstract: This document provides an introduction to Statistics in R, argues for its...
#' ---    
#' 
#' # Checking R
#' 
## ------------------------------------------------------------------------
# Script to check if the Rstudio is working
# The following code should print an output of 4
print(2+2)


#' 
#' # Setting up Directory
#' * Working Directory is a place where all the dynamic process related to the software occurs 
## ------------------------------------------------------------------------
getwd() # Getting current working directory
setwd("C:/Users/arroh")  


#' 
#' # Installing Packages
#' 
## ------------------------------------------------------------------------
# install.packages("dplyr") # dplyr package is used for Data Manipulation
# install.packages("ggplot2") # ggplot2 package is used for building charts
# install.packages("tidyverse") # Used for Data cleaning 
# install.packages("tibble") # this may be required for creating dataframes
# install.packages("tidyr") # Use for cleaning data
# install.packages("readr") # reading excel files

# The below packages are required for doing aesthetics, and sophisticated plots

# install.packages("Rmisc")  
# install.packages("reshape2")
# install.packages("GGally")
# install.packages("rlang")


#' 
#' # Activating Packages
#' ## library() function activates the package eventhough its already installed. So this library function needs to be called each time you start a session inorder to use the functions inside the package
#' 
## ----warning=FALSE-------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(Rmisc)
library(reshape2)
library(GGally)


#' 
#' 
#' # Reading the Dataset
#' 
#' * Dataset in other forms like csv, xlsx, json need to read in r inorder to work with it.
#' 
#' ** The Below function stores the data in table format automatically, similary other forms of data can be read.print(2+2)print(2+2)
## ------------------------------------------------------------------------
DietData<-read.csv("C:/Users/arroh/Downloads/stcp-Rdataset-Diet.csv") # Reading the csv file available in the location mentioned above and  stores the dataset to variable named DietData

# The Overall Description of each variable is available in this link
# https://www.sheffield.ac.uk/mash/statistics2/data

#' 
#' 
#' ## Top 5 rows of the Dataset
## ------------------------------------------------------------------------
head(DietData) # Gives us the top5 rows of all the columns

#' 
#' # Descriptive statistics
#' 
#' *Descriptive statistics are used to describe the basic features of the data in a study. They provide simple summaries about the sample and the measures. Together with simple graphics analysis, they form the basis of virtually every quantitative analysis of data.
## ------------------------------------------------------------------------
# Summary of the Dataset
summary(DietData) 
## Gives us the overall description of the dataset like mean, median, quartiles, ranges which can be useful for determining skewness, distribution


# Finding Average of specific Column
mean(DietData$Height) 
# mean() is an inbuilt function to calculate the mean
# $ operator is used to pick the particular column from the dataset

# Similarly build in functions are available for calculating median,mode, range, max,min seperately


#' 
#' # Using dplyr
#' ## 'dplyr' is an R package for working with structured data both in and outside of R. dplyr makes data manipulation for R users easy, consistent, and performant. With dplyr as an interface to manipulating Spark DataFrames, you can: Select, filter, and aggregate data.
#' 
#' # dplyr verbs	Description
#' # select()	select columns
#' # filter()	filter rows
#' # arrange()	re-order or arrange rows
#' # mutate()	create new columns
#' # summarise()	summarise values
#' # group_by()	allows for group operations in the "split-apply-combine" concept
## ------------------------------------------------------------------------
# Using Mutate() -Computes and adds new variable. Preserves existing variables.

DietData<-DietData%>%
  mutate(Weightloss=pre.weight-weight6weeks) # Adds a new column named Weightloss with data calculated as pre.weight-weight6weeks
 
# Finding aggregate for filtered data
# selects the gender Age Height, Weightloss,Diet columns
# and then filters the rows with Age column greater than 30
# and then groups Diet and gives aggreagate of mean (height) in the data 
DietData %>%
  select(gender,Age,Height,Weightloss,Diet) %>%
  filter(Age<30) %>%
  group_by(Diet) %>%
  summarise(mean(Height))


#' 
#' 
#' 
#' # Charts: using ggplots
#' ## ggplot() function is used to create a plot, the type of plot needs to added along the ggplot() function like ggplot() +geom_point(), axes and data details can be included as arguments inside this function
#' # Histogram
#' ## A histogram is an accurate representation of the distribution of numerical data. It is an estimate of the probability distribution of a continuous variable (quantitative variable)
#' ## geom_histogram() is used to create histogram plots, arguments include bins,color,size etc- these can be seen in help
#' ## labs() function is used to provide labels for each axis in the plot, this is a general option avaiable for ggplot() function
## ------------------------------------------------------------------------
# Distribution of Height Column in the Dataset with Number of bins as 30
# Gives a Histogram plot of Height variable with bin colour as white and borders as Black, labels are given for x axis as Height(cm)
p1<-ggplot(data=DietData,aes(x=Height))+
  geom_histogram(bins = 30,fill="White",color="Black") +
  labs(x="Height (cm)",caption="Histogram Plot for Height Variable")
p1

#' 
#' 
#' # ScatterPlots
#' ## A scatter plot is a two-dimensional data visualization that uses dots to represent the values obtained for two different variables - one plotted along the x-axis and the other plotted along the y-axis.
#' ## Function used for it is geom_point(), the size, shape of the points can be added as arguments
#' ##geom_smooth() function adds a line that passes through most points in the graph
## ------------------------------------------------------------------------
# Scatter plot between Age and Height with Colors showing each diet type and points showing the data points of both Age and Height, geom_smooth removes the fit line
p2<-ggplot(data = DietData,aes(x=Age,y=Height,color=Diet))+
  geom_point(aes(size=Weightloss),shape=10) +
  geom_smooth(se=FALSE)+
    labs(caption="Scatterplot Height vs Age")
p2



#' # Scatterplot Matrix
#' ##A scatter plot matrix is table of scatter plots. Each plot is small so that many plots can be fit on a page. When you need to look at several plots, such as at the beginning of a multiple regression analysis, a scatter plot matrix is a very useful tool.
#' ## It shows the relation between 2 continous variables, Color and size can be included in the graph to incorporate some extra variables in the plot - Color can be for categorical and size for continuous
#' 
#' 
## ------------------------------------------------------------------------
# Corvar variable contains the following columns -  filter data consisting of only numerical variables
CorVar<- DietData %>%
  select(gender,Age,Height,Weightloss)
# Creates a multiple Scatterplot between each variable in the same page
# Along with  scatter plots we can also see the correlation values 
ggpairs(CorVar)

# From this we can get the relation of each numerical variable with other

#' 
#' 
#' # Boxplot
#' ## The box plot (a.k.a. box and whisker diagram) is a standardized way of displaying the distribution of data based on the five number summary: minimum, first quartile, median, third quartile, and maximum.
## ------------------------------------------------------------------------

DietData$Diet<-as.factor(DietData$Diet) # making the datatype of the column Diet as factor- so this variable will be considered as categorical variable with levels which refer to the number of categories
# Below gives the Box Plot between each diet type in x axis with Data as Weightloss
p3<-ggplot(data=DietData,aes(x=Diet,y=Weightloss,color=Diet)) +
  geom_boxplot() +
    labs(caption="Boxplot By Diet Types")
# From the plots we can check if the data has outlier, the range,median and spread of the data
  
p3

#' 
#' # Barplots
#' ## A barplot (or barchart) is one of the most common type of graphic. It shows the relationship between a numeric variable and a categoric variable.
## ------------------------------------------------------------------------
# The below script gives the bar plot of Weightloss in y axis vs Diet type in x axis 
ggplot(data=DietData,aes(x=Diet,y=Weightloss,fill=Diet))+
  geom_bar(stat = "identity")+
    labs(caption="BarPlot Diet vs WeightLoss")# Stat =identity shows the values in the column(if more values it takes count by group with x classes) # Stat = count,bin shows the count of each values in each class in x axis

# Bar plot of Diet type variable in x axis and y axis showing the aggregate count of rows with labels xaxis and title
p4<-ggplot(data=DietData,aes(x=Diet))+
  geom_bar(stat = "count") +
  labs(x="Diet Type",caption="Barplot DietType vs Count")

#' 
#' # Subplots
#' 
## ------------------------------------------------------------------------
# Combining Plots in same page
# multiplot function can be used, the plots are arranged in order you want and number of columns they need to divided needs to mentioned else it takes default value =1
multiplot(p1,p2,p3,p4,cols=2)


#' 
#' 
#' # Heatmaps
#' ##A heatmap is a graphical representation of data that uses a system of color-coding to represent different values. Heatmaps are used in various forms of analytics but are most commonly used to show user behaviour on specific webpages or webpage templates. But this can be used in statistics to show the occurence, proximity of the data/ correaltion between variables 
## ------------------------------------------------------------------------
# Correlation Heatmap: The heatmap shows th correlation in better fashion with color intensity showing the strength of the correlation
# Correlation can be found only for numeric datatye variables

# as.numeric(variable) is used to convert variable to numeric datatype, similarly as.character, as.factor etc can be used to change the datatype
# DietData$Diet<-as.numeric(DietData$Diet)

# Corvar variable contains the following columns
CorVar<- DietData %>%
  select(gender,Age,Height,Weightloss)

# Finds the correlation between each variable
# cor() function creates a matrix of correlation values between each variable inside the variable specified
# round() func is used to round off value, number of decimals needs to be mentioned upto which the value needs to rounded off
cormat<-round(cor(CorVar),2)

# melt function converts the x axis and y axis in a matrix to seperate variables x1,x2 and values in value varialbe

cormat<-melt(cormat)

# Heat map plot showing correlation values in the boxes
# geom_tile is used to create a heatmap plot
ggplot(data = cormat,aes(x=Var1,y=Var2,fill=value))+
   geom_tile() +
   geom_text(aes(x=Var1,y=Var2,label=value))+
    labs(caption="Correlation Heatmap")


#' 
#' 
#' # zooming
#' ## This is used to zoom through a plot to see the zoomed part in a better way, the faster way to do this is by restricting the limits of axis on the value which needs to be zoomed
#' 
## ------------------------------------------------------------------------
# Reading Twitter Data
TwitData<-read.csv("C:/Users/arroh/Desktop/Data Mining/Choun Chou/Assignments/Assignment 1/M01_quasi_twitter.csv") # this one is for practice

# Counting rows by a column
AccCreat<-TwitData %>%
  group_by(created_at_year) %>%
  tally() # Count function

# Limits the range of the data to plot
ggplot(data = AccCreat,aes(x=created_at_year,y=n)) +
  geom_line() +
  xlim(2007,2010)


#' 
#' 
#' 
#' 
