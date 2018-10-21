# R-Tutorial-for-Statistics
R Tutorial for Statistics (Beginners)

October 17, 2018

## Overview
This course will give you a feel for the complete data analysis process in R - from importing and manipulating data through visualization. You'll see how using code to capture the analysis pipeline leads to deliverables that are documented, easily reproduced and easily automated.
We'll focus on tools in the tidyverse a core set of R packages that are designed to be easy to learn, easy to use, and solve the most frequent data analysis problems.
During the course, we'll alternate between me introducing a new concept with some examples, and you applying that concept on your own. You should expect to spend at least 50% of your time writing code in RStudio on your own laptop.

## Getting Started with R and RStudio
On your first session you'll focus on getting comfortable writing code and executing it in RStudio. We'll take things slow as you learn to navigate RStudio, learn some syntax rules, and how to get help when you get stuck. Along the way you'll meet R's most ubiquitous objects for holding data and learn to import data whether it is a CSV, SPSS or Excel data file.

### By the end of the day you will be able to:
•	Open a notebook in RStudio and execute the code chunks in it
•	Install and load an R package
•	Open the help page for a function or built-in dataset
•	Identify the components of an R function: the function name and arguments
•	Assign the results of a function to a new variable
•	Get an overview of a dataset that is in a data frame or tibble
•	Import CSV, SPSS and SAS data files




## Visualization and Manipulation of Data
We'll start the day with visualization of data in R using the package ggplot2. You'll see how ggplot2 provides a framework for thinking about plots, which means you only need to learn one template to make almost any plot you can imagine. To practice, you'll make some of the most common kinds of data visualizations: histograms, scatterplots and Box plots, and continue building your skills as we continue through data manipulation.

In the afternoon we'll focus of the most common types of data manipulation: extracting subsets from data, adding new variables and creating grouped summaries. You'll find that doing this is quite intuitive using the dplyr package which boils down manipulation into a set of verbs like: filter(), mutate() and summarise(). Occasionally, data won't come in quite the right shape for manipulation or visualization you want to do, so we'll also talk about the key parts of the tidyr package that help to reshape not not-so-tidy data.

### By the end of the day you will be able to:
•	Create plots in ggplot2 to explore data
•	Select variables and filter observations to subset data
•	Add new variables, and transform variables
•	Create grouped summaries of data
### Software Requirements
You'll need to bring a laptop with R and RStudio installed. In addition, you'll want to install the following packages:
install.packages(c("tidyverse", "rmarkdown", "gapminder", "usethis"))
If you've installed the tidyverse before, re-installing it may not update all the component packages, in which case run,
tidyverse::tidyverse_update()
to identify any out-of-date packages and follow the instructions to update them.
Don't forget to bring your power cable!
