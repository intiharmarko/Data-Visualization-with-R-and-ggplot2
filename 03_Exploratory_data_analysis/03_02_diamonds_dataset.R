# 3 Expoloratory Data Analysis (EDA)

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)


# Diamonds dataset

# Check dataset
help("diamonds") # dataset description

str(diamonds) # dataset structure

nrow(diamonds) # rows - cases
ncol(diamonds) # columns - variables

View(diamonds) # view table
head(diamonds) # print first 6 rows

summary(diamonds)


# Save data tables for further usage
set.seed(123)

# original dataset
diamonds.big <- diamonds 

# smaller dataset
diamonds.small <- diamonds.big %>% dplyr::sample_n(size = 5000, replace = F)

# tiny dataset
diamonds.tiny <- diamonds.big %>% dplyr::sample_n(size = 500, replace = F)

# Save data
save(list = c("diamonds.big", "diamonds.small", "diamonds.tiny"), file = "./data/diamonds.RData")



# Questions to answer?
#   (Initial set of questions)
#   (Will modify questions throughout the course)

# What does the distribution of each variable look like?
# Can you see any interesting or strange patterns in given distributions?
# Are there any strange values?
# Are there any typical values or most common ones?
# What is the variation of values of each variables?
# Can you see any relationships among given variables?
# ... (to be continued)

