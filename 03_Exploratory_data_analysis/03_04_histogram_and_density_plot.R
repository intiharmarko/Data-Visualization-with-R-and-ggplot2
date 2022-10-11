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



# Dotplot
# (explore continuous variables)

# carat (weight of a diamond)
diamonds.small %>% 
  ggplot(aes(x = carat)) +
  geom_dotplot(binwidth = 0.007)

# check summary od carat
summary(diamonds.big$carat)

# Questions:
#   Maximum carat value around 3 carats?
#   Why some carat value more typical (around whole numbers)?
#   Why there are some extreme values (outliers)?

# different directions of stacks
diamonds.small %>% 
  ggplot(aes(x = carat)) +
  geom_dotplot(binwidth = 0.0075, stackdir = "up") +
  #  geom_dotplot(binwidth = 0.0075, stackdir = "down") +
  #  geom_dotplot(binwidth = 0.0075, stackdir = "center") +
  scale_x_continuous(breaks = seq(0,5,0.1))

# change other parameters of geom function
diamonds.small %>% 
  ggplot(aes(x = carat)) +
  geom_dotplot(binwidth = 0.0075, 
               stackdir = "up", 
               color = "red", 
               stackratio = 0.5,
               dotsize = 1) +
  scale_x_continuous(breaks = seq(0,5,0.2))

# color aesthetics VS color paramater inside geom
diamonds.small %>% 
  ggplot(aes(x = carat)) +
  geom_dotplot(binwidth = 0.0075, color = "blue")
diamonds.small %>% 
  ggplot(aes(x = carat, color = "blue")) +
  geom_dotplot(binwidth = 0.0075)

# final carat dotplot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = carat)) +
  geom_dotplot(binwidth = 0.001, stackdir = "up", color = "gray20") +
  scale_x_continuous(breaks = seq(0,5,0.25)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  ggtitle("Diamond carat - dotplot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))

ggsave(filename = "./figure/03_dotplot_carat_diamonds.png", plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# Histogram
# (explore continuous variables)

# diamond price (in USD)
diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_histogram()

# change parameters for controlling number of bins
diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_histogram(bins = 45)

diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_histogram(bins = 500)

diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 100) +
  scale_x_continuous(breaks = seq(0,20000,500))

# Questions:
# Why the peice peak at around 800 dollars?
# Why there is another bumb at around 4500 dollars?
# Why there are are almost no diamonds with price around 1500 dollars?

# Let's see what is happening on whole dataset:
diamonds.big %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 100) +
  scale_x_continuous(breaks = seq(0,20000,500))


# final diamond price histogram (for exporting)
diamonds.big %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 100, color = "black", fill = "deepskyblue3") +
  xlab("Price (in USD)") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0,20000,2500)) +
  scale_y_continuous(breaks = seq(0,3000,500)) +
  ggtitle("Diamond price - histogram") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))

ggsave(filename = "./figure/03_histogram_price_diamonds.png", plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 

# Density plot
# (explore continuous variables)

# diamond price (in USD)
diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_density()

# adjust parameters
diamonds.small %>% 
  ggplot(aes(x = price)) +
  geom_density(adjust = 1/5, linetype = "dashed", size = 1.2, color = "red", fill = "gray")

# final diamond price density plot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = price)) +
  geom_density(adjust = 1/5, color = "black", fill = "deepskyblue3") +
  xlab("Price (in USD)") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0,20000,2500)) +
  ggtitle("Diamond price - density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"))

ggsave(filename = "./figure/03_density_price_diamonds.png", plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 
