# 4 Explore two variables

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# Load data
load("./data/diamonds.RData")


# Now we are comparing 2 variables simultaneously 
# Different questions can arise:

# Are selected variables somehow connected?
# Is there just some random connection between variables?
# Can we see any patterns when we compare variables?
# Are those patterns linear or non-linear?
# Can one variable explain the other variable?
# Is there a strong connection between variables?
# Are variables correlated?
# ...

# From this point main focus is to check 
# how diamond price is related to other variables...


# Scatterplot
# (2 continuous variables comparison)

# Let's figure out how diamond price and carat are related...

# scatterplot (carat VS price, small dataset)
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point()

# Questions:
#   Can you see subgroups of points at certain values of carat?
#   Does this subgroupping apply for the whole dataset?
#   Are those subgroups caused by some third (additional) variable?
#   Can you see a pattern in given scatteplot?
#   Does price variation increases when carat increases?
#   Can we see a linear or non-linear connection between diamond price and carat?
#   Are there any strange outliers (points)?

# scatterplot (carat VS price, big dataset)
diamonds.big %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point()

# paramater for presenting density of points - alpha
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  # geom_point(alpha = 1/1)
  # geom_point(alpha = 1/2)
  # geom_point(alpha = 1/3)
  # geom_point(alpha = 1/5)
  # geom_point(alpha = 1/10)
   geom_point(alpha = 1/20)

# parameter for controling point size
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(size = 3)

# parameter for controling point shape
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  # geom_point(shape = 1)
  # geom_point(shape = 2)
  # geom_point(shape = 3)
  # geom_point(shape = 4)
  # geom_point(shape = 5)
  # geom_point(shape = 10)
  # geom_point(shape = 15)
    geom_point(shape = 21)

# parameter for controling point color
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  # geom_point(color = "red")
  geom_point(color = "deepskyblue2")

# Shapes with border
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(shape = 22, 
             color = "black", 
             fill = "white", 
             size = 1.2, 
             stroke = 1.3)

# parameter for controling point position (jitter or nudge)
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  # geom_point(position = "jitter")
  geom_point(position = "nudge")

# geom_jitter
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  # geom_jitter()
  # geom_jitter(width = 0.5)
  # geom_jitter(height = 0.8)
  geom_jitter(height = 0.8, width = 0.2)

# final scatterplot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price VS carat - scatterplot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/04_scatterplot_price_carat_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 
