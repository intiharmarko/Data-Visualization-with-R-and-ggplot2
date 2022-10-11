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



# Fitting a linear regression model 

# idea: build a linear model where price is dependant variable and carat is independant variable
#       we can then predict price of a diamond based on the carat value
#       caution there is no linear relationships between price and carat!!
#       this will be addressed later
#       we are fitting linear model on a small diamond sample

# model fit:
lm.model.price <- lm(formula = price ~ carat, data = diamonds.small)
lm.model.price

# generate points for drawing a regression line

diamonds.small %>% # carat values range
  summarise(`carat min` = min(carat),
            `carat max` = max(carat))

df.lm.line <- data.frame(carat = seq(0,3,0.01)) # df where we store carat and predicted price
df.lm.line <- df.lm.line %>% 
  mutate(`price predicted` = predict(lm.model.price, .)) # predict price

# scatterplot (carat VS price with regression line)
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_line(data = df.lm.line, 
            aes(x = carat, y = `price predicted`), 
            color = "blue",
            size = 1.3)


# use geom_smooth - for adding regression model
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y ~ x')

# without confidence intervals around smoothed line
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# use general linear model (glm)
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = "auto", se = TRUE, color = "red")
# (gam - general additive model)



# Transforming variables
#   try to use logarithmic transformation on price and/or on carat
#   maybe we can obrain linear relation with transformed variables

# transformation manually (natural logarithm ln=log):
diamonds.small <- diamonds.small %>% 
  mutate(`price (log)` = log(price),
         `carat (log)` = log(carat))

# log price VS carat - not linear relation :(
diamonds.small %>% 
  ggplot(aes(x = carat, y = `price (log)`)) +
  geom_point()

# price VS log carat - not linear relation :(
diamonds.small %>% 
  ggplot(aes(x = `carat (log)`, y = price)) +
  geom_point()

# log price VS log carat - seems like a linear relation :)
diamonds.small %>% 
  ggplot(aes(x = `carat (log)`, y = `price (log)`)) +
  geom_point()

# let's fit a model on this transformed variables
diamonds.small %>% 
  ggplot(aes(x = `carat (log)`, y = `price (log)`)) +
  geom_point() +
  geom_smooth(method = "lm")


# instead of actual transforming variables use scale transformation!
# (log10 in integrated for scales transformation)
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  scale_y_log10() 


# final scatterplot (for exporting)
diamonds.big %>% 
  filter(carat <= 3) %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5, color = "red") +
  scale_x_log10(breaks = seq(0,5,0.5)) +
  scale_y_log10(breaks = seq(0,20000,5000)) +
  xlab("Carat transformed with log10") +
  ylab("Price (in USD) transformed with log10") +
  ggtitle("Diamond price VS carat with smoothed line - scatterplot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/04_scatterplot_logprice_logcarat_smoothed_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 

