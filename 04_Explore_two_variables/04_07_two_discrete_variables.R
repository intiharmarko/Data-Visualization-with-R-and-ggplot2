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


# Rug plot
# (2 continuous variables comparison)

# add rug plot to our scatterplot
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_rug()

# parameter for apperance of rug (lines) - inside / outside the box
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_rug(outside = T) + coord_cartesian(clip = "off")
  #geom_rug(outside = F)

# parameter for apperance of rug (lines) - position
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  # geom_rug(sides = "l") # only left side
  # geom_rug(sides = "r") # only right side
  # geom_rug(sides = "b") # only bottom side
  # geom_rug(sides = "t") # only top side
  # geom_rug(sides = "tb") # top and bottom side
  # geom_rug(sides = "lb") # left and bottom side
  geom_rug(sides = "lrtb") # all sides
  
# parameter for length of lines
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_rug(length = unit(0.25, "cm"))

# controling line jitter and transparency
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_rug(position = "jitter", alpha = 1/3)


# final rug plot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(position = "jitter",
             color = "black",
             alpha = 1/10,
             size = 1.5) +
  geom_rug(sides = "rb", color = "gray20", alpha = 1/3) +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(breaks = seq(0,20000,5000)) +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price VS carat - scatterplot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/04_rugplot_price_carat_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# Continuous bivariate distribution
# (2 continuous variables comparison)

# Let's check relation between diamond price and diamond dimensions
# Here we are trying to visualize bivariate distributions

# price VS diamond length (geom_bin2d ~ heatmap of 2 dimensional bin counts)
diamonds.small %>% 
  ggplot(aes(x = x, y = price)) +
  geom_bin2d()

# controlling the number of bins
diamonds.small %>% 
  ggplot(aes(x = x, y = price)) +
  #  geom_bin2d(bins = 10)
  #  geom_bin2d(bins = 100)
  geom_bin2d(bins = 500) 

# controlling the binwidth (2 dimensions!!!)
diamonds.small %>% 
  ggplot(aes(x = x, y = price)) +
  # geom_bin2d(binwidth = c(1,5000))
  # geom_bin2d(binwidth = c(0.25,500))
  geom_bin2d(binwidth = c(0.1,100))

# change color divergence (use viridis color scale)
diamonds.small %>% 
  ggplot(aes(x = x, y = price)) +
  geom_bin2d(binwidth = c(0.25,250)) +
  scale_fill_viridis_c()


# final heatmap of 2 dimensional bin counts (for exporting)
diamonds.big %>% 
  ggplot(aes(x = x, y = price)) +
  geom_bin2d(binwidth = c(0.1,200)) +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_y_continuous(breaks = seq(0,20000,5000)) +
  scale_fill_viridis_c() +
  xlab("Diamond length (in mm)") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price VS diamond length - heatmap 2D bin count") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

ggsave(filename = "./figure/04_heatmap2dbin_price_x_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# price VS diamond width (geom_density2d ~ 2 dimensional density plot)
diamonds.small %>% 
  ggplot(aes(x = y, y = price)) +
  geom_density2d()

# combining with scatterplot
diamonds.small %>% 
  ggplot(aes(x = y, y = price)) +
  geom_point() +
  geom_density2d()

# using stat_density_2d and fill mapping
diamonds.small %>% 
  ggplot(aes(x = y, y = price)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon")

# viridis color scale
diamonds.small %>% 
  ggplot(aes(x = y, y = price)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  scale_fill_viridis_c()

# final 2 dimensional density plot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = y, y = price)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  scale_x_continuous(breaks = seq(0,20,0.5)) +
  scale_y_continuous(breaks = seq(0,20000,1000)) +
  scale_fill_viridis_c(option = "plasma") +
  xlab("Diamond width (in mm)") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price VS diamond width - 2D density plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

ggsave(filename = "./figure/04_2ddensity_price_y_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# price VS diamond depth (geom_hex ~ Hexagonal heatmap of 2d bin counts)
install.packages("hexbin")
library(hexbin)

diamonds.small %>% 
  ggplot(aes(x = z, y = price)) +
  geom_hex()

# controlling the number of bins
diamonds.small %>% 
  ggplot(aes(x = z, y = price)) +
  #  geom_hex(bins = 10)
  #  geom_hex(bins = 100)
  geom_hex(bins = 500) 

# controlling the binwidth (2 dimensions!!!)
diamonds.small %>% 
  ggplot(aes(x = z, y = price)) +
  # geom_hex(binwidth = c(1,5000))
  # geom_hex(binwidth = c(0.25,500))
  geom_hex(binwidth = c(0.15,200))

# change color divergence (use viridis color scale)
diamonds.small %>% 
  ggplot(aes(x = z, y = price)) +
  geom_hex(binwidth = c(0.25,250)) +
  scale_fill_viridis_c(option = "inferno")


# final heatmap of Hexagonal heatmap of 2d bin counts (for exporting)
diamonds.big %>% 
  filter(z <= 10) %>% 
  ggplot(aes(x = z, y = price)) +
  geom_hex(binwidth = c(0.05,250)) +
  scale_x_continuous(breaks = seq(0,10,1)) +
  scale_y_continuous(breaks = seq(0,20000,5000)) +
  scale_fill_viridis_c(option = "viridis") +
  xlab("Diamond depth (in mm)") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price VS diamond length - Hexagonal heatmap 2D") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

ggsave(filename = "./figure/04_hexaheatmap2dbin_price_z_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# boxplot 
# (continuous and discrete variable comparison)

# Now we are comparing one discrete to one continuous variable 

# We will start by comparing diamond cut with diamond price
# We will try to answer:
#   Is there any significant difference in diamond prices for different diamond cuts?
#   Are there different price distributions for different diamond cuts?
#   Does median diamond price change for different diamond cuts?
#   Can we see a different variability in price for different diamond cuts?
#   ...

# price VS diamond cut (boxplot)
diamonds.small %>% 
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot()

# Are the median prices for each cut as we expected?
# Can you see more outliers for some diamond cuts, why is that so?
# Would you say that diamond price is asymmetrically distributed?

# flip the axis (better visibility)
diamonds.small %>% 
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot() +
  coord_flip()

# can use different colors
diamonds.small %>% 
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot(outlier.colour = "red",
               color = "blue",
               fill = "green") +
  coord_flip()

# change shape, size and transparency for outliers
diamonds.small %>% 
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot(outlier.alpha = 1/3, 
               outlier.shape = 15,
               outlier.size = 3) +
  coord_flip()

# add scatterplot
diamonds.small %>% 
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot() +
  geom_jitter(alpha = 1/5) +
  coord_flip()

# final boxplot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = cut, y = price, color = cut)) +
  geom_boxplot(size = 1.3, 
               outlier.alpha = 1/15, 
               outlier.size = 5) +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_color_viridis_d(option = "plasma") +
  xlab("Diamond cut") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price VS cut - boxplot") +
  coord_flip() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

ggsave(filename = "./figure/04_boxplot_price_cut_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# violin plot 
# (continuous and discrete variable comparison)

# price VS diamond color (violin plot)
diamonds.small %>% 
  ggplot(aes(y = price, x = color)) +
  geom_violin()

# flip coords
diamonds.small %>% 
  ggplot(aes(y = price, x = color)) +
  geom_violin()

# scale violins
diamonds.small %>% 
  ggplot(aes(y = price, x = color)) +
  # geom_violin(scale = "area") # all violins with the same area
  # geom_violin(scale = "count") # areas proportional to number of observations
  geom_violin(scale = "area") # all violins the same maximum width

# fill and color
diamonds.small %>% 
  ggplot(aes(y = price, x = color)) +
  geom_violin(fill = "deepskyblue2", color = "red")

# add quantiles
diamonds.small %>% 
  ggplot(aes(y = price, x = color)) +
  geom_violin(draw_quantiles = c(.25, .5, .75))

# different bandwidth
diamonds.small %>% 
  ggplot(aes(y = price, x = color)) +
  # geom_violin(adjust = 1)
  # geom_violin(adjust = 0.5)
   geom_violin(adjust = 0.1)

# different kernell methods
diamonds.small %>% 
  ggplot(aes(y = price, x = color)) +
  geom_violin(kernel = "cosine")


# final violin plots (for exporting)
# subplots for diamond color and clarity

violin.color <- diamonds.big %>% 
  ggplot(aes(y = price, x = color, fill = color)) +
  geom_violin() +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_fill_viridis_d(option = "plasma") +
  xlab("Diamond color") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price VS color / clarity - violin plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

violin.clarity <- diamonds.big %>% 
  ggplot(aes(y = price, x = clarity, fill = clarity)) +
  geom_violin() +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_fill_viridis_d(option = "inferno") +
  xlab("Diamond clarity") +
  ylab("Price (in USD)") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

subplot <- cowplot::plot_grid(violin.color, violin.clarity, nrow = 2)

ggsave(filename = "./figure/04_violinplot_price_cc_diamonds.png", 
       plot = subplot,
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# Compare 2 discrete variables
# here we count cases in each subgroup deffined by both discrete variables

# We will start with barplot and compare diamond cut with diamond color

# barplot plot 
# (two discrete variables comparison)

# position = "stack"
diamonds.small %>% 
  ggplot(aes(x = cut, fill = color)) +
  geom_bar(position = "stack")

# position = "dodge"
diamonds.small %>% 
  ggplot(aes(x = cut, fill = color)) +
  geom_bar(position = "dodge")

# position = "fill" (height is normalized)
diamonds.small %>% 
  ggplot(aes(x = cut, fill = color)) +
  geom_bar(position = "fill")

# flipped axis
diamonds.small %>% 
  ggplot(aes(x = cut, fill = color)) +
  geom_bar(position = "dodge") +
  coord_flip()


# final barplot (for exporting)
diamonds.big %>% 
  rename(`Diamond color` = color) %>% 
  ggplot(aes(x = cut, fill = `Diamond color`)) +
  geom_bar(position = "dodge", color = "black") +
  scale_y_continuous(breaks = seq(0,10000,500)) +
  scale_fill_viridis_d(option = "magma") +
  coord_flip() +
  xlab("Diamond cut") +
  ylab("Count") +
  ggtitle("Diamond cut VS diamond color - barplot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14))

ggsave(filename = "./figure/04_barplot_cut_color_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# Now lets draw a scatter plot and compare diamond color with diamond clarity

# scatter plot 
# (two discrete variables comparison)
diamonds.small %>% 
  ggplot(aes(x = color, y = clarity)) +
  geom_jitter()

# add color parameter for more diversity
diamonds.small %>% 
  ggplot(aes(x = color, y = clarity, color = clarity)) +
  geom_jitter()

# final scatterplot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = color, y = clarity, color = clarity)) +
  geom_jitter() +
  scale_color_viridis_d(option = "inferno") +
  xlab("Diamond color") +
  ylab("Diamond clarity") +
  ggtitle("Diamond color VS diamond clarity - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

ggsave(filename = "./figure/04_scatterplot_color_clarity_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 

