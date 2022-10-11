# 5 Explore many variables

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# Load data
load("./data/diamonds.RData")


# Now we are comparing 3 or more variables simultaneously 
# We still keep in mind similar questions related to variables connection as before.
# We will just add additional information to the graphics


# Let's start with scatter plot where we add color (third dimension)

# add new variable volume (x*y*z) - rough estimate of volume in cubic mm, since cube volume is calculated
diamonds.small <- diamonds.small %>% 
  mutate(volume = x * y * z)
diamonds.big <- diamonds.big %>% 
  mutate(volume = x * y * z)

# Save modified data frames
save(list = c("diamonds.big", "diamonds.small", "diamonds.tiny"), file = "./data/diamonds.RData")



# Questions:
#   how are diamond carat, price and volume related?
#   is there any pattern?

# add continuous variable as point color value (diverging colors)
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = volume)) +
  geom_point()

# diverging color scales (gradient2) - default color palette 
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = volume)) +
  geom_point() +
  scale_colour_gradient2()

# diverging color scales (gradient) - low/high color
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = volume)) +
  geom_point() +
  # scale_colour_gradient(low = "green", high = "red")
  # scale_colour_gradient(low = "gray", high = "red")
  scale_colour_gradient(low = "white", high = "black")
  
# your own colour scale (gradientn)
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = volume)) +
  geom_point() +
  scale_colour_gradientn(colours = c("red", "green", "blue"))

# R built colors with names
colors() # color names
terrain.colors(5) # terrain colors as hexadecimal codes

diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = volume)) +
  geom_point() +
  #scale_colour_gradientn(colours = colors()[100:105])
  scale_colour_gradientn(colours = terrain.colors(10))

# viridis color scale
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = volume)) +
  geom_point() +
  # scale_color_viridis_c(option = "viridis") +
  # scale_color_viridis_c(option = "inferno", direction = 1)
  # scale_color_viridis_c(option = "inferno", direction = -1)
  # scale_color_viridis_c(option = "magma")
  # scale_color_viridis_c(option = "plasma")
  scale_color_viridis_c(option = "cividis")

# package for viridis color scales generation
#install.packages("viridis")
library(viridis)
viridis(n = 3, option = "magma")
viridis(n = 3, option = "magma", direction = -1)

diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = volume)) +
  geom_point() +
  scale_colour_gradientn(colours = viridis(n = 10, option = "magma"))

# final scatterplot (for exporting)
diamonds.big %>% 
  filter(volume <= 1000) %>% # exclude diamonds with extreme volume value
  ggplot(aes(x = carat, y = price, color = volume)) +
  geom_point(position = "jitter",
             size = 1.3) +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_color_viridis_c(option = "magma") +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price ~ carat ~ volume - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/05_scatterplot_price_carat_volume_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# Now let's add a discrete variable for point color 

# Questions:
#   can cut, color or clarity of a diamond as point color highlight some additional pattern for decribing price?

# add cut as color
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = cut)) +
  geom_point(size = 3, position = "jitter")

# custom color palette (scale_colour_manual)
length(levels(diamonds.small$cut)) # number of different cut values
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = cut)) +
  geom_point(size = 3, position = "jitter") +
  scale_colour_manual(values = c("red", "green", "blue", "black", "magenta"))

# scale_colour_brewer  - Sequential, diverging and qualitative colour scales from colorbrewer.org
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = cut)) +
  geom_point(size = 3, position = "jitter") +
  # scale_colour_brewer() # default sequential
  # scale_colour_brewer(type = "div") # diverging
  # scale_colour_brewer(type = "qual") # qualitative
  # scale_colour_brewer(palette = "Greens") # Greens palette
  # scale_colour_brewer(palette = "Reds") # Reds palette
  scale_colour_brewer(direction = -1, type = "qual") # Reversed color direction

# viridis color scale (discrete variable - color)
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = cut)) +
  geom_point(size = 3, position = "jitter") +
  # scale_color_viridis_d()
  # scale_color_viridis_d(option = "magma")
  scale_color_viridis_d(option = "magma", direction = -1)
  

# add color as point color
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = color)) +
  geom_point(size = 3, position = "jitter") +
  scale_color_viridis_d(option = "viridis")

# add clarity as point color
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = clarity)) +
  geom_point(size = 3, position = "jitter") +
  scale_color_brewer()
  

# final scatterplots with discrete colors (for exporting)
cut.subplot <- diamonds.big %>% 
  ggplot(aes(x = carat, y = price, color = cut)) +
  geom_point(position = "jitter",
             size = 1.3) +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_color_viridis_d(option = "magma") +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price ~ carat ~ cut - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

color.subplot <- diamonds.big %>% 
  ggplot(aes(x = carat, y = price, color = color)) +
  geom_point(position = "jitter",
             size = 1.3) +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  #scale_color_viridis_d(option = "inferno") +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price ~ carat ~ color - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

clarity.subplot <- diamonds.big %>% 
  ggplot(aes(x = carat, y = price, color = clarity)) +
  geom_point(position = "jitter",
             size = 1.3) +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_color_viridis_d(option = "inferno") +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price ~ carat ~ clarity - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

subplot <- cowplot::plot_grid(cut.subplot, color.subplot, clarity.subplot,  ncol = 1, nrow = 3)

ggsave(filename = "./figure/05_scatterplot_price_carat_ccc_subplot_diamonds.png", 
       plot = subplot,
       units = "cm", width = 29.7, height = 21, dpi = 600) 


