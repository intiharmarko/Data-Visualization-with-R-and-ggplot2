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


# Now let's add a continuous variable for point size

# First let's use diamond volume
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, size = volume)) +
  geom_point() 

# Let's check distribution of diamond volume
diamonds.small %>% 
  ggplot(aes(x = volume)) +
  geom_histogram(binwidth = 5)

# filter volume (more distinct point size)
diamonds.small %>% 
  filter(volume >= 50 & volume <= 400) %>% 
  ggplot(aes(x = carat, y = price, size = volume)) +
  geom_point() 
  
# scale point size (no filtering) - scale size
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, size = volume)) +
  geom_point() +
  # scale_size(range = c(5,7))
  scale_size(range = c(1,10))

# scale_size - custom breaks
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, size = volume)) +
  geom_point() +
  # scale_size(breaks = seq(0,500,50))
  scale_size(breaks = seq(0,500,50), range = c(1,10))

# scale_size_area ~ scale_radius
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, size = volume)) +
  geom_point() +
  # scale_size_area(max_size = 10) # zero value mapped to 0, most useful option with count - point size
  scale_radius() # map size to radius - try to avoid


# Now let's add a discrete variable for point shape

# First let's use diamond cut
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, shape = cut)) +
  geom_point() 

# scale_shape
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, shape = cut)) +
  geom_point() +
  # scale_shape_discrete(solid = F) # solid false options
  scale_shape_discrete(solid = T) # solid true options

# manual shapes (scale_shape_manual)
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, shape = cut)) +
  geom_point() +
  scale_shape_manual(values = c(0, 6, 12, 18, 24))

# final scatterplots with shape of points and its size (for exporting)
diamonds.big %>% 
  filter(volume <= 1000) %>% # exclude diamonds with extreme volume value
  ggplot(aes(x = carat, y = price, 
             size = volume, shape = cut, color = cut)) +
  geom_point(position = "jitter") +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_size(name = "point size (volume)",
             breaks = seq(0,1000,100)) +
  scale_color_viridis_d(option = "magma") +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price ~ carat ~ volume ~ cut - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/05_scatterplot_price_carat_volume_cut_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# Facetting - create small multiples (break plot by discrete variable)

# facet_wrap - wrap 1D ribbon of panels into 2D (sequence of panels)

# facet wrap by diamond cut - vars
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_wrap(vars(cut))

# facet wrap by diamond cut - using formula
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_wrap(~cut)

# facet wrap by diamond cut - using nrow
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  # facet_wrap(vars(cut), nrow = 2)
  # facet_wrap(vars(cut), nrow = 3)
  facet_wrap(vars(cut), nrow = 5)

# facet multiple variables
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_wrap(vars(color,cut))
  # facet_wrap(color~cut)

# control facet labels
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  # facet_wrap(vars(color,cut), labeller = "label_value") # only values
  facet_wrap(vars(color,cut), labeller = "label_both") # var names & values

# control scales for panels (scales_free)
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  # facet_wrap(vars(color,cut), scales = "fixed") # default - fixed scales
  facet_wrap(vars(color,cut), scales = "free") # different scales for each panel

# final scatterplot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(position = "jitter",
             size = 1.3) +
  facet_wrap(vars(cut, color), 
             scales = "free", 
             labeller = "label_both") +
  scale_x_continuous(breaks = seq(0,5,1)) +
  scale_y_continuous(breaks = seq(0,20000,5000)) +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price ~ carat ~ cut ~ color - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = 10),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/05_scatterplot_price_carat_cut_color_facetwrap_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 40, height = 30, dpi = 600) 


# facet_grid - layout panels in a grid (matrix of panels)

# facet grid by diamond clarity - rows /vars
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(rows = vars(clarity))

# facet grid by diamond cut - cols /vars
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(cols = vars(cut))

# facet grid rows & cols
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(rows = vars(clarity), cols = vars(cut))

# facet grid - using formula
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(clarity ~ cut)

# control facet labels & scales
diamonds.small %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(clarity ~ cut, scales = "free", labeller = "label_both")

# final scatterplot (for exporting)
diamonds.big %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(position = "jitter",
             size = 1.3) +
  facet_grid(clarity ~ cut, 
             scales = "free", 
             labeller = "label_both") +
  scale_x_continuous(breaks = seq(0,5,1)) +
  scale_y_continuous(breaks = seq(0,20000,5000)) +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price ~ carat ~ cut ~ clarity - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = 10),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/05_scatterplot_price_carat_cut_clarity_facetgrid_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 40, height = 30, dpi = 600) 


# Now let's create figures with many graphical elements

# The main idea is still: explain how diamond price is affected by other variables!!!
# When adding many graphical elements, we should be careful not to polute plot with too many information!

# All discrete variables with price (cut/color/clarity)

# variation 1
diamonds.small %>% 
  ggplot(aes(x = cut, y = price, color = cut)) +
  geom_point(position = "jitter") +
  facet_grid(color ~ clarity)

# variation 2
diamonds.small %>% 
  ggplot(aes(x = cut, y = color, color = price)) +
  geom_point(position = "jitter") +
  facet_grid(rows = vars(clarity)) +
  scale_color_gradient2(low = "white", high = "black")

# final scatterplot - variation1 (for exporting)
diamonds.big %>% 
  ggplot(aes(x = cut, y = price, color = cut)) +
  geom_point(position = "jitter",
             size = 1.3) +
  facet_grid(color ~ clarity, 
             scales = "free", 
             labeller = "label_both") +
  scale_y_continuous(breaks = seq(0,20000,5000)) +
  scale_color_viridis_d(option = "magma") +
  xlab("Cut") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price ~ cut ~ clarity ~ color - scatter plot") +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = 10),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/05_scatterplot_price_cut_color_clarity_facetgrid_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 40, height = 25, dpi = 600) 


# Continuous variables (price/carat/volume) with discrete variables (cut/color/clarity)

# peek
diamonds.small %>% 
  ggplot(aes(x = carat, y = price, size = volume, color = cut)) +
  geom_point(position = "jitter") +
  scale_size(range = c(0.01, 5)) +
  facet_grid(color ~ clarity)


# final scatterplot (for exporting)
diamonds.big %>% 
  filter(volume <= 1000) %>% # exclude diamonds with extreme volume value
  ggplot(aes(x = carat, y = price, size = volume, color = cut)) +
  geom_point(position = "jitter") +
  facet_grid(color ~ clarity, 
             scales = "free", 
             labeller = "label_both") +
  scale_y_continuous(breaks = seq(0,20000,5000)) +
  scale_size(range = c(0.01, 3)) +
  scale_color_viridis_d(option = "viridis") +
  xlab("Carat") +
  ylab("Price (in USD)") +
  ggtitle("Diamond price ~ carat ~ volume ~ cut ~ clarity ~ color - scatter plot") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/05_scatterplot_price_carat_volume_ccc_diamonds.png", 
       plot = last_plot(),
       units = "cm", width = 40, height = 25, dpi = 600) 
