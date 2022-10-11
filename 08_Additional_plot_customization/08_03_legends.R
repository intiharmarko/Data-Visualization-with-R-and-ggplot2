# Additional plot customization

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# Legend

# data
load("./data/diamonds.RData")

# do not show legend 
ggplot(diamonds.small, aes(x = carat, y = price)) +
  geom_point(aes(color = color), show.legend = F)

# one variable mapped multile times to different aesthetics (1 legend shown)
ggplot(diamonds.small, aes(x = carat, y = price,
                           color = cut, 
                           size = cut,
                           shape = cut)) + 
  geom_point()

# different variables mapped to aesthetics (multiple legends)
ggplot(diamonds.small, aes(x = carat, y = price,
                           color = color, 
                           size = clarity,
                           shape = cut)) + 
  geom_point()


# different geoms and identical variables used for mapping (control which legend shown)
ggplot(diamonds.small, aes(x = carat, y = price)) +
  geom_point(aes(color = color)) +
  geom_smooth(aes(color = color))

# different geoms and different variables used for mapping (control which legend shown)
ggplot(diamonds.small, aes(x = carat, y = price)) + 
  geom_smooth(aes(size = cut), show.legend = F) +
  geom_point(aes(color = color)) 
  

# Guides

# data
df.continuous <- data.frame(x = 1, 
                            y = 1:10, 
                            z = seq(0,1,length.out = 10))

df.discrete <- data.frame(x = 1, 
                          y = 1:6, 
                          z = paste0("w", 1:6))


# continuous guides - guide function
ggplot(df.continuous, aes(x = x, y = y)) + 
  geom_tile(aes(fill = z)) +
  # guides(fill = guide_colorbar(reverse = T)) # reverse scale values
  # guides(fill = guide_colorbar(barwidth = unit(2, "cm"), barheight = unit(3, "cm"))) # guide width/height
  guides(fill = guide_colorbar(title = "Legend", title.position = "bottom")) # title modifications                             

# discrete guides 
ggplot(df.discrete, aes(x = x, y = y)) + 
  geom_raster(aes(fill = z)) +
  # guides(fill = guide_legend(ncol = 3)) # reshape legend structure
  guides(fill = guide_legend(ncol = 2, byrow = T)) # reshape legend structure

