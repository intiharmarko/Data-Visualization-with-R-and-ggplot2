# Additional plot customization

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# Arranging plots with cowplot

# data
df <- ggplot2::mpg 

# create some plots
p1 <- ggplot(df, aes(x = cty, y = hwy)) + geom_jitter()
p2 <- ggplot(df, aes(x = displ, y = hwy)) + geom_jitter()
p3 <- ggplot(df, aes(x = cyl, y = hwy)) + geom_jitter()
p4 <- ggplot(df, aes(x = drv, y = cty)) + geom_jitter()
p5 <- ggplot(df, aes(x = trans, y = hwy)) + geom_jitter()
p6 <- ggplot(df, aes(x = class, y = hwy)) + geom_jitter()


# basic grid of plots
plot_grid(p1, p2)


# add labels
plot_grid(p1, p2, labels = c("p1", "p2")) # custom labels
plot_grid(p1, p2, labels = "auto") # automatic labels
plot_grid(p1, p2, labels = "AUTO") # automatic labels


# plot allignment
plot_grid(p1, p2, align = "h") # horizontal allignment
plot_grid(p1, p2, align = "v") # vertical allignment


# modify labels
plot_grid(p1, p2, 
          labels = c("plot 1", "plot 2"),
          label_size = 18, # font size
          label_fontface = "bold", # font face
          label_colour = "red", # text color
          label_fontfamily = "serif" # font family
          )

# move & position labels
plot_grid(p1, p2, 
          labels = c("plot 1", "plot 2"),
          label_size = 18, # font size
          label_fontface = "bold", # font face
          label_colour = "red", # text color
          label_fontfamily = "serif", # font family
          label_x = 0, # position label - x vextor
          label_y = 0,  # position label - y vextor
          hjust = -0.5, # horizontal label allignment
          vjust = -0.5 # vertical label allignment
          )

# define grid of plots rows ~ cols
plot_grid(p1, p2, labels = "AUTO", ncol = 1)
plot_grid(p1, p2, labels = "AUTO", ncol = 2)
plot_grid(p1, p2, labels = "AUTO", nrow = 1)
plot_grid(p1, p2, labels = "AUTO", nrow = 2)
plot_grid(p1, p2, labels = "AUTO", nrow = 3)
plot_grid(p1, p2, labels = "AUTO", nrow = 2, ncol = 2)

# add blank plots to the grid - NULL
plot_grid(p1, NULL, NULL, p2, labels = "AUTO", nrow = 2, ncol = 2)


# modify subplot size within a grid
plot_grid(p1, p2, labels = "AUTO", rel_widths = c(2,1)) # relative plot widths
plot_grid(p1, p2, labels = "AUTO", rel_widths = c(2,3)) # relative plot widths
plot_grid(p1, p2, labels = "AUTO", rel_widths = c(20,18)) # relative plot widths
plot_grid(p1, p2, labels = "AUTO", ncol = 1, rel_heights = c(2,1)) # relative plot heights
plot_grid(p1, p2, labels = "AUTO", ncol = 1, rel_heights = c(2,3)) # relative plot heights


# nested plot grids
top_row <- plot_grid(p1,p2,p3,p4, nrow = 1, labels = c("1", "2", "3", "4"))
bottom_row <- plot_grid(p5,p6, nrow = 1, labels = c("5", "6"))
plot_grid(top_row, bottom_row, nrow = 2)


# example with facets
p7 <- p1 + facet_wrap(vars(drv))
bottom_bottom_row <- plot_grid(p7, labels = "7")
plot_grid(top_row, bottom_row, bottom_bottom_row, nrow = 3)


# Joining plot titles (no specific cowplot function)
plots <- plot_grid(p1, p2, nrow = 1) # pot row
title <- ggdraw() + # set up drawing layer
  draw_label(       # draw label
    "City milles per gallon and engine displacement VS highway milles per gallon",
    fontface = "bold",
    size = 20,
    x = 0, # position on x axis
    hjust = 0 # horizontal adjustment
  ) +
  theme( # add margins so title is positioned for the plot
    plot.margin = margin(0, 0, 0, 10) # add some space to the left side so title will be moved a bit to the right from the left corner
  )
plot_grid(title,
          plots,
          ncol = 1,
          rel_heights = c(0.05, 1) # with relative heights we can determine the relative size of title compared to plot
          )  

