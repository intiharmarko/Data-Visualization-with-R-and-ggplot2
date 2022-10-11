# ggplot2 for stamdard plots and beyond

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)


# pie chart

# create some data
df <- data.frame(color = c("Red", "Green", "Blue"), value = c(30,55,15))
df <- df %>% mutate(color = factor(color, levels = c("Red", "Green", "Blue")))
print(df)

# first let's create barplot
df %>% 
  ggplot(aes(x = "", y = value, fill = color)) +
  geom_bar(width = 1, stat = "identity")

# for pie chart use barplot with polar coordinates
df %>% 
  ggplot(aes(x = "", y = value, fill = color)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar(theta = "y", start = 0)

# create pie chart from factor variable
mpg %>% 
  ggplot(aes(x = "", fill = drv)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y", start = 0)

# final pie chart for export
mpg %>% 
  ggplot(aes(x = "", fill = drv)) +
  geom_bar(width = 1, color = "black") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_grey() +
  ggtitle("Type of drive - pie chart") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))

ggsave(filename = "./figure/07_piechart_drv_cars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# donut chart

# data
data <- data.frame(class = c("A", "B", "C"),
                   size = c(20, 50, 30))

# calculate percentage and borders
data <- data %>% 
  mutate(fraction = size / sum(size),
         ymax = cumsum(fraction),
         ymin = ymax - fraction)

# create donut chart
data %>% 
  ggplot(aes(ymax = ymax, 
             ymin = ymin, 
             xmax = 3, 
             xmin = 2, 
             fill = class)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(1,3))


# a little bit of customization
data <- data %>% # compute labels and labels position
  mutate(label_position = (ymin + ymax) / 2,
         label = paste0(class, "\n size: ", size))
  
data %>% 
  ggplot(aes(ymax = ymax, 
             ymin = ymin, 
             xmax = 3, 
             xmin = 2, 
             fill = class)) +
  geom_rect(color = "black") +
  geom_label(x = 2.5, aes(y = label_position, label = label), size = 6) +
  coord_polar(theta = "y") +
  scale_fill_viridis_d(option = "viridis") +
  xlim(c(1,3)) +
  theme_void() +
  theme(legend.position = "none")
  

# final donut chart for export
data %>% 
  ggplot(aes(ymax = ymax, 
             ymin = ymin, 
             xmax = 3, 
             xmin = 2, 
             fill = class)) +
  geom_rect(color = "black") +
  geom_text(x = 1.2, aes(y = label_position, label = label, color = class), size = 6) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = 3) +
  scale_color_brewer(palette = 3) +
  ggtitle("Classes count - donut chart") +
  xlim(c(-0.5,3)) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5))

ggsave(filename = "./figure/07_donut_classes.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 
  
