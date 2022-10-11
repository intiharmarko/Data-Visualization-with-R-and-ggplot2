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
  ggtitle("Type of drive pie chart") +
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
