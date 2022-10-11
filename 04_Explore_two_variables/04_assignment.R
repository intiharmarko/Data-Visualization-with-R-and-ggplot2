# Assignment - Explore two variables

rm(list = ls())
graphics.off()

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(hexbin)
library(GGally)

# data
df <- ggplot2::mpg
str(df) # table structure

df <- df %>% 
  mutate(manufacturer = as.factor(manufacturer),
         model = as.factor(model),
         trans = as.factor(trans),
         drv = as.factor(drv),
         fl = as.factor(fl),
         class = as.factor(class),
         cyl = as.factor(cyl))

# Exercise 1

# first peek scatter plot (hwy VS cty)
df %>% 
  ggplot(aes(x = cty, y = hwy)) +
  geom_point()

# add smoothing line
df %>% 
  ggplot(aes(x = cty, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm", se = T)

# scatter plot for export
df %>% 
  ggplot(aes(x = cty, y = hwy)) +
  geom_point(size = 5, color = "black") +
  geom_smooth(method = "lm", se = T) +
  scale_x_continuous(breaks = seq(0,40,2.5)) +
  scale_y_continuous(breaks = seq(0,50,2.5)) +
  xlab("City miles per gallon") +
  ylab("Highway miles per gallon") +
  ggtitle("City VS Highway miles per gallon - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

ggsave(filename = "./figure/04_assignment_scatterplot_hwy_cty_cars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)

# calculate correllation coefficient (linear correlation)
cor(x = df$hwy, y = df$cty)


# rug plot with scatter plot (hwy VS cty)
df %>% 
  ggplot(aes(x = cty, y = hwy)) +
  geom_point() +
  geom_rug()

# 2D density bin plot (hwy VS cty)
df %>% 
  ggplot(aes(x = cty, y = hwy)) +
  geom_bin2d()

# rug plot and bin2d plot for exporting
rug.plot <- df %>% 
  ggplot(aes(x = cty, y = hwy)) +
  geom_point(size = 4, color = "gray20") +
  geom_rug(sides = "tr", color = "red", size = 1.1) +
  scale_x_continuous(breaks = seq(0,40,2.5)) +
  scale_y_continuous(breaks = seq(0,50,2.5)) +
  xlab("City miles per gallon") +
  ylab("Highway miles per gallon") +
  ggtitle("City VS Highway miles per gallon - scatter plot & rug plot") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

bin2d.plot <- df %>% 
  ggplot(aes(x = cty, y = hwy)) +
  geom_bin2d(binwidth = c(1,1)) +
  scale_x_continuous(breaks = seq(0,40,2.5)) +
  scale_y_continuous(breaks = seq(0,50,2.5)) +
  scale_fill_viridis_c(option = "magma") +
  xlab("City miles per gallon") +
  ylab("Highway miles per gallon") +
  ggtitle("City VS Highway miles per gallon - heatmap 2D bin count") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

fig.subplot <- cowplot::plot_grid(rug.plot, bin2d.plot, nrow = 1)

ggsave(filename = "./figure/04_assignment_rugplot_bin2d_hwy_cty_cars.png", 
       plot = fig.subplot,
       units = "cm", width = 29.7, height = 21, dpi = 600)


# Exercise 2

# compare hwy with displ (geom_hex)
hex.plot <- df %>% 
  ggplot(aes(y = hwy, x = displ)) +
  geom_hex(binwidth = c(0.25,2)) +
  scale_x_continuous(breaks = seq(0,10,0.5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  scale_fill_viridis_c(option = "magma") +
  xlab("Engine displacement (in litres)") +
  ylab("Highway miles per gallon") +
  ggtitle("Highway miles VS engine displacement - Hexagonal heatmap 2D") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

# compare hwy with manufacturer (boxplot)
box.plot <- df %>% 
  ggplot(aes(y = hwy, x = manufacturer, fill = manufacturer)) +
  geom_boxplot(size = 1.3, 
               outlier.size = 5) +
  geom_jitter(color = "gray40", size = 2) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  scale_fill_viridis_d(option = "plasma") +
  xlab("Car manufacturer") +
  ylab("Highway miles per gallon") +
  ggtitle("Highway miles VS car manufacturer - boxplot") +
  coord_flip() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

# compare hwy with class (violin plot)
violin.plot <- df %>% 
  ggplot(aes(y = hwy, x = class, fill = class)) +
  geom_violin() +
  geom_jitter(color = "gray40", size = 2) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  scale_fill_viridis_d(option = "viridis") +
  xlab("Car type") +
  ylab("Highway miles per gallon") +
  ggtitle("Highway miles VS car type - violin plot") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position = "none")

first.column <- cowplot::plot_grid(box.plot, nrow = 1)
second.column <- cowplot::plot_grid(hex.plot, violin.plot, nrow = 2)
fig.subplot <- cowplot::plot_grid(first.column, second.column, nrow = 1, ncol = 2)

ggsave(filename = "./figure/04_assignment_geomhex_boxplot_violinplot_cars.png", 
       plot = fig.subplot,
       units = "cm", width = 40, height = 21, dpi = 600)


# Exercise 3

# compare cyl with drv (scatter plot)
drv.scatter <- df %>% 
  ggplot(aes(x = cyl, y = drv, color = drv)) +
  geom_jitter(size = 4) +
  xlab("Number of cylinders") +
  ylab("Type of drive") +
  ggtitle("Number of cylinders VS drive type - scatterplot") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

# compare cyl with fl (barplot)
fl.bar <-  df %>% 
  ggplot(aes(x = cyl, fill = fl)) +
  geom_bar(position = "stack", color = "black") +
  xlab("Number of cylinders") +
  ylab("Fuel type") +
  ggtitle("Number of cylinders VS fuel type - bar plot") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

# compare cyl with trans (barplot)
trans.bar <- df %>% 
  ggplot(aes(x = cyl, fill = trans)) +
  geom_bar(position = "dodge", color = "black") +
  xlab("Number of cylinders") +
  ylab("Transmission type") +
  ggtitle("Number of cylinders VS transmission type - bar plot") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

first.row <- cowplot::plot_grid(drv.scatter, fl.bar, nrow = 1)
fig.subplot <- cowplot::plot_grid(first.row, trans.bar, nrow = 2)

ggsave(filename = "./figure/04_assignment_discrete_subplot_cars.png", 
       plot = fig.subplot,
       units = "cm", width = 40, height = 21, dpi = 600)


# Exercise 4

ggpairs.plot <- df %>% 
  select(-model, -manufacturer) %>% # too many levels!!! -> exclude variables
  GGally::ggpairs()

ggsave(filename = "./figure/04_assignment_ggpairs_cars.png", 
       plot = ggpairs.plot,
       units = "cm", width = 29.7, height = 21, dpi = 600)
  