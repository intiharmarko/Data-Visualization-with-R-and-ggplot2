# Assignment - Expoloratory Data Analysis (EDA)

rm(list = ls())
graphics.off()

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# data
df <- ggplot2::mpg


# Exercise 1

# Explore dataset
help("mpg") # description
str(df) # table structure
nrow(df); ncol(df) # rows / columns

# Find and convert character variables to factor
colnames(df)[sapply(df, class) == 'character']
df <- df %>% 
  mutate(manufacturer = as.factor(manufacturer),
         model = as.factor(model),
         trans = as.factor(trans),
         drv = as.factor(drv),
         fl = as.factor(fl),
         class = as.factor(class))

# Summary statistics for variables
summary(df)

# What about cyl variable numeric or factor?
df <- df %>% 
  mutate(cyl = as.factor(cyl))


# Exercise 2

# cty & hwy variables

# cty first peek (histogram):
df %>% 
  ggplot(aes(x = cty)) +
  geom_histogram(binwidth = 1)

# Questions?
#   Are there any missing values in cty?
#   Where is the peak (most frequent) value for cty?
#   Does mean or median value come near the most frequent value?

df %>% 
  summarise(mean = mean(cty),
            med = median(cty))

# hwy first peek (histogram):
df %>% 
  ggplot(aes(x = hwy)) +
  geom_histogram(binwidth = 1)

# Questions?
#   Are there more than 1 peak values for hwy?
#   Why do you think there are more than 1 peaks in hwy?
#   Does mean or median value returns any relevant information?

df %>% 
  summarise(mean = mean(hwy),
            med = median(hwy))

# Figure: cty & hwy as subplots - histogram (export)

# cty histogram for exporting
fig.cty <- df %>% 
  ggplot(aes(x = cty)) +
  geom_histogram(binwidth = 1, color = "black", fill = "gray40") +
  scale_x_continuous(breaks = seq(0,50,2)) +
  scale_y_continuous(breaks = seq(0,30,2)) +
  xlab("City miles per gallon") +
  ylab("Frequency") +
  ggtitle("City and Highway miles per gallon - histograms") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

# hwy histogram for exporting
fig.hwy <- df %>% 
  ggplot(aes(x = hwy)) +
  geom_histogram(binwidth = 1, color = "black", fill = "gray40") +
  scale_x_continuous(breaks = seq(0,50,2)) +
  scale_y_continuous(breaks = seq(0,40,2)) +
  xlab("Highway miles per gallon") +
  ylab("Frequency") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

fig.subplot <- cowplot::plot_grid(fig.cty, fig.hwy, ncol = 1)

ggsave(filename = "./figure/03_assignment_subplot_hist_cty_hwy_cars.png", plot = fig.subplot,
       units = "cm", width = 29.7, height = 21, dpi = 600)


# Figure: cty & hwy on 1 plot - area plot (export)

# data wrangling
df.cty.hwy.long <- df %>% 
  select(cty, hwy) %>% 
  tidyr::pivot_longer(cols = c("cty", "hwy"), names_to = "measure", values_to = "values")

df.cty.hwy.long %>% 
  ggplot(aes(x = values, fill = measure)) +
  geom_area(stat = "bin", binwidth = 1, color = "black", alpha = 1/5) +
  scale_x_continuous(breaks = seq(0,50,2)) +
  scale_y_continuous(breaks = seq(0,50,2)) +
  xlab("Miles per gallon") +
  ylab("Frequency") +
  ggtitle("City and Highway miles per gallon - area plot") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20, face = "bold"))

ggsave(filename = "./figure/03_assignment_area_cty_hwy_cars.png", plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)


# Exercise 3

# Questions?
#   What is the minimum engine displacement in the data?
#   What is the maximum engine displacement in the data?
#   Are some engine displacements more frequent in the dataset?
#   Why do you think this is so?

# Figure: Engine displacement in litres - frequency polygon (export)
df %>% 
  ggplot(aes(x = displ)) +
  geom_freqpoly(binwidth = 0.2, size = 1.2, color = "black") +
  scale_x_continuous(breaks = seq(0,8,0.2)) +
  scale_y_continuous(breaks = seq(0,40,5)) +
  xlab("Displacement in litres") +
  ylab("Frequency") +
  ggtitle("Engine displacement - frequency polygon") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

ggsave(filename = "./figure/03_assignment_freq_displ_cars.png", plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)



# Exercise 4

# Questions?
#   Which manufacturer has the most cars in given dataset?
#   Engines with how many cylinders are the least frequent?
#   How many different transmission types are in given car dataset?
#   How many different fuel types are in given car dataset?


fig.manufact <- df %>% 
  ggplot(aes(x = manufacturer)) +
  geom_bar(stat = "count", fill = "gray60", color = "black") +
  scale_y_continuous(breaks = seq(0,50,5)) +
  xlab("Car manufacturer") +
  ylab("Car count") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

fig.cyl <- df %>% 
  ggplot(aes(x = cyl)) +
  geom_bar(stat = "count", fill = "gray60", color = "black") +
  scale_y_continuous(breaks = seq(0,90,10)) +
  xlab("Number of cylinders") +
  ylab("Car count") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

fig.trans <- df %>% 
  ggplot(aes(x = trans)) +
  geom_bar(stat = "count", fill = "gray60", color = "black") +
  scale_y_continuous(breaks = seq(0,90,10)) +
  xlab("Transmission type") +
  ylab("Car count") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

fig.fl <- df %>% 
  ggplot(aes(x = fl)) +
  geom_bar(stat = "count", fill = "gray60", color = "black") +
  scale_y_continuous(breaks = seq(0,200,25)) +
  xlab("Fuel type") +
  ylab("Car count") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

first_row <- cowplot::plot_grid(fig.manufact, nrow = 1)
second_row <- cowplot::plot_grid(fig.trans, nrow = 1)
third_row <- cowplot::plot_grid(fig.fl, fig.cyl, nrow = 1)

fig.subplot <- cowplot::plot_grid(first_row, second_row, third_row, 
                                  ncol = 1, nrow = 3)

ggsave(filename = "./figure/03_assignment_subplot_barplot_mctf_cars.png", plot = fig.subplot,
       units = "cm", width = 29.7, height = 21, dpi = 600)
