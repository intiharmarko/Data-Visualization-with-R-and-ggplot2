# Assignment - ggplot2 basics

rm(list = ls())
graphics.off()

library(ggplot2)
library(dplyr)




# Exercise 1

df <- ggplot2::mpg

ggplot(data = df, mapping = aes(x = displ, y = hwy)) +
  geom_point(size = 8, alpha = 1/5, color = "red", position = "jitter") +
  xlab("Engine displacement (volume in litres)") +
  ylab("Highway miles per gallon (MPG)") +
  ggtitle("Car fuel consumption") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)))

ggsave(filename = "./figure/01_assignment_fig1.png", plot = last_plot(), 
       units = "cm", width = 29.7, height = 21, dpi = 600)

# Exercise 2

# create new variable transmission from variable trans
df <- df %>% mutate(transmission = substr(trans, 1, 1)) %>% 
  mutate(transmission = case_when(transmission == "a" ~ "automatic trans.",
                                  transmission == "m" ~ "manual trans.")) 

ggplot(data = df, mapping = aes(x = displ, y = hwy, color = transmission)) +
  geom_point(size = 8, alpha = 1/3, position = "jitter") +
  xlab("Engine displacement (volume in litres)") +
  ylab("Highway miles per gallon (MPG)") +
  ggtitle("Car fuel consumption") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)))

ggsave(filename = "./figure/01_assignment_fig2.png", plot = last_plot(), 
       units = "cm", width = 29.7, height = 21, dpi = 600)



# Exercise 3

ggplot(data = df, aes(x = cyl, y = hwy)) +
  geom_point(size = 7, color = "red", alpha = 1/3, position = "jitter") +
  xlab("Number of cylinders") +
  ylab("Highway miles per gallon (MPG)") +
  ggtitle("Car fuel consumption") +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0))) 

ggsave(filename = "./figure/01_assignment_fig3.png", plot = last_plot(), 
       units = "cm", width = 29.7, height = 21, dpi = 600)




# Exercise 4

ggplot(data = df, aes(x = class, y = hwy)) +
  geom_point(size = 8, color = "red", alpha = 1/5, position = "nudge") +
  scale_y_continuous(breaks = seq(0,50,5)) +
  xlab("Car type") +
  ylab("Highway miles per gallon (MPG)") +
  ggtitle("Car fuel consumption") +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0))) 

ggsave(filename = "./figure/01_assignment_fig4.png", plot = last_plot(), 
       units = "cm", width = 29.7, height = 21, dpi = 600)
