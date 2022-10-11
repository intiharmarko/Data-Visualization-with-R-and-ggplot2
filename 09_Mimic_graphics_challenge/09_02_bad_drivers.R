# Mimic graphics challenge

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(fivethirtyeight)


# Challenge 2: Bad drivers

# article URL (Third barplot we try to mimic!!!)
browseURL(url = "https://fivethirtyeight.com/features/which-state-has-the-worst-drivers/")

# dataset import
df <- fivethirtyeight::bad_drivers # import

# data check
help("bad_drivers")
View(df)
str(df)


# building a plot...

# data for the plot
#   add number of drivers speeding
#   convert to long format to get proper legend
#   rename country D.C.
#   factor variable state

df <- df %>% 
  mutate(state = case_when(state == "District of Columbia" ~ "D.C.", # renamed country
                           T ~ state))

df.long <- df %>% 
  mutate(num_drivers_speeding = num_drivers * perc_speeding / 100) %>% # number of speeding
  mutate(state = factor(state, levels = rev(sort(df$state)))) %>% # factor variable state
  select(state, num_drivers, num_drivers_speeding) %>% 
  tidyr::pivot_longer(cols = c("num_drivers", "num_drivers_speeding"), names_to = "key", values_to = "val")

# theme
theme <- theme(panel.background = element_rect(fill = "gray90", size = 0),
               plot.background = element_rect(fill='gray90'),
               panel.grid.major = element_line(colour="gray83", size = 0.5),
               panel.grid.minor = element_line(size = 0.5, colour="grey"),
               panel.grid.minor.y = element_blank(),
               panel.grid.major.y = element_blank(),
               axis.ticks = element_line(colour = "gray83", size = 0.8),
               text = element_text(size = 12, family="sans"),
               plot.title = element_text(size = 16, face="bold"),
               plot.subtitle = element_text(size = 14, face="plain"),
               legend.position = "top",
               legend.key = element_rect(fill = "gray90", colour = "gray90", size = 0), 
               legend.text = element_text(size = 8, face = "bold", family = "sans"),
               legend.background = element_rect(fill = "gray90", colour = 'gray90'),
               legend.direction = "vertical",
               axis.ticks.y = element_blank()
)


# plot
ggplot(filter(df.long, key == "num_drivers"), 
       aes(x = state, y = val)) +
  geom_bar(stat = "identity", aes(fill = "#EDC0C0")) +
  geom_bar(data = filter(df.long, key == "num_drivers_speeding"), 
           stat = "identity", aes(fill = I("orangered1"))) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,25,5),
                     minor_breaks = seq(0,25,5),
                     position = "bottom") +
  xlab("") +
  ylab("") +
  labs(title = "Drivers Involved In Fatal Collisions While Speeding",
       subtitle = "As a share of the number of fatal collisions per billion miles, 2009") +
  scale_fill_manual(name = NULL, 
                    values = c("#EDC0C0", "orangered1"), 
                    labels = c('TOTAL COLLISIONS PER BILLION MILES',
                               'SPEEDING COLLISIONS PER BILLION MILES')) +
  guides(fill = guide_legend(reverse = T)) +
  theme


ggsave(filename = "./figure/09_challenge2_bad_drivers.png", 
       plot = last_plot(),
       units = "cm", width = 18, height = 25, dpi = 600) 

