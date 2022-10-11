# Mimic graphics challenge

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

install.packages("fivethirtyeight")
library(fivethirtyeight)


# Challenge 1: Congress age

# article URL (First figure we try to mimic!!!)
browseURL(url = "https://fivethirtyeight.com/features/both-republicans-and-democrats-have-an-age-problem/")

# dataset import
df <- fivethirtyeight::congress_age # import

# data check
help("congress_age")
View(df)
str(df)


# building a plot...

# data for the plot
#   average age of members at start of term 1947-2013
#   break down by party 
plot.data <- df %>% 
  select(party, age, termstart) %>% # select only relevant columns
  filter(party %in% c("D", "R")) %>% # only relevant rows
  group_by(party, termstart) %>% # aggregarion per party and term start
  summarise(`age mean` = mean(age)) # average age calculation

# custom labels
x_labels <- c("1950", "'60", "'70", "'80", "'90", "2000", "'10")
y_labels <- c("40", "45", "50", "55", "60 yrs")


# theme
theme <- theme(panel.background = element_rect(fill = "gray90", size = 0),
               plot.background = element_rect(fill ='gray90', size = 0),
               panel.grid.major = element_line(colour = "gray83", size = 0.8),
               panel.grid.minor = element_line(size = 0.8, colour = "gray83"),
               axis.ticks = element_line(colour = "gray83", size = 0.8), 
               text = element_text(size = 20, family = "sans", face = "plain"),
               plot.title = element_text(size = 30, face="bold"),
               plot.subtitle = element_text(size = 25, face="plain")
)

# plot
plot.data %>% 
  ggplot(aes(x = termstart, y = `age mean`, 
             group = party, color = party)) +
  geom_line(size = 1.8) +
  scale_y_continuous(limits = c(40,60.5), 
                     breaks = seq(40,60,5), minor_breaks = seq(40,60,5),
                     labels = y_labels) +
  scale_x_date(breaks = seq(as.Date("1950-01-01"), 
                            as.Date("2010-01-01"), "10 years"),
               minor_breaks = seq(as.Date("1950-01-01"), 
                                  as.Date("2010-01-01"), "10 years"),
               labels = x_labels) +
  ylab("") + 
  xlab("") + 
  labs(title = "Average Age of Members of Congress",
       subtitle = "At start of term, 1947-2013") +
  annotate("text", 
           x = as.Date("1968-06-01"), 
           y = 56.5, 
           label = "Republicans", 
           color = I("orangered1"), 
           size = 7.5, 
           fontface = "bold") +
  annotate("text", 
           x = as.Date("1998-09-01"), 
           y = 57.5, 
           label = "Democrats", 
           color = I("deepskyblue3"), 
           size = 7.5, 
           fontface = "bold") +
  scale_color_manual(values=c(I("deepskyblue3"), I("orangered1")), guide = F) +
  theme


ggsave(filename = "./figure/09_challenge1_congress_age.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 

