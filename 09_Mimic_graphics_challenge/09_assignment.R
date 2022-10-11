# Assignment - Mimic graphics challenge

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(fivethirtyeight)


# Challenge 3: Tarantino movies

# article URL 
browseURL(url = "https://fivethirtyeight.com/features/complete-catalog-curses-deaths-quentin-tarantino-films/")

# dataset import
df <- fivethirtyeight::tarantino # import

# data check
help("tarantino")
View(df)
str(df)

# building a plot...

# data for the plot
#   add type of event: profanity, death
#   round minutes to whole number
#   add row id - counter for each point inside minutes (for geom_point)
#   counter will represent position of the point on y axis

df <- df %>% 
  mutate(event = case_when(profane == T ~ "PROFANITY", # event type
                           profane == F ~ "DEATH",
                           T ~ "NA")) %>% 
  mutate(event = factor(event, levels = c("PROFANITY", "DEATH"))) %>% 
  mutate(minutes = ceiling(minutes_in)) %>%  # minutes
  arrange(movie, minutes, desc(event)) %>% # sort for adding row id
  group_by(movie, minutes) %>% # for each minute in a movie
  mutate(counter = row_number()) %>%  # calculate row number
  ungroup()

df %>% group_by(movie) %>% count()

# data for each movie
df.dogs <- df %>% filter(movie == "Reservoir Dogs")
df.fiction <- df %>% filter(movie == "Pulp Fiction")
df.jackie <- df %>% filter(movie == "Jackie Brown")
df.bill1 <- df %>% filter(movie == "Kill Bill: Vol. 1")
df.bill2 <- df %>% filter(movie == "Kill Bill: Vol. 2")
df.basterds <- df %>% filter(movie == "Inglorious Basterds")
df.django <- df %>% filter(movie == "Django Unchained")

# global settings
plot.title.size <- 12
point.size <- 1

# theme
theme <- theme(plot.background = element_rect(fill ='white'),
               panel.grid.major = element_line(colour = "gray83", size = 0.5),
               panel.grid.minor = element_line(size = 0.5, colour = "gray83"),
               text = element_text(size = 12, family = "sans"),
               axis.text.x = element_text(size = 16, family = "sans"), 
               plot.title = element_text(size = plot.title.size, face = "plain"),
               legend.position = "none",
               axis.ticks.y = element_line(colour = "gray83", size = 0.8),
               axis.ticks.x = element_line(colour = "white", size = 0),
               axis.line = element_line(colour = "gray20", size = 0.8, linetype = "solid")
)


# sub-plots
plot.1.dogs <- df.dogs %>% 
  ggplot(aes(x = minutes, y = counter, color = event)) + 
  geom_point(size = point.size) +
  scale_x_continuous(limits = c(0,97), 
                     breaks = c(0, 50, 100, 150), 
                     minor_breaks = c(0, 50, 100, 150),
                     labels = c("" , "", "", "")) +
  scale_y_continuous(limits = c(0,20), 
                     breaks = c(0,10,20), 
                     minor_breaks = c(0,10,20),
                     labels = c("0", "", "20")) +
  scale_color_manual(values = c("gray10", "#ff6a00")) +
  ggtitle("RESERVOIR DOGS, 1992") +
  ylab("") +
  xlab("") +
  theme + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

plot.2.fiction <- df.fiction %>% 
  ggplot(aes(x = minutes, y = counter, color = event)) + 
  geom_point(size = point.size) +
  scale_x_continuous(limits = c(0,149), 
                     breaks = c(0, 50, 100, 150), 
                     minor_breaks = c(0, 50, 100, 150),
                     labels = c("" , "", "", "")) +
  scale_y_continuous(limits = c(0,20), 
                     breaks = c(0,10,20), 
                     minor_breaks = c(0,10,20),
                     labels = c("0", "", "20")) +
  scale_color_manual(values = c("gray10", "#ff6a00")) +
  ggtitle("PULP FICTION, 1994") +
  ylab("") +
  xlab("") +
  theme + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"))


plot.3.jackie <- df.jackie %>% 
  ggplot(aes(x = minutes, y = counter, color = event)) + 
  geom_point(size = point.size) +
  scale_x_continuous(limits = c(0,144), 
                     breaks = c(0, 50, 100, 150), 
                     minor_breaks = c(0, 50, 100, 150),
                     labels = c("" , "", "", "")) +
  scale_y_continuous(limits = c(0,20), 
                     breaks = c(0,10,20), 
                     minor_breaks = c(0,10,20),
                     labels = c("0", "", "20")) +
  scale_color_manual(values = c("gray10", "#ff6a00")) +
  ggtitle("JACKIE BROWN, 1997") +
  ylab("") +
  xlab("") +
  theme + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"))


plot.4.bill1 <- df.bill1 %>% 
  ggplot(aes(x = minutes, y = counter, color = event)) + 
  geom_point(size = point.size) +
  scale_x_continuous(limits = c(0,101), 
                     breaks = c(0, 50, 100, 150), 
                     minor_breaks = c(0, 50, 100, 150),
                     labels = c("" , "", "", "")) +
  scale_y_continuous(limits = c(0,20), 
                     breaks = c(0,10,20), 
                     minor_breaks = c(0,10,20),
                     labels = c("0", "", "20")) +
  scale_color_manual(values = c("gray10", "#ff6a00")) +
  ggtitle("KILL BILL: VOL. 1, 2003") +
  ylab("") +
  xlab("") +
  theme + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"))


plot.5.bill2 <- df.bill2 %>% 
  ggplot(aes(x = minutes, y = counter, color = event)) + 
  geom_point(size = point.size) +
  scale_x_continuous(limits = c(0,125), 
                     breaks = c(0, 50, 100, 150), 
                     minor_breaks = c(0, 50, 100, 150),
                     labels = c("" , "", "", "")) +
  scale_y_continuous(limits = c(0,20), 
                     breaks = c(0,10,20), 
                     minor_breaks = c(0,10,20),
                     labels = c("0", "", "20")) +
  scale_color_manual(values = c("gray10", "#ff6a00")) +
  ggtitle("KILL BILL: VOL. 2, 2004") +
  ylab("") +
  xlab("") +
  theme + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"))


plot.6.basterds <- df.basterds %>% 
  ggplot(aes(x = minutes, y = counter, color = event)) + 
  geom_point(size = point.size) +
  scale_x_continuous(limits = c(0,151), 
                     breaks = c(0, 50, 100, 150), 
                     minor_breaks = c(0, 50, 100, 150),
                     labels = c("" , "", "", "")) +
  scale_y_continuous(limits = c(0,20), 
                     breaks = c(0,10,20), 
                     minor_breaks = c(0,10,20),
                     labels = c("0", "", "20")) +
  scale_color_manual(values = c("gray10", "#ff6a00")) +
  ggtitle("INGLOURIOUS BASTERDS, 2009") +
  ylab("") +
  xlab("") +
  theme + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"))


plot.7.django <- df.django %>% 
  ggplot(aes(x = minutes, y = counter, color = event)) + 
  geom_point(size = point.size) +
  scale_x_continuous(limits = c(0,161), 
                     breaks = c(0, 50, 100, 150), 
                     minor_breaks = c(0, 50, 100, 150),
                     labels = c("0 m" , "50", "100", "150")) +
  scale_y_continuous(limits = c(0,20), 
                     breaks = c(0,20), 
                     minor_breaks = c(0,10,20)) +
  scale_color_manual(values = c("gray10", "#ff6a00")) +
  ggtitle("DJANGO UNCHAINED, 2012") +
  ylab("") +
  xlab("") +
  theme + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"))


# build legend
legend.data <- data.frame(x = c(1,1), y = c(1,3), event = c("DEATH", "PROFANITY")) %>% 
  mutate(event = factor(event, levels = c("PROFANITY", "DEATH")))

plot.legend <- legend.data %>% 
  ggplot(aes(x = x, y = y, color = event)) +
  geom_point(size = 3) +
  scale_color_manual(name = NULL,
                     values = c("gray10", "#ff6a00")) +
  theme(legend.background = element_rect(fill = "white", colour = "white", size = 0), 
        legend.key = element_rect(fill = "white", colour = "white", size = 0),
        legend.text = element_text(size = 12, face = "plain", colour = "gray"))
plot.legend <- cowplot::get_legend(plot.legend)


# movie max min length ratio
dogs.max.min <- df.dogs %>% summarise(max = max(minutes)) %>% pull()
fiction.max.min <- df.fiction %>% summarise(max = max(minutes)) %>% pull()
jackcie.max.min <- df.jackie %>% summarise(max = max(minutes)) %>% pull()
bill1.max.min <- df.bill1 %>% summarise(max = max(minutes)) %>% pull()
bill2.max.min <- df.bill2 %>% summarise(max = max(minutes)) %>% pull()
basterds.max.min <- df.basterds %>% summarise(max = max(minutes)) %>% pull()
django.max.min <- df.django %>% summarise(max = max(minutes)) %>% pull()

# relative to django minutes
dogs.max.min / django.max.min
fiction.max.min / django.max.min
jackcie.max.min / django.max.min
bill1.max.min / django.max.min
bill2.max.min / django.max.min
basterds.max.min / django.max.min


# arrange plot in a grid
legend <- cowplot::plot_grid(NULL, plot.legend, NULL, rel_widths = c(0.005, 0.0905,0.9), ncol = 3)

plot1 <- cowplot::plot_grid(plot.1.dogs, NULL, rel_widths = c(0.64, 0.36)) + theme(plot.margin = unit(c(0,0,0,0), "cm"))
plot2 <- cowplot::plot_grid(plot.2.fiction, NULL, rel_widths = c(0.93, 0.07))  + theme(plot.margin = unit(c(0,0,0,0), "cm"))
plot3 <- cowplot::plot_grid(plot.3.jackie, NULL, rel_widths = c(0.90, 0.10))  + theme(plot.margin = unit(c(0,0,0,0), "cm"))
plot4 <- cowplot::plot_grid(plot.4.bill1, NULL, rel_widths = c(0.66, 0.34))  + theme(plot.margin = unit(c(0,0,0,0), "cm"))
plot5 <- cowplot::plot_grid(plot.5.bill2, NULL, rel_widths = c(0.79, 0.21))  + theme(plot.margin = unit(c(0,0,0,0), "cm"))
plot6 <- cowplot::plot_grid(plot.6.basterds, NULL, rel_widths = c(0.94, 0.06))  + theme(plot.margin = unit(c(0,0,0,0), "cm"))
plot7 <- cowplot::plot_grid(plot.7.django, NULL, rel_widths = c(1, 0))  + theme(plot.margin = unit(c(0,0,0,0), "cm"))

title <- ggdraw() + 
  draw_label("The complete obsence guide to Tarantino",
             fontface = "bold",
             size = 20,
             x = 0, 
             hjust = 0 ) +
  theme(plot.margin = margin(0, 0, 0, 10))

subtitle <- ggdraw() + 
  draw_label("Time stamp of every instance of profanity and each death in feature films directed by Quentin Tarantino",
             fontface = "plain",
             size = 16,
             x = 0, 
             hjust = 0 ) +
  theme(plot.margin = margin(0, 0, 0, 10))

# create final plot
rel.heights.title <- rep(0.03,2)
rel.heights.legend <- 0.07
rel.heights.plots <- rep((1-sum(rel.heights.title) - rel.heights.legend) / 7, 7)

cowplot::plot_grid(title,
                   subtitle,
                   legend,
                   plot1,
                   plot2,
                   plot3,
                   plot4,
                   plot5, 
                   plot6, 
                   plot7, 
                   ncol = 1,
                   rel_heights = c(rel.heights.title, 
                                   rel.heights.legend, 
                                   rel.heights.plots))


ggsave(filename = "./figure/09_assignment_challenge3_tarantino.png", 
       plot = last_plot(),
       units = "cm", width = 28, height = 25, dpi = 600) 

