# ggplot2 for stamdard plots and beyond

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggwordcloud)

install.packages("ggiraphExtra")
library(ggiraphExtra)


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


# time series visualization (line chart)

# data (ggplot2 dataset)
df <- ggplot2::economics

# visualize unemployment
df %>% 
  ggplot(aes(x = date, y = unemploy)) +
  geom_line()

# change labels for date
df %>% 
  ggplot(aes(x = date, y = unemploy)) +
  geom_line() +
  # scale_x_date(date_breaks = "year")
  # scale_x_date(date_breaks = "5 years")
  # scale_x_date(date_breaks = "10 years")
  # scale_x_date(date_breaks = "month")
  # scale_x_date(date_breaks = "10 years", date_labels = "%Y") # show only year
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") # show only year

# different line styles
df %>% 
  ggplot(aes(x = date, y = unemploy)) +
  # geom_line(linetype = "dashed") 
  # geom_line(linetype = "dotdash")
  # geom_line(linetype = "longdash")
  geom_line(size = 2, color = "red")


# multiple lines

# long format data
df.long <- df %>% 
  select(-pce, -pop) %>% 
  tidyr::pivot_longer(cols = colnames(df)[4:ncol(df)], 
                      names_to = "indicator", 
                      values_to = "value")  

df.long %>% 
  ggplot(aes(x = date, y = value, 
             group = indicator, color = indicator)) +
  geom_line() +
  scale_y_log10()

# final line plot for export
df.long %>% 
  ggplot(aes(x = date, y = value, 
             group = indicator, color = indicator)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_log10() +
  xlab("Month") +
  ylab("Value") +
  ggtitle("Economic indicators - line plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/07_lineplot_economics.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# word cloud

# library: ggwordcloud  (have to install and load)

# create word cloud for car manufacturers from the cars dataset
data <- mpg %>% 
  group_by(manufacturer) %>% 
  count()

# create word cloud
set.seed(135) #randomness in positioning labels in the cloud

data %>% 
  ggplot(aes(label = manufacturer)) +
  geom_text_wordcloud() +
  theme_minimal()


# add text size - number of cars
data %>% 
  ggplot(aes(label = manufacturer, size = n)) +
  geom_text_wordcloud() +
  theme_minimal()

# determine max text size 
data %>% 
  ggplot(aes(label = manufacturer, size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()

# text area
data %>% 
  ggplot(aes(label = manufacturer, size = n)) +
  #geom_text_wordcloud(area_corr = T) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_size_area(max_size = 25) +
  theme_minimal()

# rotate words
data <- data %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1)))

data %>% 
  # ggplot(aes(label = manufacturer, size = n, angle = angle)) +
  ggplot(aes(label = manufacturer, size = n, angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()


# final word cloud for export
data %>% 
  ggplot(aes(label = manufacturer, 
             size = n, 
             angle = angle1,
             color = manufacturer)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 30) +
  scale_color_viridis_d(option = "magma") +
  theme_minimal()

ggsave(filename = "./figure/07_wordcloud_cars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 


# waterfall chart

# data - economics: increase/decrease in unemployment
df <- ggplot2::economics %>% 
  select(date, unemploy)

# calculate differences between two successive months 
df <- df %>% 
  mutate(unemploy_lag = lag(unemploy, 1), # unemployment rate previous month
         `unemploy diff` = unemploy - unemploy_lag, # difference in employment between two months
  `diff type` = case_when(`unemploy diff` > 0 ~ "increase", # type of difference
                          `unemploy diff` < 0 ~ "decrease",
                          `unemploy diff` == 0 ~ "no change")) %>% 
  filter(!is.na(unemploy_lag)) # remove first month


# draw waterfall chart
df %>% 
  filter(date < "1970-01-01") %>% 
  ggplot(aes(x = date, fill = `diff type`)) +
  geom_rect(aes(x = date, 
                xmin = date - 10, xmax = date + 10, 
                ymin = unemploy_lag, ymax = unemploy))


# final waterfall chart for export
df %>% 
  filter(date >= "1980-01-01" & date <= "2000-01-01") %>% 
  ggplot(aes(x = date, fill = `diff type`)) +
  geom_rect(aes(x = date, 
                xmin = date - 15, xmax = date + 15, 
                ymin = unemploy_lag, ymax = unemploy)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_fill_manual(values = c("limegreen", "brown1", "gray")) +
  xlab("Month") +
  ylab("Number of unemployed in 1000") +
  ggtitle("Unemployement in the USA - waterfall chart") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 1))


ggsave(filename = "./figure/07_waterfall_chart_unemploymenet.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# radar chart / spider chart  

# library: ggiraphExtra  (have to install and load)

# iris dataset
ggRadar(data=iris, aes(color=Species))
ggRadar(data=iris,aes(group=Species))

# cars dataset (interactive map)
help(mtcars)
View(mtcars)

df <- mtcars %>% 
  mutate(Transmission = case_when(am == "0" ~ "automatic",
                                  am == "1" ~ "manual"))

ggRadar(data = df, 
        aes(colour = Transmission, facet = cyl), 
        interactive=TRUE)

ggsave(filename = "./figure/07_radar_chart_mtcars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 
