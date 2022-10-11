# Assignment - ggplot2 for standard plots and beyond

rm(list = ls())
graphics.off()

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggwordcloud)
library(maps)
library(lubridate)



# Exercise 1

# data (coin prices)
coins <- read.table(file = "./data/coin_prices_2017.txt", header = T, sep = "\t", colClasses = "character")

# transform columns
coins <- coins %>% 
  mutate(DATE = as.Date(DATE),
         CLOSE = as.numeric(CLOSE))

# keep only bitcoin & closing price
coins <- coins %>% 
  filter(COIN_ABR == "BTC") %>% 
  select(coin = COIN, `coin abr` = COIN_ABR, date = DATE, price = CLOSE)

# bitcoin daily closing price time series
subplot.line <- coins %>% 
  ggplot(aes(x = date, y = price)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "month", date_labels = "%B") +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  xlab("Day") +
  ylab("Closing price in USD") +
  ggtitle("Bitcoin price in 2017 - line plot") +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

# calculate columns for waterfall chart
coins <- coins %>% 
  mutate(price_lag = lag(price, 1), # closing price previous day
         `price diff` = price - price_lag, # difference in price between two days
         `diff type` = case_when(`price diff` > 0 ~ "increase", # type of difference
                                 `price diff` < 0 ~ "decrease",
                                 `price diff` == 0 ~ "no change"))

# bitcoin daily closing price waterfall chart
subplot.waterfall <- coins %>% 
  ggplot(aes(x = date, fill = `diff type`)) +
  geom_rect(aes(x = date, 
                xmin = date - 0.5, xmax = date + 0.5, 
                ymin = price_lag, ymax = price)) +
  scale_x_date(date_breaks = "month", date_labels = "%B") +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  scale_fill_manual(values = c("brown1", "limegreen", "gray")) +
  xlab("Day") +
  ylab("Closing price in USD") +
  ggtitle("Bitcoin price in 2017 - waterfall chart") +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 1))

plot_grid(subplot.line, subplot.waterfall, nrow = 2)


ggsave(filename = "./figure/07_assignment_bitcoin_price_time_series.png", 
       plot = last_plot(),
       units = "cm", width = 35, height = 21, dpi = 600) 



# Exercise 2

# data (coronavirus from the exercises)
load(file = "./data/corona.RData")

# extract only last date and countries with at least 1 infected
last.date <- infected %>% summarise(max_date = max(Date)) %>% pull()

infected.last.date <- infected %>% 
  filter(Date == last.date & Infected > 0)

# count all infected per country
infected.last.date <- infected.last.date %>% 
  group_by(Country.Region, Date) %>% 
  summarise(Infected = sum(Infected)) %>% 
  ungroup()


# number of infected by countries wordcloud

# rotate country names
infected.last.date <- infected.last.date %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1)))

set.seed(135) #randomness in positioning labels in the cloud

infected.last.date %>% 
  ggplot(aes(label = Country.Region, 
             size = Infected, 
             angle = angle1)) +
  geom_text_wordcloud(color = "brown1") +
  scale_size_area(max_size = 80) +
  scale_color_viridis_d(option = "magma") +
  theme_minimal()

ggsave(filename = "./figure/07_assignment_wordcloud_infected.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# Exercise 3

# aggregate data for country and last date
corona.all.last.date <- corona.all %>% 
  filter(Date == last.date) %>% 
  group_by(Country.Region, Type, Date) %>% 
  summarise(Count = sum(Count)) %>% 
  ungroup() %>% 
  select(Country = Country.Region, Date, Type, Count)

# package for adding continents
install.packages("countrycode")
library(countrycode)

# add continents
corona.all.last.date <- corona.all.last.date %>% 
  mutate(Continent = countrycode(sourcevar = Country, origin = "country.name", destination = "continent"))

# unmatched countries
corona.all.last.date <- corona.all.last.date %>% 
  mutate(Continent = case_when(Country == "Cruise Ship" ~ "Cruise Ship",
                               T ~ Continent))

# Exclude countries
corona.all.last.date <- corona.all.last.date %>% 
  filter(Country != "China") %>% # exclude China
  filter(Country != "Cruise Ship") # exclude Cruise Ship
  
# additional variables for parallel coordinates plot
corona.all.last.date <- corona.all.last.date %>% 
  select(Continent, Country, Date, Type, Count) %>% 
  group_by(Date, Type) %>% # for each type
  mutate(`min count` = min(Count), # max value
            `max count` = max(Count)) %>% # min value
  ungroup() %>% 
  mutate(`Relative Count` = (Count - `min count`) / (`max count` - `min count`))

# order types 
corona.all.last.date <- corona.all.last.date %>% 
  mutate(Type = factor(Type, levels = c("infected", "recovered", "killed")))

# infected/recovered/killed by country and continent parallel coordinates plot 
corona.all.last.date %>% 
  ggplot(aes(x = Type, 
             y = `Relative Count`, 
             group = Country, 
             color = Continent)) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "viridis") +
  xlab("") +
  ylab("") +
  ggtitle("Coronavirus dataset - parallel coordinates plot") +
  theme_minimal() +
  theme(axis.text = element_text(size = 20),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave(filename = "./figure/07_assignment_parallelplot_corona.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 



# Exercise 4

# filter china
infected.china <- infected %>% 
  filter(Country.Region == "China") %>% 
  arrange(Date, Province.State)

# add week number and day of week
infected.china <- infected.china %>% 
  mutate(Week = isoweek(Date),
         DOW = wday(Date, week_start = getOption("lubridate.week.start", 1)))

# add rank for week and flag for first day of the week for filtering 
infected.china <- infected.china %>% 
  mutate(`Week rank` = dense_rank(Week)) %>% 
  mutate(`keep rows` = case_when(`Week rank` == 1 & DOW == 3 ~ 1,
                                 `Week rank` > 1  & DOW == 1 ~ 1,
                                 T ~ 0))
# filter rows and add week label
infected.china <- infected.china %>% 
  filter(`keep rows` == 1) %>% 
  mutate(`Week label` = paste("W", `Week rank`, " : ", Date, sep = ""))

# draw map - infection spread in china over weeks
infected.china %>% 
  filter(Infected > 0) %>% 
  ggplot(aes(x = Long, y = Lat, size = Infected, color = Infected)) +
  borders(database = "world", regions = "china") +
  geom_point(alpha = 1/2) +
  facet_wrap(. ~ `Week label`) +
  scale_color_gradient(low = "gray", high = "red") +
  scale_size_area(max_size = 50, guide = F) +
  xlab("") +
  ylab("") +
  ggtitle("Coronavirus spread in China over weeks") +
  coord_quickmap() +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        title = element_text(size = 25, colour = "brown1"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", size = 20),
        legend.position = "right",
        legend.title = element_text(size = 20, color = "black", face = "bold"),
        legend.text = element_text(size = 14),
        panel.grid = element_blank())

ggsave(filename = "./figure/07_assignment_map_corona_spread.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 

