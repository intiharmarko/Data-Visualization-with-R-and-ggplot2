# Assignment - Data wrangling crash course

rm(list = ls())
graphics.off()

library(hflights)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)

df <- tibble::as_tibble(hflights) # convert dataframe to tibble


# Exercise 1

# Cancelled flights total (number and percentage)
df.canceled.total <- df %>%
  mutate(Cancelled = case_when(Cancelled == 0 ~ F, 
                               Cancelled == 1 ~ T)) %>% 
  group_by(Cancelled) %>% 
  summarise(n = n(),
            percent = round(n/nrow(df) * 100,1))

# Cancelled flights by carrier (percentage)
df.canceled.carrier.percent <- df %>%
  mutate(Cancelled = case_when(Cancelled == 0 ~ "Not cancelled", 
                               Cancelled == 1 ~ "Cancelled")) %>% 
  group_by(UniqueCarrier, Cancelled) %>% 
  summarise(n = n()) %>% 
  tidyr::pivot_wider(names_from = Cancelled, values_from = n) %>% 
  mutate(Cancelled = replace_na(Cancelled, 0),
         `Not cancelled` = replace_na(`Not cancelled`, 0)) %>% 
  mutate(total = Cancelled + `Not cancelled`) %>% 
  mutate(Cancelled = round(Cancelled/total * 100,1),
         `Not cancelled` = round(`Not cancelled`/total * 100,1)) %>% 
  select(-total)

# Cancelled flights by cancellation code and carrier (number)
df.canceled.carrier.code <- df %>% 
  filter(Cancelled == 1) %>% 
  group_by(UniqueCarrier, CancellationCode) %>% 
  summarise(n = n()) %>% 
  tidyr::pivot_wider(names_from = CancellationCode, values_from = n) %>% 
  mutate_all(.funs = replace_na, 0) 


# Exercise 2

# Number of flights, daily, break down by origin airport
df.origin.flights <- df %>% 
  mutate(Month = str_pad(Month, width = 2, side = "left", pad = "0"), 
         DayofMonth = str_pad(DayofMonth, width = 2, side = "left", pad = "0")) %>% 
  tidyr::unite(col = Date, Year, Month, DayofMonth, sep = "-") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  group_by(Date, Origin) %>% 
  summarise(n = n())


# Exercise 3

# Sort Carriers by number of flights in descending order
carrier.levels <- df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  pull(UniqueCarrier)

# Visualize number of flights by cancelattion code, break down by carrier (bar plot)
df.canceled.carrier.code %>% 
  tidyr::pivot_longer(c("A", "B", "C", "D"), 
                      names_to = "Cancellation code", 
                      values_to = "Number of flights") %>% 
  mutate(Carrier = factor(UniqueCarrier, levels = carrier.levels)) %>% 
  ggplot(aes(x = Carrier, y = `Number of flights`, fill = `Cancellation code`)) +
    geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,1400,100)) +
  ggtitle("Number of cancelled flights by cancellation code and carrier") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14))


# Exercise 4

# Visualize number of flights, daily, break down by origin airport (area plot)
df.origin.flights %>% 
  ggplot(aes(x = Date, y = n, fill = Origin)) +
  geom_area(color = "gray40") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(breaks = seq(0,1400,100)) +
  ylab("Number of flights") +
  ggtitle("Number of flights break down by flight origin") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14))
