# Assignment - Additional plot customization

rm(list = ls())
graphics.off()

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(gghighlight)
library(countrycode)




# Exercise 1

my_new_gg_theme <- theme(
  
  # Margins
  plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"), 
  
  # Title
  plot.title = element_text(size = 22,            
                            face = "bold",
                            colour = "black",
                            hjust = 0.5, 
                            margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt")),
  
  # Axis title
  axis.title = element_text(size = 16,            
                              face = "bold.italic",
                              colour = "black",
                              hjust = 0.5, 
                              margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")),
  
  # Axis text
  axis.text = element_text(size = 12,
                           face = "plain",
                           colour = "black"),
  
  # Axis ticks
  axis.ticks = element_line(colour = "black", size = 0.3), 
  
  # Grid
  panel.grid.major = element_line(colour = "black", size = 0.25), 
  panel.grid.minor = element_line(colour = "black", size = 0.15, linetype = "dashed"),
  
  # Background
  plot.background = element_rect(fill = "white"), 
  
  # Panel background & border
  panel.background = element_rect(fill = "ghostwhite"), 
  
  # Axis lines
  axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"), 
  
  # Legend
  legend.background = element_rect(fill = "white", colour = "black", size = 0.2),
  legend.key = element_rect(fill = "gray80", colour = "black"), 
  #legend.key.height = unit(0.95, "cm"), # legend key height
  #legend.key.width = unit(1.2, "cm"), # legend key width
  legend.title = element_text(size = 16, face = "bold", colour = "black"), 
  legend.text = element_text(size = 12, face = "bold.italic", colour = "black"), 
  legend.title.align = 0.5, 
  legend.text.align = 0, 

  legend.position = "right",
  
  # Facetting elements
  strip.background = element_rect(fill = "ghostwhite", colour = "black", size = 0.5 ), 
  strip.text = element_text(face = "bold", colour = "black", size = 16),   
  panel.spacing.x = unit(0.5, "cm"), 
  panel.spacing.y = unit(0.5, "cm") 
)

# test how theme looks like
load("./data/diamonds.RData")

ggplot(diamonds.small, aes(x = carat, y = price, color = cut)) +
  geom_point(size = 3) +
  facet_wrap(. ~ cut) +
  ggtitle("Test theme") +
  my_new_gg_theme


# Exercise 2

# data (coin prices)
coins <- read.table(file = "./data/coin_prices_2017.txt", header = T, sep = "\t", colClasses = "character")

# transform columns
coins <- coins %>% 
  mutate(DATE = as.Date(DATE),
         CLOSE = as.numeric(CLOSE))

# list coins
coins %>% 
  group_by(COIN, COIN_ABR) %>% 
  count()

plot.bitcoin <- coins %>% 
  filter(COIN_ABR == "BTC") %>% 
  ggplot(aes(x = DATE, y = CLOSE)) +
  geom_line(size = 0.75) +
  annotate(geom = "text", 
           x = as.Date("2017-07-01"), y = 12500, 
           label = "Bitcoin", size = 20, color = "gold3") +
  xlab("Day") +
  ylab("Closing price in USD") +
  my_new_gg_theme

plot.ethereumn <- coins %>% 
  filter(COIN_ABR == "ETH") %>% 
  ggplot(aes(x = DATE, y = CLOSE)) +
  geom_line(size = 0.75) +
  annotate(geom = "text",
           x = as.Date("2017-07-01"), y = 600,
           label = "Ethereum", size = 20, color = "gray40") +
  xlab("Day") +
  ylab("Closing price in USD") +
  my_new_gg_theme

plot.litecoin  <- coins %>% 
  filter(COIN_ABR == "LTC") %>% 
  ggplot(aes(x = DATE, y = CLOSE)) +
  geom_line(size = 0.75) +
  annotate(geom = "text",
           x = as.Date("2017-07-01"), y = 200,
           label = "Litecoin ", size = 20, 
           color = "#C0C0C0") + # silver color
  xlab("Day") +
  ylab("Closing price in USD") +
  my_new_gg_theme

plot.ripple <- coins %>% 
  filter(COIN_ABR == "XRP") %>% 
  ggplot(aes(x = DATE, y = CLOSE)) +
  geom_line(size = 0.75) +
  annotate(geom = "text",
           x = as.Date("2017-07-01"), y = 1.5,
           label = "Ripple", size = 20, color = "royalblue3") +
  xlab("Day") +
  ylab("Closing price in USD") +
  my_new_gg_theme

plots <- plot_grid(plot.bitcoin, plot.ethereumn, 
                   plot.litecoin, plot.ripple, nrow = 2, ncol = 2) 
title <- ggdraw() + 
  draw_label(       
    "Daily closing crypto prices over time (year 2017)",
    fontface = "bold",
    color = "brown1",
    size = 25,
    x = 0, 
    hjust = 0 
  ) +
  theme( 
    plot.margin = margin(0, 15, 0, 20) 
  )
plot_grid(title,
          plots,
          ncol = 1,
          rel_heights = c(0.05, 1))  

ggsave(filename = "./figure/08_assignment_cowplot_crypto.png", 
       plot = last_plot(),
       units = "cm", width = 35, height = 21, dpi = 600) 


# Exercise 3

# data (coronavirus from the exercises)
load(file = "./data/corona.RData")

last.date <- corona.all %>% summarise(max = max(Date)) %>% pull() # last date

# prepare data
df.corona <- corona.all %>% 
  rename(Country = Country.Region) %>% 
  filter(Date == last.date) %>% 
  filter(Country != "Cruise Ship") %>% 
  group_by(Country, Date, Type) %>% 
  summarise(Count = sum(Count)) %>% 
  ungroup() %>% 
  tidyr::pivot_wider(names_from = Type, values_from = Count) %>% 
  filter(infected > 0) %>% 
  mutate(Continent = countrycode(sourcevar = Country, 
                                 origin = "country.name", 
                                 destination = "continent")) %>% 
  mutate(`recovered ratio` = recovered / infected,
         `killed ratio` = killed / infected)
  
ggplot(df.corona, aes(x = infected, y = recovered + 1, 
                      size = killed, color = Continent)) +
  geom_jitter() +
  facet_wrap(. ~ Continent) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size(range = c(3,10)) +
  xlab("Number of infected people (log10 scale)") +
  ylab("Number of recovered people (log10 scale)") +
  ggtitle("Coronavirus by country (highlighted countries: infected > 10 & recovered ratio > 15%)") +
  gghighlight(infected > 10, `recovered ratio` > 0.15,
              use_group_by = F,  label_key = Country) +
  my_new_gg_theme

ggsave(filename = "./figure/08_assignment_highlight_coronavirus.png", 
       plot = last_plot(),
       units = "cm", width = 35, height = 25, dpi = 600) 

