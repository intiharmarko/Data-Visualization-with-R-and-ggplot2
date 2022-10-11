# Data visualization with ggplot2 & grammar of graphics


rm(list = ls())
graphics.off()

library(ggplot2)
library(dplyr)

# 1st layer data

# assign cars fuel economy data to data frame
df <- ggplot2::mpg

# create new variable transmission from variable trans
df <- df %>% mutate(transmission = substr(trans, 1, 1)) %>% 
  mutate(transmission = case_when(transmission == "a" ~ "automatic trans.",
                                  transmission == "m" ~ "manual trans.")) 

# create new variable type_of_drive from variable drv 
df <- df %>%
  mutate(type_of_drive = case_when(drv == "f" ~ "front-wheel drive",
                                   drv == "r" ~ "rear-wheel drive",
                                   drv == "4" ~ "4-wheel drive")) %>% 
  mutate(type_of_drive = factor(type_of_drive, levels = c("front-wheel drive", "rear-wheel drive", "4-wheel drive")),
         transmission = factor(transmission, levels = c("manual trans.", "automatic trans.")))

# start building plot with data layer
ggplot(data = df)



# Add aesthetics mapping 
# we map:
#   - variable displ -> x-axis  (displ: engine displacement in litres)
#   - variable hwy   -> y-axis  (hwy: higway miles per gallon, car fuel consumption on highways)
ggplot(data = df, mapping = aes(x = displ, y = hwy))
