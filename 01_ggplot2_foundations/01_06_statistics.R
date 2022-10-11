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



# Add geometry
# we would like to create scatterplot:
#   - rendering observations as points
#   - need to determine point size and point transparency (look at original plot)
ggplot(data = df, mapping = aes(x = displ, y = hwy)) +
  geom_point(size = 5, alpha = 1/3)



# Add facets
# we would like to split original plot into subplots by rows and columns:
#   - use facet_grid()
#   - column split by variable type_of_drive
#   - row split by variable transmission
#   - use function argument to allign axis limits to each subplot (scales = "free")
ggplot(data = df, mapping = aes(x = displ, y = hwy)) +
  geom_point(size = 5, alpha = 1/3) +
  facet_grid(transmission ~ type_of_drive, scales = "free")



# Add statistics layer
# we would like to fit linear model to each set of points in each facet (smoothing line):
#   - use fgeom_smooth()
#   - column split by variable type_of_drive
#   - row split by variable transmission
#   - use function argument to allign axis limits to each subplot (scales = "free")
ggplot(data = df, mapping = aes(x = displ, y = hwy)) +
  geom_point(size = 5, alpha = 1/3) +
  facet_grid(transmission ~ type_of_drive, scales = "free") +
  geom_smooth(method = "lm")
