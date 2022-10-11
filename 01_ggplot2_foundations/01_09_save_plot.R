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



# Add coordinate layer & scales
#   - use Cartesian coordinate system (just for demonstration, we could leave this out - by default Cartesian)
#   - add labels titles and figure title
#   - add scaling layer to x & y axis
#   - scaling x layer: define breaks from 0 to 50 by step size 5
#   - scaling y layer: define breaks from 0 to 10 by step size 0.5
ggplot(data = df, mapping = aes(x = displ, y = hwy)) +
  geom_point(size = 5, alpha = 1/3) +
  facet_grid(transmission ~ type_of_drive, scales = "free") +
  geom_smooth(method = "lm") +
  coord_cartesian() +
  scale_y_continuous(breaks = seq(0,50,5)) +
  scale_x_continuous(breaks = seq(0,10,0.5)) +
  xlab("Engine displacement (volume in litres)") +
  ylab("Highway miles per gallon (MPG)") +
  ggtitle("Car fuel consumption")



# Alter default theme layer:
#   - change plot title: font size 30, face "bold", center allignment, increase below margin 20 points
#   - change axis titles: font size 20, margins: y axis right margin - 20 points ; x axis top margin - 20 points
#   - change axis text (ticks): font size 16
#   - change rfacets rectangles (titles): color "black", fill color "white", text "bold" with size 16
fig <- ggplot(data = df, mapping = aes(x = displ, y = hwy)) +
  geom_point(size = 5, alpha = 1/3) +
  facet_grid(transmission ~ type_of_drive, scales = "free") +
  geom_smooth(method = "lm") +
  coord_cartesian() +
  scale_y_continuous(breaks = seq(0,50,5)) +
  scale_x_continuous(breaks = seq(0,10,0.5)) +
  xlab("Engine displacement (volume in litres)") +
  ylab("Highway miles per gallon (MPG)") +
  ggtitle("Car fuel consumption") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))


print(fig) # print plot


# Save plot to disk:
#   - .png 
#   - A4 paper size (landscape): 29.7 cm width | 21 cm height
#   - dpi 600
#   - figure name: 01_scatterplot_cars.png
#   - save to figure folder
ggsave(filename = "./figure/01_scatterplot_cars.png", units = "cm", 
       width = 29.7, height = 21, dpi = 600)
