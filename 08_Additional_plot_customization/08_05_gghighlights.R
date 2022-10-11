# Additional plot customization

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

#install.packages("gghighlight")
library(gghighlight)

# Create highlights with gghighlight

# generate some data
#   a lot of time series
#   random walks (1 dimension movement over time)
steps <- 500 # number of steps
walks <- 30 # number of walks
step_size <- seq(-5,5, length.out = 100) # possible steps

# generate a walks
set.seed(1234)

df.walks <- NULL

for(id.walk in 1:walks){ # loop over walks

  df.walk <- data.frame(id.walk = id.walk, 
                        t = 0:steps, # time step 
                        position = c(0, rep(NA, steps)) # position (starting is at 0)
                        )
  
  position <- 0 # starting position 
  r <- 2 # row counter
  for(t in 1:steps){ # loop over each movement/step
    position <- position + sample(x = step_size, size = 1) # move to a new position
    df.walk[r, c("t", "position")] <- c(t, position) # save movement to df
    r <- r + 1
  }
  df.walks <- rbind(df.walks, df.walk)
}


# Visualize walks
df.walks <- df.walks %>% 
  mutate(id.walk = as.factor(id.walk))

ggplot(df.walks) +
  geom_line(aes(x = t, y = position, group = id.walk, color = id.walk)) 

# we would like to highlight walks (lines) where max position is over 120 (during complete walk)
# using dplyr
df.walks.filtered <- df.walks %>% 
  group_by(id.walk) %>% 
  filter(max(position) > 120) %>% 
  ungroup()

# visualize highlighted walks
ggplot(df.walks.filtered) +
  geom_line(aes(x = t, y = position, group = id.walk, color = id.walk)) 


# gghighlight for highlighting filtered walks
ggplot(df.walks) +
  geom_line(aes(x = t, y = position, group = id.walk, color = id.walk)) +
  gghighlight(max(position) > 120) # main gghighlight function


# more than one condition for highlighting
df.walks %>% 
  group_by(id.walk) %>% 
  summarise(max = max(position),
            min = min(position),
            mean = mean(position)) %>% 
  arrange(desc(max))

ggplot(df.walks) +
  geom_line(aes(x = t, y = position, group = id.walk, color = id.walk)) +
  gghighlight(max(position) > 100, mean(position) > 20) 


# set different theme
ggplot(df.walks) +
  geom_line(aes(x = t, y = position, group = id.walk, color = id.walk)) +
  gghighlight(max(position) > 100, mean(position) > 20) +
  theme_bw()

# labels in the legend
ggplot(df.walks) +
  geom_line(aes(x = t, y = position, group = id.walk, color = id.walk)) +
  gghighlight(max(position) > 100, mean(position) > 20, use_direct_label = F) +
  theme_bw()


# facet plot by highlighted series
ggplot(df.walks) +
  geom_line(aes(x = t, y = position, group = id.walk, color = id.walk)) +
  gghighlight(max(position) > 100, mean(position) > 20) +
  theme_bw() +
  facet_wrap(.~id.walk)



# gghighlight can be used for different geoms

# Histogram

load("./data/diamonds.RData")

# initial histogram
ggplot(diamonds.small, aes(x = price, fill = cut)) +
  geom_histogram(bins = 30)

# highlighted
ggplot(diamonds.small, aes(x = price, fill = cut)) +
  geom_histogram(bins = 30) +
  gghighlight()

# add facets to see the highlights
ggplot(diamonds.small, aes(x = price, fill = cut)) +
  geom_histogram(bins = 30) +
  gghighlight() +
  facet_wrap(. ~ cut)

# highlight specific group
ggplot(diamonds.small, aes(x = price, fill = cut)) +
  geom_histogram(bins = 30) +
  gghighlight(cut == "Ideal", 
              use_group_by = F) # we can exclude the operations of grouping by



# Point (scatter plot)

# initial scatter plot
ggplot(diamonds.small, aes(x = carat, y = price)) +
  geom_jitter()

# highlight diamonds where carat > 3
ggplot(diamonds.small, aes(x = carat, y = price)) +
  geom_jitter() +
  gghighlight(carat > 3)

# highlight diamonds where carat > 2.5 & price > 15000 add label for diamond color
ggplot(diamonds.small, aes(x = carat, y = price)) +
  geom_jitter() +
  gghighlight(carat > 2.5, price > 15000, 
              label_key = color)



# Labels customization

# customize labels
ggplot(df.walks) +
  geom_line(aes(x = t, y = position, group = id.walk, color = id.walk)) +
  gghighlight(max(position) > 120,
              label_params = list(size = 10, color = "red"))

# use labels directly - gghighlight replaces whole dataset with filtered dataset
ggplot(diamonds.small, aes(x = carat, y = price)) +
  geom_jitter() +
  gghighlight(carat > 3, use_direct_label = F) +
  geom_label(aes(label = color),
             fill = "gray", color = "red", alpha = 0.5, size = 8)



# Customize highlighted & unhighlighted data
ggplot(df.walks) +
  geom_line(aes(x = t, y = position, group = id.walk, color = id.walk), size = 3) +
  gghighlight(max(position) > 100, mean(position) > 20,
              unhighlighted_params = list(size = 1, colour = alpha("brown1", 0.3)))

