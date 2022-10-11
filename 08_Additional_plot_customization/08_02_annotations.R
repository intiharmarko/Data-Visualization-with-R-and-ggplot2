# Additional plot customization

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# Annotations and text labels

# custom text labels - geom_text

# create some data
data <- data.frame(x = c(1,2,3), 
                   y = c(3,2,1), 
                   label = c("point 1", "point 2", "point 3"))

ggplot(data = data, aes(x = x, y = y)) +
  geom_text(aes(label = label), size = 20, color = "red") +
  scale_x_continuous(limits = c(-2,5)) +
  scale_y_continuous(limits = c(-2,5))

# different font family
data$`font family` = c("sans", "serif", "mono")

ggplot(data = data, aes(x = x, y = y)) +
  geom_text(aes(label = label, family = `font family`), size = 20) +
  scale_x_continuous(limits = c(-2,5)) +
  scale_y_continuous(limits = c(-2,5))

# different font face
data$`font face` = c("plain", "bold", "italic")

ggplot(data = data, aes(x = x, y = y)) +
  geom_text(aes(label = label, fontface = `font face`), size = 20) +
  scale_x_continuous(limits = c(-2,5)) +
  scale_y_continuous(limits = c(-2,5))


# draw points and add point labels (nudge text a bit)
ggplot(data = data, aes(x = x, y = y)) +
  geom_text(aes(label = label), size = 12, 
            nudge_x = 0.05, nudge_y = -0.35) + # nudge x/y direction
  geom_point(size = 5, color = "red") +
  scale_x_continuous(limits = c(-2,5)) +
  scale_y_continuous(limits = c(-2,5))


# labeling outliers
df <- ggplot2::msleep # data

df %>% filter(brainwt > 1)

df <- df %>%  # add labels for outliers
  mutate(label = case_when(brainwt > 1 ~ name,
                           T ~ ""))
df %>% 
  ggplot(aes(x = bodywt, y = brainwt, label = label)) +
  geom_text(nudge_y = -0.15, size = 6) +
  geom_point(size = 3)


# labeling all points overlapping!!!
df %>% 
  ggplot(aes(x = bodywt, y = brainwt, label = name)) +
  geom_text(check_overlap = T) # overlapped points removed!


# one solution to overlapped text
# library: ggrepel
install.packages("ggrepel")
library(ggrepel)

df %>% 
  ggplot(aes(x = bodywt, y = brainwt)) +
  geom_point(color = "red") +
  ggrepel::geom_text_repel(data = df, aes(label = name)) #+
  # scale_y_log10() +
  # scale_x_log10()


# using geom_label for labelling

# lets generate some data
x1 <- rnorm(n = 100000, mean = 5, sd = 2) 
y1 <- rnorm(n = 100000, mean = 10, sd = 1)
x2 <- rnorm(n = 100000, mean = 10, sd = 1)
y2 <- rnorm(n = 100000, mean = 15, sd = 2)

df <- data.frame(x = c(x1, x2), y = c(y1, y2)) # bivariate normal distribution

df.label <- data.frame(x = c(5,10),
                    y = c(10, 15),
                    label = c("N(mu=(5,10), sig=(2,1))", "N(mu=(10,15), sig=(1,2))"))

df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_bin2d(binwidth = c(0.15, 0.15)) +
  geom_label(data = df.label, aes(x = x, y = y, label = label)) +
  scale_fill_viridis_c(option = "magma") +
  coord_equal()


# some custom annotations
df <- ggplot2::economics %>% 
  select(date, unemploy)

df %>% 
  ggplot(aes(x = date, y = unemploy)) +
  geom_line() +
  annotate(geom = "text", 
           x = as.Date("1980-01-01"), 
           y = 15000,
           label = "Unemployment in the US",
           size = 16)

# lets add decades
df <- df %>% 
  mutate(decade = case_when(date >= "1960-01-01" & date < "1970-01-01" ~ "1960s",
                            date >= "1970-01-01" & date < "1980-01-01" ~ "1970s",
                            date >= "1980-01-01" & date < "1990-01-01" ~ "1980s",
                            date >= "1990-01-01" & date < "2000-01-01" ~ "1990s",
                            date >= "2000-01-01" & date < "2010-01-01" ~ "2000s",
                            date >= "2010-01-01" & date < "2020-01-01" ~ "2010s"))

decades.labels <- data.frame(date = c(as.Date("1965-01-01"),
                                   as.Date("1975-01-01"),
                                   as.Date("1985-01-01"),
                                   as.Date("1995-01-01"),
                                   as.Date("2005-01-01"),
                                   as.Date("2015-01-01")),
                             unemploy = 2000,
                             label = c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s"))

ggplot() +
  geom_line(data = df, aes(x = date, 
                           y = unemploy, 
                           color = decade, 
                           group = 1), 
            size = 2) +
  geom_label(data = decades.labels, aes(x = 
                                          date, 
                                        y = unemploy, 
                                        label = label,
                                        color = label),
             size = 12) +
  theme(legend.position = "none")

