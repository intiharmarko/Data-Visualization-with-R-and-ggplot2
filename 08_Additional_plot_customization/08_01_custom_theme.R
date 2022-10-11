# Additional plot customization

rm(list = ls())
graphics.off()

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)


# Custom theme

# lets create figure (for theme setting)
df <- ggplot2::mpg

fig <- df %>% 
  ggplot(aes(x = cty, y = hwy, color = drv)) +
  geom_jitter(size = 2) +
  scale_x_continuous(breaks = seq(0,50,5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  xlab("City milles per gallon") +
  ylab("Highway milles per gallon") +
  ggtitle("Highway VS City mileage")
print(fig)

# built in themes (ggplot2)
theme.bw <- fig + ggtitle("theme_bw()")       + theme_bw() 
theme.ld <- fig + ggtitle("theme_linedraw()") + theme_linedraw()
theme.li <- fig + ggtitle("theme_light()")    + theme_light()
theme.da <- fig + ggtitle("theme_dark()")     + theme_dark() 
theme.mi <- fig + ggtitle("theme_minimal()")  + theme_minimal()
theme.cl <- fig + ggtitle("theme_classic()")  + theme_classic()
theme.vo <- fig + ggtitle("theme_void()")     + theme_void() 

plot_grid(theme.bw, theme.ld, theme.li, 
          theme.da, theme.mi, theme.cl, 
          theme.vo, nrow = 3, ncol = 3)


# more themes ggthemes library
install.packages("ggthemes")
library(ggthemes)


theme.calc <- fig + ggtitle("theme_calc()") + ggthemes::theme_calc() # libre office 
theme.clen <- fig + ggtitle("theme_clean()") + ggthemes::theme_clean()
theme.econ <- fig + ggtitle("theme_economist()") + ggthemes::theme_economist() # The Economist
theme.excl <- fig + ggtitle("theme_excel()") + ggthemes::theme_excel() # Excel 
theme.5308 <- fig + ggtitle("theme_fivethirtyeight()") + ggthemes::theme_fivethirtyeight() # fivethirtyeight plots
theme.solr <- fig + ggtitle("theme_solarized()") + ggthemes::theme_solarized()

plot_grid(theme.calc, theme.clen, theme.econ, 
          theme.excl, theme.5308, theme.solr, 
          nrow = 2, ncol = 3)


# set default theme
theme_get() # get current theme (default)
theme_set(theme_bw()) # set defaul theme
print(fig)
theme_update(title = element_text(size = 30)) # mof+dify theme individual elements
print(fig)

# restart R studio session (to get default theme)
print(fig)


# Let's build our own theme
my_gg_theme <- theme(
  
  # Margins
  plot.margin = margin(t = 20, r = 30, b = 20, l = 30, unit = "pt"), # plot margins
  
  # Title
  plot.title = element_text(size = 25,            # plot title: font size,face,color, text positioning, margins around text
                            face = "bold",
                            colour = "brown1",
                            hjust = 0.5, 
                            margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")),
  
  # Axis title
  axis.title.x = element_text(size = 20,            # axis titles (you can use axis.title.x, axis.title.y)
                              face = "bold.italic",
                              colour = "gray20",
                              hjust = 0.5, 
                              margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")),
  axis.title.y = element_text(size = 20,            
                              face = "bold.italic",
                              colour = "gray40",
                              hjust = 0.5, 
                              margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")),
  
  # Axis text
  axis.text = element_text(size = 14,
                           face = "plain",
                           colour = "black"),
  
  # Axis ticks
  axis.ticks.x = element_line(colour = "red", size = 2), # control ticks on x axis 
  axis.ticks.y = element_line(colour = "blue", size = 2), # control ticks on x axis 
  
  # Grid
  panel.grid.major.x = element_line(colour = "black", size = 0.5), # control vertical lines - major grid
  panel.grid.major.y = element_line(colour = "black", size = 0.5), # control horizontal lines - major grid
  panel.grid.minor.x = element_line(colour = "gray", size = 0.25, linetype = "dashed"), # control vertical lines - minor grid
  panel.grid.minor.y = element_line(colour = "gray", size = 0.25, linetype = "dashed"), # control horizontal lines - minor grid
  
  # Background
  plot.background = element_rect(fill = "linen", size = 0.8, linetype = "solid", color = "black"), # control area outside plot
  
  # Panel background & border
  panel.background = element_rect(fill = "white", colour = "black", size = 0.8), # control plot area
  
  # Axis lines
  axis.line.x = element_line(colour = "gray20", size = 2, linetype = "solid"), # control line on x axis
  axis.line.y = element_line(colour = "gray20", size = 2, linetype = "solid"), # control line on y axis
  
  # Legend
  legend.background = element_rect(fill = "gray60", colour = "black", size = 0.8), # legend background
  legend.key = element_rect(fill = "gray80", colour = "white", size = 0.6), # legend keys fill, line
  legend.key.height = unit(0.95, "cm"), # legend key height
  legend.key.width = unit(1.2, "cm"), # legend key width
  legend.title = element_text(size = 20, face = "bold", colour = "black"), # legend title
  legend.text = element_text(size = 16, face = "bold.italic", colour = "white"), # legend text
  legend.title.align = 0.5, # legend title allignment
  legend.text.align = 0.5, # legend text allignment
  
  # Legend position & justification
  #legend.position = "bottom"
  #legend.position = c(0,1), legend.justification = c(0,1)
  legend.position = "right",
  
  # Facetting elements
  strip.background = element_rect(fill = "ghostwhite", colour = "black", size = 0.5 ), # background of panel strips
  strip.text.x = element_text(face = "bold", colour = "blue", size = 20), # text facets (vertical direction)
  strip.text.y = element_text(face = "italic", colour = "red", size = 20),  # text facets (horizontal direction)
  panel.spacing.x = unit(1, "cm"), # margin between facets (vertical direction)
  panel.spacing.y = unit(2, "cm") # margin between facets (horizontal direction)
)

fig + my_gg_theme # no facets
fig + facet_grid(drv ~ fl) + my_gg_theme # with facets

# create another plot and use new theme 
load("./data/diamonds.RData")

diamonds.small %>% 
  ggplot(aes(x = carat, y = price, color = cut)) +
  geom_point() +
  facet_wrap(vars(color)) +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(breaks = seq(0,20000,5000)) +
  xlab("Carat") +
  ylab("Price in USD") +
  ggtitle("Diamonds") +
  my_gg_theme
  
