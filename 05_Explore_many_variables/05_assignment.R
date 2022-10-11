# Assignment - Explore many variables

rm(list = ls())
graphics.off()

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(hexbin)


# data
df <- ggplot2::mpg
str(df) # table structure

df <- df %>% 
  mutate(manufacturer = as.factor(manufacturer),
         model = as.factor(model),
         trans = as.factor(trans),
         drv = as.factor(drv),
         fl = as.factor(fl),
         class = as.factor(class),
         cyl = as.factor(cyl))



# Exercise 1

df %>% 
  ggplot(aes(x = cty, y = hwy, color = displ)) +
  geom_point(size = 5, position = "jitter") +
  scale_color_gradient(low = "white", high = "red") +
  scale_x_continuous(breaks = seq(0,40,2.5)) +
  scale_y_continuous(breaks = seq(0,50,2.5)) +
  xlab("City miles per gallon") +
  ylab("Highway miles per gallon") +
  ggtitle("Hwy ~ Cty ~ displ - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16),
        legend.text =  element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

ggsave(filename = "./figure/05_assignment_scatter_hwy_cty_displ_color_cars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)

df %>% 
  ggplot(aes(x = cty, y = hwy, size = displ)) +
  geom_point(position = "jitter", color = "gray20") +
  scale_size(range = c(0.01,10)) +
  scale_x_continuous(breaks = seq(0,40,2.5)) +
  scale_y_continuous(breaks = seq(0,50,2.5)) +
  xlab("City miles per gallon") +
  ylab("Highway miles per gallon") +
  ggtitle("Hwy ~ Cty ~ displ - scatter plot") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16),
        legend.text =  element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

ggsave(filename = "./figure/05_assignment_scatter_hwy_cty_displ_size_cars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)



# Exercise 2

df %>% 
  ggplot(aes(x = cty, y = hwy, color = cyl, shape = drv)) +
  geom_point(position = "jitter", size = 5) +
  facet_wrap(vars(fl), labeller = "label_both") +
  scale_size(range = c(0.01,10)) +
  scale_x_continuous(breaks = seq(0,60,5)) +
  scale_y_continuous(breaks = seq(0,60,5)) +
  xlab("City miles per gallon") +
  ylab("Highway miles per gallon") +
  ggtitle("Hwy ~ Cty ~ cyl ~ drv ~ fl - scatter plot") +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16),
        legend.text =  element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = 14))

ggsave(filename = "05_assignment_scatter_hwy_cty_cyl_drv_fl_cars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)



# Exercise 3

source("05_functions.R")

# Generate df for models
predictor.vars <- c("cty", "displ", "cyl", "drv", "fl")
predicted.var <- "hwy"
cars.models <- generate.models(predictors.vars = predictor.vars, outcome.var = predicted.var)

# Split df
set.seed(123)
df <- split.sample(df = df, train.size = 0.8)
df.train <- df %>% filter(sample == "train")
df.test <- df %>% filter(sample == "test")

# Fit models and calculate RMSE

cars.models <- cars.models %>% # add RMSE column
  mutate(RMSE = NA)

for(id.model in 1:nrow(cars.models)){ # loop over each model
  
  # train model
  formula <- cars.models[id.model,"formula"]
  lm.model <- lm(formula = formula, data = df.train)
  
  # predict price (test dataset)
  df.test <- df.test %>% 
    mutate(`hwy predicted` = predict(lm.model, .)) 
  
  # calculate RMSE (predicted price VS actual price)
  SSE <- (df.test$`hwy predicted` - df.test$hwy)^2 # sum of squared errors
  RMSE <- sqrt(mean(SSE))
  
  # write RMSE back to table
  cars.models[id.model,"RMSE"] <- RMSE
}



# Exercise 4

predictors.levels <- cars.models %>% arrange(id) %>% pull(predictors) # levels for predictors factor variable

cars.models %>% 
  mutate(`nr. predictors` = as.factor(as.character(`nr. predictors`)),
         predictors = factor(predictors, levels = predictors.levels)) %>% 
  ggplot(aes(x = predictors, y = RMSE, fill = `nr. predictors`)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks = seq(0, 5, 0.25)) +
  scale_fill_viridis_d(option = "inferno", direction = -1) +
  xlab("Predictors") +
  ylab("RMSE") +
  ggtitle("Highway miles prediction model performance") +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./figure/05_assignment_model_performance_cars.png", 
       plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600) 

