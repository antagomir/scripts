# http://ggvis.rstudio.com/cookbook.html

library(ggvis)
library(dplyr)

mtcars %>% ggvis(~wt, ~mpg) %>% layer_points()

# 95% Confidence and linear fit
mtcars %>% 
  ggvis(~wt, ~mpg) %>%
  layer_points() %>%
  layer_model_predictions(model = "lm", se = TRUE)
