library(tidyverse)
library(readr)
library(janitor)
library(visdat)
library(naniar)
library(moments)
library(GGally)
library(BSDA)
library(car)
library(leaps)
#The explanatory phase - exploring the relationship between the plant based foods and obesity

#reading in the data
food_intake_kcal <- readr::read_csv("Food_Supply_kcal_Data.csv")

# Does the data contain missing values?
anyNA(food_intake_kcal)

# Visualising the missing values
visdat::vis_dat(food_intake_kcal)

# getting summary stats for obesity variable
summary(food_intake_kcal$Obesity)

food_intake_kcal<- janitor::clean_names(food_intake_kcal)
glimpse(food_intake_kcal)

ggplot2::ggplot(data = food_intake_kcal, aes(x = animal_products, y = obesity)) +
  geom_point()

scatterplot_matrix <- GGally::ggpairs(data=food_intake_kcal)
scatterplot_matrix

food_intake_kcal <- food_intake_kcal %>%
  select(
    animal_products,
    animal_fats,
    eggs,
    fish_seafood,
    fruits_excluding_wine,
    meat,
    milk_excluding_butter,
    offals,
    pulses,
    spices,
    starchy_roots,
    treenuts,
    vegetable_oils,
    vegetables,
    obesity
  )

multiple_model <- lm(formula = obesity ~., data = food_intake_kcal)
summary(multiple_model)

food_intake_kcal <- food_intake_kcal %>%
  dplyr::mutate(obesity = 
  dplyr::if_else(is.na(obesity),
                true = mean(obesity, na.rm = TRUE),
                            false = obesity)) %>%
  dplyr::mutate(obesity_above_median = (obesity > median(obesity))) 
unique(food_intake_kcal$obesity_above_median)

logistic_example <- glm(obesity_above_median ~ animal_products + animal_fats + eggs + meat, 
                        family = "binomial",
                        data = food_intake_kcal)
summary(logistic_example)

above_median_data <- food_intake_kcal %>%
  select(-obesity)

full_model <- glm(formula = obesity_above_median ~., family = "binomial", data = above_median_data)
backward_selection <- step(full_model, direction = "backward")

food_intake_kcal_glm <- food_intake_kcal %>%
  select(-obesity_above_median)

best_model <-
  leaps::regsubsets(obesity ~.,
                    data = food_intake_kcal_glm,
                    nvmax = 5,
                    method = "seqrep")

summarise_model <- summary(best_model)
summarise_model

data.frame(Adj_R2 = which.max(summarise_model$adjr2),
           RSS = which.min(summarise_model$rss),
           BIC = which.min(summarise_model$bic))
