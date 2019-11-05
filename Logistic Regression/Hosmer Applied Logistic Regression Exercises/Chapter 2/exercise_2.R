library(ggplot2)
library(data.table)
library(dplyr)

setwd("~/Desktop/machine-learning-projects/Logistic Regression/Hosmer Applied Logistic Regression Exercises")

icu_data <- fread("datasets/ICU/ICU.txt", header = T) %>%
  mutate(RACE = as.character(RACE))

# a)
race_encoder <- data.frame(race = c(1, 2, 3),
                           meaning = c("White", "Black", "Other"),
                           D1 = c(0, 1, 0),
                           D2 = c(0, 0, 1))
race_encoder

# b)
# 6 + 1 parameters

# c)
# 6 + 1 likelihood equations

