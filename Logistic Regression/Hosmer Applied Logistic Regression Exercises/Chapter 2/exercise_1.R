library(ggplot2)
library(data.table)
library(dplyr)

setwd("~/Desktop/machine-learning-projects/Logistic Regression/Hosmer Applied Logistic Regression Exercises")

glow_data <- fread("datasets/GLOW/GLOW500.txt", header = T) %>%
  mutate(RATERISK = as.character(RATERISK))


complete_model <- glm(FRACTURE ~ AGE + WEIGHT + PRIORFRAC + PREMENO + RATERISK,
                 data = glow_data,
                 family = binomial)
summary(complete_model)$coefficients

reduced_model <- glm(FRACTURE ~ AGE + PRIORFRAC + RATERISK,
                 data = glow_data,
                 family = binomial)
summary(reduced_model)$coefficients

G <- as.numeric(-2*(logLik(reduced_model) - logLik(complete_model)))
pchisq(G, df = 2, lower.tail = F)
