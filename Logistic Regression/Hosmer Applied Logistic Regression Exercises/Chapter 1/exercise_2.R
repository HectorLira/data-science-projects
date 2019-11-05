library(ggplot2)
library(data.table)
library(dplyr)
library(Hmisc)

setwd("~/Desktop/machine-learning-projects/Logistic Regression/Hosmer Applied Logistic Regression Exercises")

dataset <- fread("datasets/MYOPIA/MYOPIA.txt", header = T)

X <- "SPHEQ"
Y <- "MYOPIC"

min_x <- min(dataset[[X]])
max_x <- max(dataset[[X]])

# b)
ggplot(data = dataset) +
  geom_point(aes(y = get(Y), x = get(X))) +
  xlim(c(min_x, max_x)) +
  ylim(c(0,1)) +
  xlab(X) +
  ylab(Y) +
  ggtitle(sprintf("Scatterplot of %s vs. %s", Y, X)) +
  theme(plot.title = element_text(hjust = 0.5))

# c)
intervals <- Hmisc::cut2(dataset[[X]],
                         m = nrow(dataset) / 8,
                         onlycuts = T)
class_rep <- rowMeans(cbind(head(intervals, -1),
                            intervals[-1]))

data_summary <- dataset %>%
  mutate(x_intervals = cut(get(X), breaks = intervals,
                           include.lowest = T,
                           right = F)) %>%
  group_by(x_intervals) %>%
  summarise(y_counts = n(),
            y_mean = mean(get(Y))) %>%
  mutate(class_reps = class_rep)

ggplot(data = data_summary,
       aes(y = y_mean, x = class_reps)) +
  geom_line() +
  geom_point() +
  xlim(c(min_x, max_x)) +
  ylim(c(0,1)) +
  ylab(sprintf("Mean %s", Y)) +
  ggtitle(sprintf("Scatterplot of Mean %s vs. %s classes",
                  Y,
                  X)) +
  theme(plot.title = element_text(hjust = 0.5))

# e)
logistic_model <- glm(get(Y) ~ get(X),
                      data = dataset,
                      family = binomial)
summary(logistic_model)

logistic_model_function <- function(x){
  logit_prob <- logistic_model$coefficients[1] +
    logistic_model$coefficients[2] * x
  
  return(exp(logit_prob) / (1 + exp(logit_prob)))
}

ggplot(data = dataset, aes(y = get(Y), x = get(X))) +
  geom_point() +
  stat_function(fun = logistic_model_function,
                color = 'blue') +
  xlim(c(min_x, max_x)) +
  ylim(c(0,1)) +
  xlab(X) +
  ylab(Y) +
  ggtitle(sprintf("Scatterplot of %s vs. %s (+ model)", Y, X)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data_summary,
       aes(y = y_mean, x = class_reps)) +
  geom_line() +
  geom_point() +
  stat_function(fun = logistic_model_function,
                color = 'blue') +
  xlim(c(min_x, max_x)) +
  ylim(c(0,1)) +
  ggtitle(sprintf("Mean %s vs. %s classes (+ model)", Y, X)) +
  theme(plot.title = element_text(hjust = 0.5))

# f)
## Likelihood ratio test
likelihood_without_variables <- function(y){
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  n <- n1 + n0
  
  likelihood <- ((n1/n)^n1) * ((n0/n)^n0)
  
  return(likelihood)
}

logistic_likelihood <- function(y, model_predictions){
  likelihood <- prod((model_predictions^y) *
                       (1-model_predictions)^(1-y))
  
  return(likelihood)
}

predictions <- predict(logistic_model,
                       type = 'response')

G <- -2*log(likelihood_without_variables(dataset[[Y]]) /
              logistic_likelihood(dataset[[Y]], predictions))
G

pchisq(G, df = 1, lower.tail = F)

## Wald test
### standard error
coef(summary(logistic_model))[2, 2]

wald_statistic <- coef(logistic_model)[2] /
  coef(summary(logistic_model))[2, 2]
wald_statistic

2 * (1 - pnorm(q = abs(wald_statistic)))

## Score test
compute_st_statistic <- function(x, y){
  st_numerator <- sum(x*(y - mean(y)))
  st_denominator <- sqrt(mean(y) *
                           (1-mean(y)) *
                           sum((x-mean(x))^2))
  
  st_statistic <- st_numerator / st_denominator
  
  return(st_statistic)
}

st_statistic <- compute_st_statistic(dataset[[X]],
                                     dataset[[Y]])
st_statistic

2 * (1 - pnorm(q = abs(st_statistic)))

## Deviance
logistic_model$deviance

# g)
alpha <- 0.05

confint.default(logistic_model,
                parm = 2,
                level = 1 - alpha)
