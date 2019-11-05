library(ggplot2)
library(data.table)
library(dplyr)

# https://www.jstor.org/stable/2347496

setwd("~/Desktop/machine-learning-projects/Logistic Regression/Hosmer Applied Logistic Regression Exercises")

icu_data <- fread("datasets/ICU/ICU.txt", header = T)

icu_data_nonwhite <- icu_data %>%
  mutate(NONWHITE = ifelse(RACE == 1, 0, 1))

ggplot(data = icu_data_nonwhite) +
  geom_point(aes(y = STA, x = NONWHITE)) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  ggtitle("Scatterplot of STA vs. NONWHITE") +
  theme(plot.title = element_text(hjust = 0.5))


logistic_model <- glm(STA ~ NONWHITE,
                      data = icu_data_nonwhite,
                      family = binomial)

profile_interval <- confint(logistic_model, parm = "NONWHITE", level = 0.95)
wald_interval <- confint.default(logistic_model, parm = "NONWHITE", level = 0.95)
nonwhite_parameter <- coef(logistic_model)[2]

# assymetry
A <- 100*((profile_interval[2] - nonwhite_parameter) - (nonwhite_parameter - profile_interval[1])) / diff(profile_interval)
A

100*((profile_interval[2] - profile_interval[1]) / (wald_interval[2] - wald_interval[1]) - 1)

# Manual profile likelihood confidence intervals
beta1 <- seq(from = -3, to = 1, by = 0.1)

profile_loglik <- NULL
for(i in 1:length(beta1)){
  temp_model <- glm(STA ~ 1,
                    data = icu_data_nonwhite,
                    offset = beta1[i] * NONWHITE,
                    family = binomial)
  profile_loglik <- c(profile_loglik, logLik(temp_model))
  
}

h <- logLik(logistic_model) - qchisq(0.95, 1) / 2

ggplot() +
  geom_line(aes(x = beta1, y = profile_loglik)) +
  geom_line(aes(x = beta1, y = h), color = 'red') +
  ylab("log-likelihood") +
  xlab("beta 1")
  

function_to_optimize <- function(beta, data, maxLogLik){
  temp_model <- glm(STA ~ 1,
                    data = data,
                    offset = beta * NONWHITE,
                    family = binomial)
  logLik(temp_model) - maxLogLik + qchisq(0.95, 1) / 2
}

uniroot(function_to_optimize, c(-2.5, -2), data = icu_data_nonwhite, maxLogLik = logLik(logistic_model))$root
uniroot(function_to_optimize, c(0, 0.5), data = icu_data_nonwhite, maxLogLik = logLik(logistic_model))$root

# 100*((0.162 - 0.067) / (0.158 - 0.064) - 1)

# Using the data from the ICU study create a dichotomous variable NONWHITE
# (NONWHITE = 1 if RACE = 2 or 3 and NONWHITE = 0 if RACE = 1).
# Fit the logistic regression of STA on NONWHITE and show that the 95 per-
#   cent profile likelihood confidence interval for the coefficient for nonwhite has
# asymmetry of −13% and that this interval is 26% wider than the Wald-based
# interval. This example points out that even when the sample size and number
# of events are large n = 200, and n 1 = 40 there can be substantial asymmetry
# and differences between the two interval estimators. Explain why this is the
# case in this example.

