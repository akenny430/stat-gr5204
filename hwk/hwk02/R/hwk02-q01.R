alpha <- dbinom(x = 6, size = 100, prob = 0.08)
round(alpha, 3)

beta <- 1 - dbinom(x = 6, size = 100, prob = 0.04)
round(beta, 3)



rm(list = ls())
