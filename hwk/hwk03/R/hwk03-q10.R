library(data.table)

data <- data.table(
  x = c(1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 4.6, 1.6, 5.5, 3.4),
  y = c(0.7, -1.0, -0.2, -1.2, -0.1, 3.4, 0.0, 0.8, 3.7, 2.0)
)

purrr:::map_dbl(data, mean)



# part a ------------------------------------------------------------------



get_mle <- function(df) {
  x <- df[[1]]
  y <- df[[2]]
  n <- nrow(df)
  model_fit <- lm(y ~ x, df)
  model_coef <- model_fit$coefficients
  model_resid <- model_fit$residuals
  sigma2 <- sum(model_resid^2) / n
  mle_pars <- c(model_coef[1], model_coef[2], sigma2)
  mle_pars_sig <- purrr::map_dbl(mle_pars, ~signif(., 3))
  names(mle_pars_sig) <- c("b0", "b1", "sigma2")
  return(mle_pars_sig)
}

get_mle(data)



# parts b and c -----------------------------------------------------------



coef_var <- function(df) {
  x <- df[[1]]
  barx <- mean(x)
  n <- nrow(df)
  xnorm <- sum((x - barx)^2)
  b0 <- 1 / xnorm
  b1 <- 1 / n + barx^2 / xnorm
  cov <- - barx / xnorm
  cor <- cov / sqrt(b0 * b1)
  coef_pars <- c(b0, b1, cov, cor)
  coef_pars_sig <- purrr::map_dbl(coef_pars, ~signif(., 3))
  names(coef_pars_sig) <- c("var(b0)", "var(b1)", "cov(b0,b1)", "corr(b0,b1)")
  return(coef_pars_sig)
}

coef_var(data)



# rm(list = ls())
