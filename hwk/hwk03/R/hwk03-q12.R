source("hwk/hwk03/R/hwk03-q10.R")

linear_fit <- lm(y ~ x, data)

signif(
  predict(linear_fit, data.table(x = 2)), 3
)

var_of_prediction <- function(x0, df) {
  x <- df[[1]]
  barx <- mean(x)
  n <- nrow(df)
  xnorm <- sum((x - barx)^2)
  pred_var_coef <- 1 + 1 / n + (x0 - barx)^2 / xnorm
  sig <- purrr:::map_dbl(pred_var_coef, ~round(., 3))
  return(sig)
}

var_of_prediction(2, data)
