power_fun <- function(theta, c, n) {
  c_alt <- sqrt(n) * (c - theta)
  val <- pnorm(c_alt, 0, 1, lower.tail = FALSE)
  return(val)
}

test_info <- function(theta_hyp, theta0, power_at_theta0, n) {
  c_alt <- qnorm(power_at_theta0, 0, 1, lower.tail = FALSE)
  c <- theta0 + c_alt / sqrt(n)
  alpha <- power_fun(theta_hyp, c, n)
  return(
    list(c = c, alpha = alpha)
  )
}

pars <- test_info(0, 1, 0.95, 16)
purrr::map_dbl(.x = pars, .f = ~signif(., 3))



rm(list = ls())
