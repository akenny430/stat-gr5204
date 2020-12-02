find_c <- function(alpha, n) {
  c <- qchisq(p = alpha, df = n - 1, lower.tail = FALSE)
  return(c)
}

c <- find_c(0.025, 13)
round(c, 3)

power_fun <- function(sig2, sig20, alpha, n) {
  c <- find_c(alpha, n)
  calt <- c * sig20 / n
  val <- pgamma(calt, shape = (n - 1) / 2, rate = n / (2 * sig2), lower.tail = FALSE)
  return(val)
}

power_fun(1, 1, 0.025, 13)

rm(list = ls())
