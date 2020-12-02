library(ggplot2)
library(data.table)
myred <- "#ff6666"

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

power_fun(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5), 0.025, 13)

ggplot(
  data.table(sig2 = seq(1, 5, 0.01)),
  aes(sig2)
) +
  geom_function(fun = power_fun, args = list(sig20 = 1, alpha = 0.025, n = 13), color = myred, lwd = 2) +
  labs(x = expression(sigma^2), y = expression(pi(sigma^2 , delta))) +
  theme_bw(base_size = 30)
ggsave("hwk/hwk02/img/q08-power-function.png")




rm(list = ls())
