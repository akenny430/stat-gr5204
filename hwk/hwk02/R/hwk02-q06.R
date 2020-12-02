library(ggplot2)
library(data.table)
myred <- "#ff6666"

power_fun <- function(theta, n) {
  par <- n * theta
  val <- exp(- par) * (1 + par + par^2 / 2)
  return(val)
}

ggplot(
  data.table(theta = seq(0.01, 0.5, 0.01)),
  aes(theta)
) +
  geom_function(fun = power_fun, args = list(n = 25), color = myred, lwd = 2) +
  labs(x = expression(theta), y = expression(pi(theta , delta))) +
  theme_bw(base_size = 30)
ggsave("hwk/hwk02/img/q06-power-function.png")



rm(list = ls())
