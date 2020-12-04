library(ggplot2)
library(data.table)
myred    <- "#ff6666"
myorange <- "#ffcc00"
myyellow <- "#ffff00"
mygreen  <- "#99ff33"
myblue   <- "#99ccff"
mydblue   <- "#0080ff"
mypink   <- "#ff99ff"
dark1   <- "#565657"
dark2   <- "#808081"
dark3   <- "#aaaaab"

data <- data.table(
  x = seq(0.5, 4, 0.5),
  y = 40 + c(0, 1, 3, 2, 4, 2, 3, 2)
)



# parts a and b -----------------------------------------------------------



linear_fit <- lm(y ~ x, data)

quadratic_fit <- lm(y ~ poly(x, 2, raw = TRUE), data)

coef_fit <- list(
  linear_fit$coefficients, 
  quadratic_fit$coefficients
)

purrr::map(coef_fit, ~round(., 3))



# part c ------------------------------------------------------------------



ggplot(data, aes(x, y)) +
  geom_point(color = dark1, cex = 4, pch = 1, stroke = 2) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = myred,
    lwd = 1.5
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 2, raw = TRUE),
    se = FALSE,
    color = myorange,
    lwd = 1.5
  ) +
  labs(x = "Coded Temperature", y = "Coded Curability") +
  theme_bw(base_size = 30)

ggsave("hwk/hwk03/img/q09-model-plots.png")



# rm(list = ls())
