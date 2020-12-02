n <- 2
gamma <- 0.9
exp_width <- qt(p = (1 + gamma) / 2, df = n - 1)^2 / n
while (exp_width >= 1/8) {
  n <- n + 1
  exp_width <- qt(p = (1 + gamma) / 2, df = n - 1)^2 / n
}
print(n)



rm(list = ls())
