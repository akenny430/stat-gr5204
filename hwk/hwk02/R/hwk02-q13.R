n <- 16

power_at <- 0.95

c_alt <- qnorm(power_at, 0, 1, lower.tail = FALSE)

c <- 1 + c_alt / sqrt(n)

