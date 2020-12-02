confint <- function(gamma = 0.95, y, n) {
  z <- qnorm((1 + gamma) / 2, 0, 1)
  phat <- y / n
  palt <- (phat + z^2 / (2 * n)) / (1 + z^2 / (2 * n))
  ster <- sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  ends <- palt + z * ster * c(-1, 1)
  return(
    round(ends, 3)
  )
}

confint(0.90, 75, 300)



rm(list = ls())
