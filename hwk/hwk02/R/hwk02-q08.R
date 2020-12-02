find_c <- function(alpha, n) {
  c <- qchisq(p = alpha, df = n - 1, lower.tail = FALSE)
  return(c)
}

c <- find_c(0.025, 13)
round(c, 3)



rm(list = ls())
