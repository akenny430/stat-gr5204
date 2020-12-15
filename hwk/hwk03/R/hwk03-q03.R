test_prop_equal <- function(n, k, alpha = 0.05) {
  p0 <- 1 / k
  np0 <- n * p0
  q <- qchisq(p = alpha, df = k - 1, lower.tail = FALSE)
  n2 <- np0 * (q + 400)
  vec <- c(q, n2)
  names(vec) <- c("Q", "sum N^2")
  return(vec)
}

test_prop_equal(n = 400, k = 5, alpha = 0.01)
