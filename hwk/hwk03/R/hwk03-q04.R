test_dist_same <- function(df, alpha = 0.05) {
  df <- as.matrix(df)
  r <- nrow(df)
  c <- ncol(df)
  nr <- vector("numeric", r)
  nc <- vector("numeric", c)
  for (i in 1:r) {
    nr[i] <- sum(df[i, ])
  }
  for (j in 1:c) {
    nc[j] <- sum(df[, j])
  }
  n <- sum(nr)
  E <- matrix(nrow = r, ncol = j)
  for (i in 1:r) {
    for (j in 1:c) {
      E[i, j] <- nr[i] * nc[j] / n
    }
  }
  Q <- sum(
    (df - E)^2 / E
  )
  qstat <- qchisq(p = alpha, df = (r - 1) * (c - 1), lower.tail = FALSE)
  rejyn <- ifelse(Q >= qstat, "Reject H0", "Don't reject H0")
  return(
    list(Q = Q, qa = qstat, reject = rejyn)
  )
}

test_dist_same(
  matrix(
    data = rbind(
      c(24, 6, 5, 15), 
      c(43, 24, 7, 26), 
      c(69, 47, 22, 62)
    ),
    nrow = 3, ncol = 4
  ),
  alpha = 0.1
)
