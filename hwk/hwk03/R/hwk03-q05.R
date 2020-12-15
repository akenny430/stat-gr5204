source("hwk/hwk03/R/hwk03-q04.R")

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

test_dist_same(
  matrix(
    data = rbind(
      c(24, 5, 6, 15), 
      c(43, 25, 6, 26), 
      c(69, 47, 22, 62)
    ),
    nrow = 3, ncol = 4
  ),
  alpha = 0.1
)
