source("hwk/hwk03/R/hwk03-q10.R")

linear_combo_var <- function(df, a, b) {
  var_info <- coef_var(df)
  lin_com_coef <- a^2 * var_info[1] + b^2 * var_info[2] + 2 * a * b * var_info[4]
  coef_sig <- purrr::map_dbl(lin_com_coef, ~signif(., 3))
  names(coef_sig) <- "var(lin_com)"
  return(lin_com_coef)
}

linear_combo_var(data, 3, -2)
