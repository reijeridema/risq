# Calculate the weighted mean.
weighted_mean <- function(x, weights) {
  stats::weighted.mean(x, weights)
}

# Calculate the weighted variance (unbiased or ML).
# Note that unbiased calculation requires sum(weights) > 1.
weighted_var <- function(x, weights, method = c("unbiased", "ML")) {
  method <- match.arg(method)
  N <- sum(weights)

  x_mean <- stats::weighted.mean(x, weights)
  x_var_numer <- sum(weights * (x - x_mean)^2)
  x_var_denom <- switch(method, "unbiased" = N - 1, "ML" = N)
  
  x_var_numer / x_var_denom
}
