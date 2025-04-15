# Module with functions for calculating weighted mean and variance.

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

# Calculate the weighted means by category, for all categories specified by a
# given list of factors. Returns a vector of the same size as x.
weighted_means_by_cat <- function(x, weights, categories) {
  numer <- stats::ave(weights * x, categories, FUN = sum)
  denom <- stats::ave(weights, categories, FUN = sum)
  numer / denom
}

# Calculate the weighted variances by category, for all categories specified by
# a given list of factors. Returns a vector of the same size as x.
weighted_vars_by_cat <- function(x, weights, categories) {
  x_mean <- weighted_means_by_cat(x, weights, categories)
  x_var <- weights * (x - x_mean)^2 / sum(weights)
}
