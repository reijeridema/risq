#' Representativity Indicator
#'
#' @description
#' Calculate the bias-adjusted representativity indicator for a target variable,
#' within the context of the given `risq` object.
#'
#' @param robj `risq` object (see [`risq`][risq]).
#' @param target Name of the target variable. Must be the name of a `logical`
#'  variable in the `data` component of the `risq` object.
#'
#' @return
#' Named `list` with the bias-adjusted representativity indicator value `ri`
#' and the associated standard error `ri_se`.
#'
#' @export
#' @examples
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' ri_hlc <- ri(risq_hlc, "response")
ri <- function(robj, target) {
  # Input validation.
  validate_risq_object(robj)
  validate_target(robj, target)

  weights <- robj$design$weights
  design_var_func <- function(x) {calc_design_total_var(x, robj$design)}

  # Fit model and calculate bias factor.
  fit <- fit_model(robj$model, target, robj$data, weights)
  bias_factor <- calc_bias_factor(
    fit$prop, fit$sigma, fit$z, weights, design_var_func
  )

  # Calculate representativity indicator values.
  ri_val <- calc_ri(fit$prop, weights, bias_factor)
  ri_se <- calc_ri_se(fit$prop, fit$sigma, fit$z, weights, design_var_func)

  list(ri = ri_val, ri_se = ri_se)
}

# Calculate bias factor for R-indicator.
calc_bias_factor <- function(prop, sigma, z, weights, total_var_func) {
  N <- sum(weights)

  lambda_1_func <- function(zi) {t(zi) %*% sigma %*% zi}
  lambda_1 <- sum(apply(z * sqrt(weights), 1, lambda_1_func))
  lambda_2 <- total_var_func(prop) / N
  bias <- (lambda_1 - lambda_2) / N

  prop_var <- weighted_var(prop, weights)

  bias_factor <- max(1 - bias / prop_var, 0)

  bias_factor
}

# Calculate estimate for R-indicator.
calc_ri <- function(prop, weights, bias_factor) {
  prop_var <- weighted_var(prop, weights)
  ri <- 1 - 2 * sqrt(prop_var * bias_factor)

  ri
}

# Calculate standard error for R-indicator.
calc_ri_se <- function(prop, sigma, z, weights, total_var_func) {
  prop_mean <- weighted_mean(prop, weights)
  prop_var_ml <- weighted_var(prop, weights, method = "ML")
  prop_z <- cbind(prop, z)

  A <- stats::cov.wt(prop_z, wt = weights, method = "ML")$cov[-1, 1]
  B <- stats::cov.wt(z, wt = weights, method = "ML")$cov
  C <- total_var_func((prop - prop_mean)^2)

  variance <- numeric()
  variance[1] <- 4 * t(A) %*% sigma %*% A
  variance[2] <- 2 * sum(diag(B %*% sigma %*% B %*% sigma))
  variance[3] <- C / sum(weights)^2
  variance <- sum(variance) / prop_var_ml

  ri_se <- sqrt(variance)

  ri_se
}
