#' Representativity Indicator
#'
#' @description
#' Estimate the bias-adjusted representativity indicator for a target variable,
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
