#' Coefficient of Variation
#'
#' @description
#' Estimate the bias-adjusted coefficient of variation for a target variable,
#' within the context of the given `risq` object.
#'
#' @param robj `risq` object (see [`risq`][risq]).
#' @param target Name of the target variable. Must be the name of a `logical`
#'  variable in the `data` component of the `risq` object.
#'
#' @return
#' Named `list` with the bias-adjusted coefficient of variation value `cv` and
#' the associated standard error `cv_se`.
#'
#' @export
#' @examples
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' cv_hlc <- cv(risq_hlc, "response")
cv <- function(robj, target) {
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

  # Calculate coefficient of variation values.
  cv_val <- calc_cv(fit$prop, weights, bias_factor)
  cv_se <- calc_cv_se(
    fit$prop, fit$sigma, fit$z, weights, design_var_func, cv_val
  )

  list(cv = cv_val, cv_se = cv_se)
}
