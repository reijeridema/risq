#' Coefficient of Variation
#'
#' @description
#' Estimate the bias-adjusted coefficient of variation for a target variable,
#' within the context of the given [`risq`][risq()] object.
#'
#' @param x A `risq` object.
#' @param target Name of the target variable. Must be the name of a logical
#'  variable in the `risq` object data.
#' @param include_se An optional logical specifying whether to include the
#'  standard error or not. If omitted, the standard error is included.
#'
#' @return
#' Named `list` with the bias-adjusted coefficient of variation `value` and,
#' if `include_se` is `TRUE`, the associated standard error `se`.
#'
#' @seealso `risq` object constructor: [risq()]
#' @family coefficient of variation functions
#'
#' @examples
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' cv(risq_hlc, "response_1")
#'
#' @export
cv <- function(x, target, include_se = TRUE) {
  # Input validation.
  validate_risq_object(x)
  validate_target(x, target)
  validate_logical(include_se)

  weights <- x$design$weights
  design_var_func <- function(y) {calc_design_total_var(y, x$design)}

  # Fit model and calculate bias factor.
  fit <- fit_model(x$model, target, x$data, weights)
  bias_factor <- calc_bias_factor(
    fit$prop, fit$sigma, fit$z, weights, design_var_func
  )

  # Calculate coefficient of variation values.
  cv_value <- calc_cv(fit$prop, weights, bias_factor)
  result <- list(value = cv_value)

  if (include_se) {
    cv_se <- calc_cv_se(
      fit$prop, fit$sigma, fit$z, weights, design_var_func, cv_value
    )
    result$se <- cv_se
  }

  result
}
