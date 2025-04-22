#' Response Rate
#'
#' @description
#' Calculate the response rate for a target variable, within the context of the
#' given `risq` object.
#'
#' @param robj `risq` object (see [`risq`][risq]).
#' @param target Name of the target variable. Must be the name of a `logical`
#'  variable in the `data` component of the `risq` object.
#'
#' @return
#' Named `list` with the bias-adjusted representativity indicator value `val`
#' and the associated standard error `se`.
#'
#' @export
#' @examples
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' rr_hlc <- rr(risq_hlc, "response")
rr <- function(robj, target) {
  # Input validation.
  validate_risq_object(robj)
  validate_target(robj, target)

  # Fit model and calculate response rate.
  weights <- robj$design$weights
  fit <- fit_model(robj$model, target, robj$data, weights)
  response_rate <- calc_rr(fit$prop, weights)

  response_rate
}
