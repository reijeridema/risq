#' Response Rate
#'
#' @description
#' Calculate the response rate for a target variable, within the context of the
#' given [`risq`][risq()] object.
#'
#' @param x A `risq` object.
#' @param target Name of the target variable. Must be the name of a logical
#'  variable in the `data` component of the `risq` object.
#'
#' @return
#' Response rate numeric value.
#'
#' @seealso `risq` object constructor: [risq()]
#'
#' @examples
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' rr_hlc <- rr(risq_hlc, "response")
#'
#' @export
rr <- function(x, target) {
  # Input validation.
  validate_risq_object(x)
  validate_target(x, target)

  # Fit model and calculate response rate.
  weights <- x$design$weights
  fit <- fit_model(x$model, target, x$data, weights)
  response_rate <- calc_rr(fit$prop, weights)

  response_rate
}
