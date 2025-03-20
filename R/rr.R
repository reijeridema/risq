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
  response_rate <- weighted_mean(fit$prop, weights)

  response_rate
}

# Validate risq object.
validate_risq_object <- function(robj) {
  if (!is_risq(robj)) {
    stop("`robj` must be a risq object")
  }
}

# Validate target variable within the context of a valid risq object.
validate_target <- function(robj, target) {
  if (!is.character(target) || length(target) != 1) {
    stop("`target` must be a string")
  }

  predictor_variables <- get_rhs_variables(robj$model$predictor)
  is_target_in_predictor <- target %in% predictor_variables
  if (is_target_in_predictor) {
    stop("`target` variable must not be in risq predictor")
  }

  is_target_in_data <- target %in% names(robj$data)
  if (!is_target_in_data) {
    stop("`target` variable must be present in risq data")
  }

  is_target_logical <- is.logical(robj$data[[target]])
  if (!is_target_logical) {
    stop("`target` variable must be logical in risq data")
  }
}
