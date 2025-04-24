#' Representativity Indicators for Survey Quality
#'
#' @description
#' Build a `risq` object that can be used as input for indicator functions.
#'
#' @param predictor An object of class [`formula`][stats::formula] describing
#'  the linear model of explanatory variables used to predict the response.
#'  The left-hand side must be empty.
#' @param family An optional string that specifies the regression type. Use
#'  `"binomial"` for logistic regression or `"gaussian"` for linear regression.
#'  If not provided, logistic regression is used.
#' @param data A [`data.frame`][base::data.frame] with sample data. Must contain
#'  all variables used in `predictor`. The response variable must be `logical`.
#' @param weights An optional `numeric` vector with the inclusion weights of the
#'  sampling units. Values must be strictly positive. If not provided, the
#'  inclusion weights are set to `1`.
#' @param strata An optional `factor` with the strata membership of the sampling
#'  units. If not provided, a default is created based on the inclusion weights.
#'  The default uses a stratum for each unique weight, up to a maximum of `20`.
#'  If the number of unique weights exceeds `20`, a single stratum is used.
#'
#' @return
#' A `risq` object that can be used as input for indicator functions. The `risq`
#' object stores the validated input, as well as derived information like the
#' type of the sample design.
#'
#' @export
#' @examples
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
risq <- function(
  predictor,
  family = c("binomial", "gaussian"),
  data,
  weights = NULL,
  strata = NULL
) {
  # Input validation: predictor.
  if (!inherits(predictor, "formula")) {
    stop("`predictor` must be an object of class formula")
  }
  if (length(predictor) > 2) {
    stop("`predictor` left-hand side must be empty")
  }

  # Input validation: family.
  family <- match.arg(family)

  # Input validation: data.
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame")
  }
  predictor_variables <- all.vars(predictor)
  if (!all(predictor_variables %in% colnames(data))) {
    stop("`data` must contain all variables used in `predictor`")
  }
  if (any(is.na(data[predictor_variables]))) {
    stop("`data` for `predictor` variables must not contain `NA` values")
  }

  sample_cnt <- nrow(data)

  # Input validation: weights.
  if (is.null(weights)) {
    weights <- build_default_weights(sample_cnt)
  }
  if (!is.numeric(weights)) {
    stop("`weights` must be a numeric vector")
  }
  if (length(weights) != sample_cnt) {
    stop("`weights` length must match sample data")
  }
  if (any(is.na(weights))) {
    stop("`weights` must not contain `NA` values")
  }
  if (any(weights < 1)) {
    stop("`weights` must have value 1 or greater")
  }

  # Input validation: strata.
  if (is.null(strata)) {
    strata <- build_default_strata(weights)
  }
  if (!is.factor(strata)) {
    stop("`strata` must be a factor")
  }
  if (length(strata) != sample_cnt) {
    stop("`strata` length must match sample data")
  }
  if (any(is.na(strata))) {
    stop("`strata` must not contain `NA` values")
  }

  # Build model and design.
  model <- build_model(predictor, family)
  design <- build_design(weights, strata)

  # Build risq object.
  result <- list(
    model = model,
    data = data,
    design = design
  )
  class(result) <- "risq"

  result
}
