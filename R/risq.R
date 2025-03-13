#' Representativity Indicators for Survey Quality
#'
#' @description
#' Build a `risq` object that can be used as input for indicator functions.
#'
#' @param formula An object of class [`formula`][stats::formula]. Specifies the
#'  response model that will be used to calculate indicators. The left-hand side
#'  states the response variable, and the right-hand side describes the linear
#'  model of auxiliary variables used to explain the response.
#' @param family An optional string that specifies the regression type. Use
#'  `"binomial"` for logistic regression or `"gaussian"` for linear regression.
#'  If not provided, logistic regression is used.
#' @param data A [`data.frame`][base::data.frame] with sample data. Must contain
#'  all variables used in `formula`. The response variable must be `logical`.
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
#' response_formula <- formula(response ~ gender + age)
#' risq_hlc <- risq(formula = response_formula, data = hlc)
risq <- function(
  formula,
  family = c("binomial", "gaussian"),
  data,
  weights = NULL,
  strata = NULL
) {
  # Input validation: formula.
  validate_formula(formula)

  # Input validation: family.
  family <- match.arg(family)

  # Input validation: data.
  validate_data(data, formula)
  sample_cnt <- nrow(data)

  # Input validation: weights.
  if (is.null(weights)) {
    weights <- build_default_weights(sample_cnt)
  }
  validate_weights(weights, sample_cnt)

  # Input validation: strata.
  if (is.null(strata)) {
    strata <- build_default_strata(weights)
  }
  validate_strata(strata, sample_cnt)

  model <- build_model(formula, family)
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

validate_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be an object of class formula")
  }

  lhs_variables <- get_lhs_variables(formula)
  rhs_variables <- get_rhs_variables(formula)

  if (length(lhs_variables) != 1) {
    stop("`formula` must have exactly 1 dependent variable")
  }

  if (any(lhs_variables %in% rhs_variables)) {
    stop("`formula` dependent variable must not also be independent variable")
  }
}

validate_data <- function(data, formula) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame")
  }

  lhs_variables <- get_lhs_variables(formula)
  rhs_variables <- get_rhs_variables(formula)
  formula_variables <- c(lhs_variables, rhs_variables)

  if (!all(formula_variables %in% colnames(data))) {
    stop("`data` must contain all variables used in `formula`")
  }

  for (v in lhs_variables) {
    if (!is.logical(data[[v]])) {
      stop("dependent variable `", v, "` must be a logical vector")
    }
  }
}

validate_weights <- function(weights, expected_length) {
  if (!is.numeric(weights)) {
    stop("`weights` must be a numeric vector")
  }
  if (length(weights) != expected_length) {
    stop("`weights` length must match sample data")
  }
  if (any(weights < 1)) {
    stop("`weights` must have value 1 or greater")
  }
}

validate_strata <- function(strata, expected_length) {
  if (!is.factor(strata)) {
    stop("`strata` must be a factor")
  }
  if (length(strata) != expected_length) {
    stop("`strata` length must match sample data")
  }
}
