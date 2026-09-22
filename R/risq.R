#' RISQ: Representativity Indicators for Survey Quality
#'
#' @description
#' Build a `risq` object that can serve as input for RISQ functions.
#'
#' @param predictor An object of class [`formula`][stats::formula] describing
#'  the linear model of explanatory variables used to predict the response.
#'  The left-hand side must be empty.
#' @param family An optional string that specifies the regression type. Use
#'  `"binomial"` for logistic regression or `"gaussian"` for linear regression.
#'  If omitted, logistic regression is used.
#' @param data A data frame with sample data of a survey. Must contain all
#'  variables used in `predictor`. Response variables must be logical. Other
#'  variables must be factors if they are to be used for the calculation of
#'  partial indicators.
#' @param weights An optional vector with inclusion weights for the sampling
#'  units. Values must be strictly positive. If omitted, the inclusion weights
#'  are set to `1`.
#' @param strata An optional factor with the strata membership of the sampling
#'  units. If omitted, a default is created based on the inclusion weights.
#'  The default uses a stratum for each unique weight, up to a maximum of `20`.
#'  If the number of unique weights exceeds `20`, a single stratum is used.
#'
#' @return
#' A `risq` object that can serve as input for other functions in this package.
#'
#' @seealso Response rate: [rr()]
#' @seealso Representativity indicator: [ri()], [ri_by_var()], [ri_by_cat()]
#' @seealso Coefficient of variation: [cv()], [cv_by_var()], [cv_by_cat()]
#'
#' @family risq methods
#'
#' @examples
#' risq(predictor = ~ gender + age, data = hlc)
#'
#' @export
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
    stop("`weights` length must match `data`")
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
    stop("`strata` length must match `data`")
  }
  if (any(is.na(strata))) {
    stop("`strata` must not contain `NA` values")
  }
  strata_no_empty <- droplevels(strata)
  if (nlevels(strata_no_empty) != nlevels(strata)) {
    strata <- strata_no_empty
    warning("removed empty levels from `strata`")
  }

  # Build model and design.
  model <- build_model(predictor, family)
  design <- build_design(weights, strata)

  # Build risq object.
  build_risq(model, data, design)
}

#' Print a RISQ object
#'
#' @description
#' Print a description of a [`risq`][risq()] object.
#'
#' @param x A `risq` object.
#' @param ... Unused.
#'
#' @return
#' No return value.
#'
#' @family risq methods
#'
#' @export
print.risq <- function(x, ...) {
  cat("model:")
  cat(paste0("\n  predictor: ", deparse(x$model$predictor)))
  cat(paste0("\n  family: ", x$model$family))
  cat("\ndata:")
  cat(paste0("\n  samples: ", nrow(x$data)))
  cat(paste0("\n  variables: ", ncol(x$data)))
  cat("\ndesign:")
  cat("\n  weights:")
  cat(paste0("\n    values: ", length(x$design$weights)))
  cat(paste0("\n    unique: ", length(unique(x$design$weights))))
  cat("\n  strata:")
  cat(paste0("\n    values: ", length(x$design$strata)))
  cat(paste0("\n    levels: ", length(levels(x$design$strata))))
  cat(paste0("\n  type: ", x$design$type))
  cat("\n")
  invisible(x)
}

# To be able to define sample.risq() as an S3 method for risq objects, we first
# need to define sample() as a generic function that defaults to base::sample().

#' Random Sampling
#'
#' @description
#' Generic function for taking samples. Defaults to [base::sample()].
#'
#' @param x Object to sample.
#' @param ... Further arguments to control the sampling.
#'
#' @return
#' A sample of `x`.
#'
#' @seealso [base::sample()], [sample.risq()]
#'
#' @export
sample <- function(x, ...) {
  UseMethod("sample")
}

#' @export
sample.default <- function(x, ...) {
  base::sample(x, ...)
}

#' Sample a RISQ object
#'
#' @description
#' Take a sample of the specified size from a [`risq`][risq()] object. The
#' sampling can be done either with or without replacement, and can be weighted
#' or unweighted. The default behaviour is unweighted without replacement.
#'
#' @param x A `risq` object.
#' @param size An optional positive integer specifying the number of items to
#'  sample. Defaults to the number of records in the `risq` object data.
#' @param ... Other arguments to be passed into [base::sample()] to control the
#'  sampling of records from the `risq` object.
#'
#' @return
#' A `risq` object with the same model as `x`, but with a sample of the data,
#' weights and strata of `x`.
#'
#' @family risq methods
#'
#' @export
sample.risq <- function(x, size = nrow(x$data), ...) {
  validate_positive_integer(size)

  # Draw random sample from row indices.
  n <- nrow(x$data)
  i <- sample(n, size, ...)

  # Builds components of sample risq object.
  model <- x$model
  data <- x$data[i, ]
  rownames(data) <- NULL
  design <- build_design(
    weights = x$design$weights[i],
    strata = droplevels(x$design$strata[i]),
    type = x$design$type
  )

  # Build sample risq object.
  build_risq(model, data, design)
}

# Build a risq object. For internal use only. Arguments are assumed to be valid.
build_risq <- function(model, data, design) {
  structure(
    list(
      model = model,
      data = data,
      design = design
    ),
    class = "risq"
  )
}
