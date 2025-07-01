#' RISQ Bootstrap
#'
#' @description
#' Draw random samples from a [`risq`][risq()] object and apply a
#' representativity indicator or coefficient of variation function to these
#' samples. The result is a `bootstrap` object from which statistical quantities
#' can be derived using `bootstrap` class methods.
#'
#' @param x The `risq` object to sample from.
#' @param fun The function to apply to the bootstrap samples. Can be any
#'  representativity indicator or coefficient of variation function.
#' @param ... Other arguments to pass into `fun`. Must not include `include_se`.
#' @param seed An optional integer that is used to seed the random number
#'  generator for sampling. If omitted, the current seed is left intact.
#'  For details, see [set.seed()].
#' @param iterations An optional positive integer specifying the number of
#'  samples to draw and apply `fun` to.
#'
#' @return
#' A `bootstrap` object.
#'
#' @seealso `risq` object constructor: [risq()]
#' @seealso Representativity indicator: [ri()], [ri_by_var()], [ri_by_cat()]
#' @seealso Coefficient of variation: [cv()], [cv_by_var()], [cv_by_cat()]
#'
#' @family bootstrap methods
#'
#' @examples
#' # Note: a low iteration count is used to limit computing time of example.
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' bs <- bootstrap(risq_hlc, ri, target = "response", iterations = 5)
#' mean(bs)
#' var(bs)
#'
#' @export
bootstrap <- function(x, fun, ..., seed = NULL, iterations = 1000L) {
  # Input validation.
  validate_risq_object(x)
  if (!is.null(seed)) {
    validate_integer(seed)
  }
  validate_positive_integer(iterations)

  # Set seed, if provided.
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Perform bootstrap iterations.
  i <- 0
  n <- nrow(x$data)
  while (i < iterations) {
    sample_x <- sample(x, size = n, replace = TRUE)
    indicator <- fun(sample_x, ..., include_se = FALSE)
    if (i == 0) {
      # Initialize bootstrap components based on indicator.
      values_df <- if (is.data.frame(indicator)) {
        indicator[names(indicator) != "value"]
      } else {
        NULL
      }
      indicator_value_len <- length(indicator$value)
      values <- matrix(nrow = indicator_value_len, ncol = iterations)
    }
    values[, i + 1] <- indicator$value
    i <- i + 1
  }

  # Check bootstrap values for issues.
  if (any(is.na(values))) {
    warning("Some bootstrap samples resulted in NA values.")
  }

  # Build bootstrap object.
  build_bootstrap(seed, values_df, values)
}

#' Arithmetic Mean of a Bootstrap
#'
#' @description
#' Calculate the arithmetic mean over all bootstrap iterations for each
#' indicator in the [`bootstrap`][bootstrap()] object.
#'
#' @param x A `bootstrap` object.
#' @param ... Additional arguments to control the mean calculation.
#' For details see [base::mean()].
#'
#' @return
#' For a bootstrap using a non-partial indicator function, a single mean value.
#' For a bootstrap using a partial indicator function, a data frame with the
#' same shape as the output of that partial indicator function.
#'
#' @family bootstrap methods
#'
#' @examples
#' # Note: a low iteration count is used to limit computing time of example.
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' bs <- bootstrap(
#'   risq_hlc, ri_by_var, target = "response", variables = "age", iterations = 5
#' )
#' mean(bs)
#'
#' @export
mean.bootstrap <- function(x, ...) {
  mean_func <- function(y) {base::mean(y, ...)}

  result <- if (is.null(x$values_df)) {
    stopifnot(nrow(x$values) == 1)  # Sanity check.
    mean_func(x$values[1, ])
  } else {
    mean_values <- apply(x$values, 1, mean_func)
    cbind(x$values_df, data.frame(value = mean_values))
  }

  result
}

# To be able to define var.bootstrap() as an S3 method for bootstrap objects, we
# first need to define var() as a generic function defaulting to stats::var().

#' Variance
#'
#' @description
#' Generic function for calculating variance. Defaults to [stats::var()].
#'
#' @param x Object to calculate variance for.
#' @param ... Further arguments to control variance calculation.
#'
#' @return
#' The variance of `x`.
#'
#' @seealso [stats::var()], [var.bootstrap()]
#'
#' @export
var <- function(x, ...) {
  UseMethod("var")
}

#' @export
var.default <- function(x, ...) {
  stats::var(x, ...)
}

#' Variance of a Bootstrap
#'
#' @description
#' Calculate the variance over all bootstrap iterations for each indicator in
#' the [`bootstrap`][bootstrap()] object.
#'
#' @param x A `bootstrap` object.
#' @param ... Additional arguments to control the variance calculation.
#' For details see [stats::var()].
#'
#' @return
#' For a bootstrap using a non-partial indicator function, a single variance
#' value. For a bootstrap using a partial indicator function, a data frame with
#' the same shape as the output of that partial indicator function.
#'
#' @family bootstrap methods
#'
#' @examples
#' # Note: a low iteration count is used to limit computing time of example.
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' bs <- bootstrap(
#'   risq_hlc, ri_by_cat, target = "response", variables = "age", iterations = 5
#' )
#' var(bs)
#'
#' @export
var.bootstrap <- function(x, ...) {
  var_func <- function(y) {stats::var(y, ...)}

  result <- if (is.null(x$values_df)) {
    stopifnot(nrow(x$values) == 1)  # Sanity check.
    var_func(x$values[1, ])
  } else {
    var_values <- apply(x$values, 1, var_func)
    cbind(x$values_df, data.frame(value = var_values))
  }

  result
}

#' Quantiles of a Bootstrap
#'
#' @description
#' Calculate sample quantiles over all bootstrap iterations for each indicator
#' in the [`bootstrap`][bootstrap()] object.
#'
#' @param x A `bootstrap` object.
#' @param ... Additional arguments to control the quantile calculation.
#' For details see [stats::quantile()].
#'
#' @return
#' For a bootstrap using a non-partial indicator function, a vector with
#' quantile values. For a bootstrap using a partial indicator function, a data
#' frame with the same shape as the output of that partial indicator function,
#' but with a quantile column for each entry in `probs`, instead of a single
#' `value` column.
#'
#' @family bootstrap methods
#'
#' @examples
#' # Note: a low iteration count is used to limit computing time of example.
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' bs <- bootstrap(risq_hlc, cv, target = "response", iterations = 5)
#' quantile(bs, probs = c(0.05, 0.95))
#'
#' @importFrom stats quantile
#' @export
quantile.bootstrap <- function(x, ...) {
  quantile_func <- function(y) {stats::quantile(y, ...)}

  result <- if (is.null(x$values_df)) {
    stopifnot(nrow(x$values) == 1)  # Sanity check.
    quantile_func(x$values[1, ])
  } else {
    quantile_list <- apply(x$values, 1, quantile_func, simplify = FALSE)
    quantile_values <- do.call(rbind, quantile_list)
    cbind(x$values_df, quantile_values)
  }

  result
}

#' Bootstrap Subtraction
#'
#' @description
#' Calculate the difference between two [`bootstrap`][bootstrap()] objects.
#' This can be used to measure the progression between two target variables.
#'
#' @param x A `bootstrap` object.
#' @param y A `bootstrap` object.
#'
#' @return
#' A `bootstrap` object with the difference between `x` and `y`.
#'
#' @details
#' For the result to be meaningful, the `bootstrap` objects `x` and `y` must
#' have been made with the same input arguments for the [bootstrap()] function,
#' except for the target variable.
#'
#' @family bootstrap methods
#'
#' @examples
#' # Prepare data with multiple response columns
#' data <- hlc[c("age", "gender")]
#' data$resp1 <- hlc$response
#' no_resp <- which(!hlc$response)
#' resp2 <- hlc$response
#' resp2[no_resp[seq(1, length(no_resp), 2)]] <- TRUE
#' data$resp2 <- resp2
#'
#' # Note: a low iteration count is used to limit computing time of example.
#' risq_hlc <- risq(predictor = ~ age +gender, data = data)
#' bs1 <- bootstrap(risq_hlc, ri, target = "resp1", seed = 0, iterations = 5)
#' bs2 <- bootstrap(risq_hlc, ri, target = "resp2", seed = 0, iterations = 5)
#' mean(bs2 - bs1)
#'
#' @export
`-.bootstrap` <- function(x, y) {

  if (missing(y)) {
    stop("Negation not supported for bootstrap objects")
  }
  if (!inherits(y, "bootstrap")) {
    stop("Arguments must be bootstrap objects")
  }
  if (is.null(x$seed) || is.null(y$seed) || x$seed != y$seed) {
    stop("Arguments must have the same non-NULL seed")
  }
  if (!identical(x$values_df, y$values_df)) {
    stop("Arguments must have the same data rows")
  }
  if (ncol(x$values) != ncol(y$values)) {
    stop("Arguments must have the same number of iterations")
  }

  values <- x$values - y$values
  build_bootstrap(x$seed, x$values_df, values)
}

# Build a bootstrap object. For internal use only. Arguments are assumed valid.
build_bootstrap <- function(seed, values_df, values) {
  structure(
    list(
      seed = seed,
      values_df = values_df,
      values = values
    ),
    class = "bootstrap"
  )
}
