# Build model for fiting survey data.
build_model <- function(predictor, family) {
  list(predictor = predictor, family = family)
}

# Fit model with given target variable to weighted data. Returns quantities
# relevant for indicator calculation (propensities, sigma and z).
fit_model <- function(model, target, data, weights) {
  # Build model formula of form target ~ predictor.
  formula <- build_formula(target, model$predictor)

  # Determine desired model behaviour based on the model$family string.
  # Note that link_grad() is the derivative of the link function.
  regression <- switch(
    model$family,
    "binomial" = list(
      family = stats::binomial(link = "logit"),
      link_grad = function(mu) exp(mu) / (1 + exp(mu))^2
    ),
    "gaussian" = list(
      family = stats::gaussian(link = "identity"),
      link_grad = function(mu) 1
    )
  )

  # Add the weights to the data for glm(), using a column name that is unlikely
  # to exist. Stop if it already exists, to prevent unexpected behavour.
  weights_colname <- "(risq_weights)"
  if (weights_colname %in% names(data)) {
    stop("data must not contain a variable named ", weights_colname)
  }
  data[[weights_colname]] <- weights / mean(weights)

  # Fit model to data using glm(). Note that weights are scaled with their mean,
  # leading to non-integer weights. This triggers a warning in glm() if family
  # is binomial. This warning is not relevant for the user, so we muffle it.
  # This may not work if the user is using R in a language other than English.
  withCallingHandlers(
    modelfit <- do.call(
      what = stats::glm,
      args = list(formula, regression$family, data, as.name(weights_colname))
    ),
    warning = function(w) {
      warning_msg <- conditionMessage(w)
      if (startsWith(warning_msg, "non-integer #successes")) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # Calculate propensities and other relevant quantities.
  prop <- stats::predict(modelfit, type = "response")
  sigma <- stats::vcov(modelfit)
  x <- stats::model.matrix(formula, data)[, colnames(sigma)]
  z <- regression$link_grad(stats::predict(modelfit, type = "link")) * x

  list(
    prop = prop,
    sigma = sigma,
    z = z
  )
}

# Build formula from given `target` string and `predictor` formula. The result
# is a formula with target as left-hand side and predictor as right-hand side.
build_formula <- function(target, predictor) {
  target_formula <- stats::formula(paste(target, "~ ."))
  formula <- stats::update(target_formula, predictor)

  formula
}

# Check if `formula` has a non-empty left-hand side.
has_lhs <- function(formula) {
  length(formula) > 2
}

# Get variables used in right-hand side of `formula`.
get_rhs_variables <- function(formula) {
  rhs <- if(length(formula) > 2) formula[[3L]] else formula[[2L]]
  all.vars(rhs)
}
