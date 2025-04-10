#' Representativity Indicator by Variable
#'
#' @description
#' Calculate bias-adjusted partial representativity indicator values by variable
#' for a selection of variables, given a `risq` object and target variable.
#'
#' @param robj `risq` object (see [`risq`][risq]).
#' @param target Name of the target variable. Must be the name of a `logical`
#'  variable in the `data` component of the `risq` object.
#' @param variables A `character` vector that specifies the variables for which
#'  to calculate partial representativity indicator values. Must be the names of
#'  categorical variables in the `data` component of the `risq` object.
#'
#' @return
#' A `data.frame` with columns
#' - `variable`: name of the variable for which the row holds values,
#' - `ri_u`: estimate for the unconditional partial representativity indicator
#'  for the variable,
#' - `ri_se_u`: standard error the for unconditional partial representativity
#'  indicator for the variable,
#' - `ri_c`: estimate for the conditional partial representativity indicator for
#'  the variable,
#' - `ri_se_c`: standard error for the conditional partial representativity
#'  indicator for the variable.
#'
#' @export
#' @examples
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' ri_by_var_hlc <- ri_by_var(risq_hlc, "response", c("gender", "age", "job"))
ri_by_var <- function(robj, target, variables) {
  # Input validation.
  validate_risq_object(robj)
  validate_target(robj, target)
  validate_variables(robj, target, variables)

  model <- robj$model
  data <- robj$data
  weights <- robj$design$weights
  design_var_func <- function(x) {calc_design_total_var(x, robj$design)}

  # Fit model and calculate bias factor.
  fit <- fit_model(model, target, data, weights)
  bias_factor <- calc_bias_factor(
    fit$prop, fit$sigma, fit$z, weights, design_var_func
  )

  # Build data frame with representativity indicator values for each variable.
  result <- NULL
  for (variable in variables) {
    categories <- data[[variable]]
    predictor_variables <- all.vars(model$predictor)
    other_variables <- predictor_variables[predictor_variables != variable]
    other_categories <- as.list(data[other_variables])

    ri_u <- calc_ri_by_var_unconditional(
      categories, fit$prop, weights, bias_factor
    )
    ri_se_u <- calc_ri_se_by_var_unconditional(
      variable, model$family, target, data, weights, design_var_func
    )
    ri_c <- calc_ri_by_var_conditional(
      other_categories, fit$prop, weights, bias_factor
    )

    result_single_var <- data.frame(
      variable = variable,
      ri_u = ri_u,
      ri_se_u = ri_se_u,
      ri_c = ri_c,
      ri_se_c = ri_se_u
    )
    result <- rbind(result, result_single_var)
  }

  # Return result.
  result
}

# Calculate estimate for the unconditional partial R-indicator.
calc_ri_by_var_unconditional <- function(
  categories, prop, weights, bias_factor
) {
  wp <- data.frame(w = weights, wp = weights * prop)
  wp_by_cat <- stats::aggregate(wp, by = list(category = categories), FUN = sum)

  prop_by_cat <- wp_by_cat$wp / wp_by_cat$w
  weights_by_cat <- wp_by_cat$w
  prop_var <- weighted_var(prop_by_cat, weights_by_cat, method = "ML")
  ri <- sqrt(prop_var * bias_factor)

  ri
}

# Calculate standard error for the unconditional partial R-indicator.
calc_ri_se_by_var_unconditional <- function(
  variable, family, target, data, weights, total_var_func
) {
  predictor <- stats::formula(paste("~", variable))
  model <- build_model(predictor, family)
  fit <- fit_model(model, target, data, weights)
  ri_se <- 0.5 * calc_ri_se(
    fit$prop, fit$sigma, fit$z, weights, total_var_func
  )

  ri_se
}

# Calculate estimate for the conditional partial R-indicator.
calc_ri_by_var_conditional <- function(
  other_categories, prop, weights, bias_factor
) {
  prop_var_by_others <- weighted_vars_by_cat(prop, weights, other_categories)
  ri <- sqrt(sum(prop_var_by_others) * bias_factor)

  ri
}
