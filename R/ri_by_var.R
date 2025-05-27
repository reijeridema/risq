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
#'  The returned conditional values `ri_c` en `ri_se_c` are `NA` for a variable
#'  if that variable is the only variable in the predictor of the model.
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
    # Only non-empty categories are relevant.
    categories <- droplevels(data[[variable]])
    predictor_variables <- all.vars(model$predictor)
    other_variables <- predictor_variables[predictor_variables != variable]
    other_categories <- as.list(data[other_variables])

    # Calcuate unconditional values.
    ri_u <- calc_ri_by_var_unconditional(
      categories, fit$prop, weights, bias_factor
    )
    ri_se_u <- calc_ri_se_by_var_unconditional(
      variable, model$family, target, data, weights, design_var_func
    )

    # Calcuate conditional values.
    is_variable_in_model = (variable %in% predictor_variables)
    if (!is_variable_in_model) {
      # Conditional values are 0 for variables outside the model.
      ri_c <- 0
      ri_se_c <- 0
    } else if (length(other_categories) == 0) {
      # Conditional values require other model variables to condition on.
      ri_c <- NA
      ri_se_c <- NA
    } else {
      ri_c <- calc_ri_by_var_conditional(
        other_categories, fit$prop, weights, bias_factor
      )
      # Conditional standard error is approximated by the unconditional one.
      ri_se_c <- ri_se_u
    }

    # Combine results for the current variable.
    result_single_var <- data.frame(
      variable = variable,
      ri_u = ri_u,
      ri_se_u = ri_se_u,
      ri_c = ri_c,
      ri_se_c = ri_se_c
    )
    result <- rbind(result, result_single_var)
  }

  # Return result.
  result
}
