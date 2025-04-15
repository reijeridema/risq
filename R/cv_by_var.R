#' Coefficient of Variation by Variable
#'
#' @description
#' Calculate bias-adjusted partial coefficient of variation values by variable
#' for a selection of variables, given a `risq` object and target variable.
#'
#' @param robj `risq` object (see [`risq`][risq]).
#' @param target Name of the target variable. Must be the name of a `logical`
#'  variable in the `data` component of the `risq` object.
#' @param variables A `character` vector that specifies the variables for which
#'  to calculate partial coefficient of variation values. Must be the names of
#'  categorical variables in the `data` component of the `risq` object.
#'
#' @return
#' A `data.frame` with columns
#' - `variable`: name of the variable for which the row holds values,
#' - `cv_u`: estimate for the unconditional partial coefficient of variation for
#'  the variable,
#' - `cv_se_u`: standard error the for unconditional partial coefficient of
#'  variation for the variable,
#' - `cv_c`: estimate for the conditional partial coefficient of variation for
#'  the variable,
#' - `cv_se_c`: standard error for the conditional partial coefficient of
#'  variation for the variable.
#'
#' @export
#' @examples
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' cv_by_var_hlc <- cv_by_var(risq_hlc, "response", c("gender", "age", "job"))
cv_by_var <- function(robj, target, variables) {
  # Input validation.
  validate_risq_object(robj)
  validate_target(robj, target)
  validate_variables(robj, target, variables)

  model <- robj$model
  data <- robj$data
  weights <- robj$design$weights
  design_var_func <- function(x) {calc_design_total_var(x, robj$design)}

  # Fit model and calculate bias factor and response rate.
  fit <- fit_model(model, target, data, weights)
  bias_factor <- calc_bias_factor(
    fit$prop, fit$sigma, fit$z, weights, design_var_func
  )
  response_rate <- calc_rr(fit$prop, weights)

  # Build data frame with coefficient of variation values for each variable.
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
      cv_u = ri_u / response_rate,
      cv_se_u = ri_se_u / response_rate,
      cv_c = ri_c / response_rate,
      cv_se_c = ri_se_u / response_rate
    )
    result <- rbind(result, result_single_var)
  }

  # Return result.
  result
}
