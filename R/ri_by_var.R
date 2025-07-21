#' Representativity Indicator by Variable
#'
#' @description
#' Estimate bias-adjusted unconditional or conditional partial representativity
#' indicator values by variable for a selection of categorical variables, given
#' a [`risq`][risq()] object and target variable.
#'
#' @param x A `risq` object.
#' @param target Name of the target variable. Must be the name of a logical
#'  variable in the `risq` object data.
#' @param variables A `character` vector that specifies the variables for which
#'  to estimate partial representativity indicator values. Must be the names of
#'  categorical variables in the `risq` object data. May include names of
#'  variables that are not part of the model.
#' @param type An optional string that specifies the type of partial
#'  representativity indicator to estimate. Must be either `"unconditional"` or
#'  `"conditional"`. Defaults to `"unconditional"`.
#' @param include_se An optional logical specifying whether to include the
#'  standard error or not. If omitted, the standard error is included.
#'
#' @return
#' A data frame with columns
#' - `variable`: name of the variable for which the row holds values,
#' - `value`: estimate for the partial representativity indicator,
#' - `se`: standard error for the partial representativity indicator (only if
#' `include_se` is TRUE).
#'
#' Note that conditional estimates are `NA` for a variable if that variable is
#' the only variable in the predictor of the `risq` object model.
#'
#' @seealso `risq` object constructor: [risq()]
#' @family representativity indicator functions
#'
#' @examples
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' ri_by_var(risq_hlc, "response_1", c("gender", "age", "job"))
#'
#' @export
ri_by_var <- function(
  x,
  target,
  variables,
  type = c("unconditional", "conditional"),
  include_se = TRUE
) {
  # Input validation.
  validate_risq_object(x)
  validate_target(x, target)
  validate_variables(x, target, variables)
  type <- match.arg(type)
  validate_logical(include_se)

  model <- x$model
  data <- x$data
  weights <- x$design$weights
  design_var_func <- function(y) {calc_design_total_var(y, x$design)}

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

    if (type == "unconditional") {
      # Calculate unconditional values.
      ri_value <- calc_ri_by_var_unconditional(
        categories, fit$prop, weights, bias_factor
      )
      if (include_se) {
        ri_se <- calc_ri_se_by_var_unconditional(
          variable, model$family, target, data, weights, design_var_func
        )
      }
    } else {
      # Calculate conditional values.
      is_variable_in_model = (variable %in% predictor_variables)
      if (!is_variable_in_model) {
        # Conditional values are 0 for variables outside the model.
        ri_value <- 0
        ri_se <- 0
      } else if (length(other_categories) == 0) {
        # Conditional values require other model variables to condition on.
        ri_value <- NA
        ri_se <- NA
      } else {
        ri_value <- calc_ri_by_var_conditional(
          other_categories, fit$prop, weights, bias_factor
        )
        if (include_se) {
          # Conditional standard error is approximated by the unconditional one.
          ri_se <- calc_ri_se_by_var_unconditional(
            variable, model$family, target, data, weights, design_var_func
          )
        }
      }
    }

    # Build result for the variable.
    result_single_var <- data.frame(
      variable = variable,
      value = ri_value
    )
    if (include_se) {
      result_single_var$se <- ri_se
    }

    # Combine with previous results.
    result <- rbind(result, result_single_var)
  }

  # Return result.
  result
}
