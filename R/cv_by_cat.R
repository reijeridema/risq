#' Coefficient of Variation by Category
#'
#' @description
#' Estimate unconditional and conditional partial coefficient of variation
#' values by category for a selection of categorical variables, given a
#' [`risq`][risq()] object and target variable.
#'
#' @param x A `risq` object.
#' @param target Name of the target variable. Must be the name of a logical
#'  variable in the `risq` object data.
#' @param variables A `character` vector that specifies the variables for which
#'  to estimate partial coefficient of variation values. Must be the names of
#'  categorical variables in the `risq` object data. May include names of
#'  variables that are not part of the model.
#' @param type An optional string that specifies the type of partial
#'  coefficient of variation to estimate. Must be either `"unconditional"` or
#'  `"conditional"`. Defaults to `"unconditional"`.
#' @param include_se An optional logical specifying whether to include the
#'  standard error or not. If omitted, the standard error is included.
#'
#' @return
#' A data frame with columns
#' - `variable`: name of the variable for which the row holds values,
#' - `category`: category within the variable for which the row holds values,
#' - `value`: estimate for the partial coefficient of variation,
#' - `se`: standard error for the partial coefficient of variation (only if
#' `include_se` is TRUE).
#'
#' Note that estimates are `NA` for empty categories and that conditional
#' estimates are `NA` for a variable if that variable is the only variable in
#' the predictor of the `risq` object model.
#'
#' @seealso `risq` object constructor: [risq()]
#' @family coefficient of variation functions
#'
#' @examples
#' risq_hlc <- risq(predictor = ~ gender + age, data = hlc)
#' cv_by_cat_hlc <- cv_by_cat(risq_hlc, "response", c("gender", "age", "job"))
#'
#' @export
cv_by_cat <- function(x, target, variables, type = c("unconditional", "conditional"), include_se = TRUE) {
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

  # Fit model and calculate response rate.
  fit <- fit_model(model, target, data, weights)
  response_rate <- calc_rr(fit$prop, weights)

  # Build data frame with coefficient of variation values for each category.
  result <- NULL
  for (variable in variables) {
    # Calculate values for non-empty categories.
    categories <- droplevels(data[[variable]])
    category_levels <- levels(categories)

    predictor_variables <- all.vars(model$predictor)
    other_variables <- predictor_variables[predictor_variables != variable]
    other_categories <- as.list(data[other_variables])

    if (type == "unconditional") {
      # Calculate unconditional values.
      ri_value <- calc_ri_by_cat_unconditional(
        categories, fit$prop, weights
      )
      if (include_se) {
        ri_se <- calc_ri_se_by_cat_unconditional(
          categories, fit$prop, weights, design_var_func
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
        ri_value <- calc_ri_by_cat_conditional(
          categories, other_categories, fit$prop, weights
        )
        if (include_se) {
          ri_se <- calc_ri_se_by_cat_conditional(
            categories,
            other_categories,
            fit$prop,
            fit$sigma,
            fit$z,
            weights,
            design_var_func
          )
        }
      }
    }

    # Build result for current variable. Include rows for empty levels.
    all_category_levels <- levels(data[[variable]])
    is_nonempty_level <- (all_category_levels %in% category_levels)
    result_single_var <- data.frame(
      variable = factor(variable, levels = variables),
      category = all_category_levels,
      value = NA
    )
    result_single_var$value[is_nonempty_level] = ri_value / response_rate
    if (include_se) {
      result_single_var$se <- NA
      result_single_var$se[is_nonempty_level] <- ri_se / response_rate
    }

    # Combine with previous results.
    result <- rbind(result, result_single_var)
  }

  # Return result.
  result
}
