#' Representativity Indicator by Category
#'
#' @description
#' Calculate unconditional and conditional partial representativity indicator
#' values by category for a selection of variables, given a `risq` object and
#' target variable.
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
#' - `category`: category within the variable for which the row holds values,
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
#' ri_by_cat_hlc <- ri_by_cat(risq_hlc, "response", c("gender", "age", "job"))
ri_by_cat <- function(robj, target, variables) {
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
  prop <- fit$prop
  sigma <- fit$sigma
  z <- fit$z
  bias_factor <- calc_bias_factor(prop, sigma, z, weights, design_var_func)

  # Build data frame with representativity indicator values for each category.
  result <- NULL
  for (variable in variables) {
    categories <- data[[variable]]
    predictor_variables <- all.vars(model$predictor)
    other_variables <- predictor_variables[predictor_variables != variable]
    other_categories <- as.list(data[other_variables])

    ri_u <- calc_ri_by_cat_unconditional(
      categories, prop, weights
    )
    ri_se_u <- calc_ri_se_by_cat_unconditional(
      categories, prop, weights, design_var_func
    )
    ri_c <- calc_ri_by_cat_conditional(
      categories, other_categories, prop, weights
    )
    ri_se_c <- calc_ri_se_by_cat_conditional(
      categories, other_categories, prop, sigma, z, weights, design_var_func
    )

    # Ensure consistent ordering of rows.
    category_levels <- levels(categories)
    sorted_ri_u <- ri_u[match(category_levels, ri_u$category), ]
    sorted_ri_se_u <- ri_se_u[match(category_levels, ri_se_u$category), ]
    sorted_ri_c <- ri_c[match(category_levels, ri_c$category), ]
    sorted_ri_se_c <- ri_se_c[match(category_levels, ri_se_c$category), ]

    # Combine results for the current variable.
    result_single_var <- data.frame(
      variable = factor(variable, levels = variables),
      category = category_levels,
      ri_u = sorted_ri_u$val,
      ri_se_u = sorted_ri_se_u$val,
      ri_c = sorted_ri_c$val,
      ri_se_c = sorted_ri_se_c$val
    )

    result <- rbind(result, result_single_var)
  }

  # Return result.
  result
}

# Calculate estimate for the unconditional partial R-indicator.
calc_ri_by_cat_unconditional <- function(categories, prop, weights) {
  wp <- data.frame(w = weights, wp = weights * prop)
  wp_by_cat <- stats::aggregate(wp, by = list(category = categories), FUN = sum)

  N <- sum(wp_by_cat$w)
  prop_mean <- sum(wp_by_cat$wp) / N;
  prop_deviation <- wp_by_cat$wp / wp_by_cat$w - prop_mean
  prop_var <- wp_by_cat$w * prop_deviation^2 / N
  ri <- sign(prop_deviation) * sqrt(prop_var)

  data.frame(category = wp_by_cat$category, val = ri)
}

# Calculate standard error for the unconditional partial R-indicator.
calc_ri_se_by_cat_unconditional <- function(
  categories, prop, weights, total_var_func
) {
  category_levels <- levels(categories)
  category_cnt <- length(category_levels)

  n <- numeric(category_cnt)
  v1 <- numeric(category_cnt)
  v2 <- numeric(category_cnt)
  for (i in seq_len(category_cnt)) {
    delta <- ifelse(categories == category_levels[i], 1, 0)
    n[i] <- sum(delta * weights)
    v1[i] <- total_var_func(delta * prop) 
    v2[i] <- total_var_func((1 - delta) * prop)
  }

  N <- sum(weights)
  ri_se <- sqrt(n / N * (v1 * (1 / n - 1 / N)^2 + v2 * (1 / N)^2))

  data.frame(category = category_levels, val = ri_se)
}

# Calculate estimate for the conditional partial R-indicator.
calc_ri_by_cat_conditional <- function(
  categories, other_categories, prop, weights
) {
  prop_var_by_others <- weighted_vars_by_cat(prop, weights, other_categories)
  pv <- data.frame(prop_var = prop_var_by_others)
  pv_by_cat <- stats::aggregate(pv, by = list(category = categories), FUN = sum)
  ri <- sqrt(pv_by_cat$prop_var)

  data.frame(category = pv_by_cat$category, val = ri)
}

# Calculate standard error for the conditional partial R-indicator.
calc_ri_se_by_cat_conditional <- function(
  categories, other_categories, prop, sigma, z, weights, total_var_func
) {
  category_levels <- levels(categories)
  category_cnt <- length(category_levels)

  prop_mean_by_others <- weighted_means_by_cat(prop, weights, other_categories)
  prop_deviation <- prop - prop_mean_by_others

  calc_z_mean_by_others <- function(zi) {
    weighted_means_by_cat(zi, weights, other_categories)
  }
  z_mean_by_others <- apply(z, 2, calc_z_mean_by_others)
  z_deviation <- z - z_mean_by_others

  N <- sum(weights)
  variance <- numeric(category_cnt)
  for (i in seq_len(category_cnt)) {
    delta <- ifelse(categories == category_levels[i], 1, 0)
    zdw <- z_deviation * delta * weights
    ppd <- prop_deviation * prop_deviation * delta

    A <- matrix(prop_deviation, nrow = 1) %*% zdw
    B <- t(z_deviation) %*% zdw

    v1 <- 4 * A %*% sigma %*% t(A)
    v2 <- 2 * sum(diag(B %*% sigma %*% B %*% sigma))
    v3 <- total_var_func(ppd)

    variance[i] <- 0.25 * (v1 + v2 + v3) / (N * sum(ppd * weights))
  }
  ri_se = sqrt(variance)

  data.frame(category = category_levels, val = ri_se)
}
