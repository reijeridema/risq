# Module with functions for the core indicator calculations.

# Calculate bias factor for R-indicator.
calc_bias_factor <- function(prop, sigma, z, weights, total_var_func) {
  N <- sum(weights)

  lambda_1_func <- function(zi) {crossprod(zi, sigma) %*% zi}
  lambda_1 <- sum(apply(z * sqrt(weights), 1, lambda_1_func))
  lambda_2 <- total_var_func(prop) / N
  bias <- (lambda_1 - lambda_2) / N

  prop_var <- weighted_var(prop, weights)

  bias_factor <- max(1 - bias / prop_var, 0)

  bias_factor
}

# Calculate response rate.
calc_rr <- function(prop, weights) {
  response_rate <- weighted_mean(prop, weights)

  response_rate
}

# Calculate estimate for R-indicator.
calc_ri <- function(prop, weights, bias_factor) {
  prop_var <- weighted_var(prop, weights)
  ri <- 1 - 2 * sqrt(prop_var * bias_factor)

  ri
}

# Calculate standard error for R-indicator.
calc_ri_se <- function(prop, sigma, z, weights, total_var_func) {
  prop_mean <- weighted_mean(prop, weights)
  prop_var_ml <- weighted_var(prop, weights, method = "ML")
  prop_z <- cbind(prop, z)

  A <- stats::cov.wt(prop_z, wt = weights, method = "ML")$cov[-1, 1]
  B <- stats::cov.wt(z, wt = weights, method = "ML")$cov
  C <- total_var_func((prop - prop_mean)^2)

  variance <- numeric()
  variance[1] <- 4 * crossprod(A, sigma) %*% A
  variance[2] <- 2 * sum(diag(B %*% sigma %*% B %*% sigma))
  variance[3] <- C / sum(weights)^2
  variance <- sum(variance) / prop_var_ml

  ri_se <- sqrt(variance)

  ri_se
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

# Calculate estimate for the unconditional partial R-indicator.
# Returns a dataframe with rows in the order of levels(categories).
calc_ri_by_cat_unconditional <- function(categories, prop, weights) {
  wp <- data.frame(w = weights, wp = weights * prop)
  wp_by_cat <- stats::aggregate(wp, by = list(category = categories), FUN = sum)

  N <- sum(wp_by_cat$w)
  prop_mean <- sum(wp_by_cat$wp) / N;
  prop_deviation <- wp_by_cat$wp / wp_by_cat$w - prop_mean
  prop_var <- wp_by_cat$w * prop_deviation^2 / N
  ri <- sign(prop_deviation) * sqrt(prop_var)

  category_levels <- levels(categories)
  value_order <- match(category_levels, wp_by_cat$category)
  data.frame(category = category_levels, val = ri[value_order])
}

# Calculate standard error for the unconditional partial R-indicator.
# Returns a dataframe with rows in the order of levels(categories).
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
# Returns a dataframe with rows in the order of levels(categories).
calc_ri_by_cat_conditional <- function(
  categories, other_categories, prop, weights
) {
  prop_var_by_others <- weighted_vars_by_cat(prop, weights, other_categories)
  pv <- data.frame(prop_var = prop_var_by_others)
  pv_by_cat <- stats::aggregate(pv, by = list(category = categories), FUN = sum)
  ri <- sqrt(pv_by_cat$prop_var)

  category_levels <- levels(categories)
  value_order <- match(category_levels, pv_by_cat$category)
  data.frame(category = category_levels, val = ri[value_order])
}

# Calculate standard error for the conditional partial R-indicator.
# Returns a dataframe with rows in the order of levels(categories).
calc_ri_se_by_cat_conditional <- function(
  categories, other_categories, prop, sigma, z, weights, total_var_func
) {
  category_levels <- levels(categories)
  category_cnt <- length(category_levels)

  prop_mean_by_others <- weighted_means_by_cat(prop, weights, other_categories)
  prop_deviation <- prop - prop_mean_by_others
  pp <- prop_deviation * prop_deviation

  calc_z_mean_by_others <- function(zi) {
    weighted_means_by_cat(zi, weights, other_categories)
  }
  z_mean_by_others <- apply(z, 2, calc_z_mean_by_others)
  z_deviation <- z - z_mean_by_others
  zw <- z_deviation * weights

  N <- sum(weights)
  variance <- numeric(category_cnt)
  for (i in seq_len(category_cnt)) {
    is_cat <- (categories == category_levels[i])
    zw_cat <- zw[is_cat, , drop = FALSE]  # Only the rows where is_cat is TRUE.
    pp_cat <- pp * is_cat # Value pp where is_cat is TRUE, 0 elsewhere.

    A <- crossprod(prop_deviation[is_cat], zw_cat)
    B <- crossprod(z_deviation[is_cat, , drop = FALSE], zw_cat)

    v1 <- 4 * A %*% tcrossprod(sigma, A)
    v2 <- 2 * sum(diag(B %*% sigma %*% B %*% sigma))
    v3 <- total_var_func(pp_cat)

    variance[i] <- 0.25 * (v1 + v2 + v3) / (N * sum(pp_cat * weights))
  }
  ri_se = sqrt(variance)

  data.frame(category = category_levels, val = ri_se)
}

# Calculate estimate for coefficient of variation.
calc_cv <- function(prop, weights, bias_factor) {
  prop_mean <- weighted_mean(prop, weights)
  prop_var <- weighted_var(prop, weights)
	cv <- sqrt(prop_var * bias_factor) / prop_mean

  cv
}

# Calculate standard error for coefficient of variation.
calc_cv_se <- function(prop, sigma, z, weights, total_var_func, cv_val) {
  n <- length(weights)
  prop_mean <- weighted_mean(prop, weights)
  ri_se <- calc_ri_se(prop, sigma, z, weights, total_var_func)
  cv_var <- (0.5 * ri_se / prop_mean)^2 + cv_val^4 / n
	cv_se <- sqrt(cv_var)

  cv_se
}
