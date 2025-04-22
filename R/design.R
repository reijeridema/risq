# Module with functions that relate to the survey design.

# Build default inclusion weights. All weights are equal to `1`.
build_default_weights <- function(sample_cnt) {
  rep(1, sample_cnt)
}

# Build default strata factor. Uses a different stratum for each unique weight,
# up to `max_strata_cnt` strata. If there are more than `max_strata_cnt` unique
# weights, a single stratum is used.
build_default_strata <- function(weights, max_strata_cnt = 20) {
  unique_weights <- unique(weights)
  strata <- if (length(unique_weights) <= max_strata_cnt) {
    # Use a different stratum for each unique weight.
    match(weights, sort(unique_weights))
  } else {
    # Too many unique weights. Use a single stratum.
    rep(1, length(weights))
  }

  factor(strata)
}

# Build design object. Consists of the weights and strata, as well as a type.
# The type of the sample design is determined using as follows:
# - A single stratum and constant weights implies SI sampling.
# - Multiple strata and constant weights per stratum implies STSI sampling.
# - Non-constant weights per stratum implies PPS sampling.
build_design <- function(weights, strata) {
  # Determine if weights are constant based on range of weights per stratum.
  weight_range <- sapply(split(weights, strata), range)
  has_constant_weights <- all(weight_range[1,] == weight_range[2,])

  # Determine type of sampling design.
  if (has_constant_weights) {
    strata_cnt <- length(levels(strata))
    type <- if (strata_cnt == 1) "SI" else "STSI"
  } else {
    type <- "PPS"
  }

  # Return design object.
  list(
    type = type,
    weights = weights,
    strata = strata
  )
}

# Calculate the total weighted variance for `x`, within the given design.
calc_design_total_var <- function(x, design) {
  total_var_func <- list(
    SI = calc_total_var_stsi,
    STSI = calc_total_var_stsi,
    PPS = calc_total_var_pps
  )[[design$type]]
  total_var <- total_var_func(x, design$weights, design$strata)

  total_var
}

# Calculate the total weighted variance for design type SI or STSI.
calc_total_var_stsi <- function(x, weights, strata) {
  calc_stratum_var <- function(sample) {
    n <- nrow(sample)
    N <- sum(sample$weights)
    stratum_var <- N^2 * (1 - n / N) * stats::var(sample$x) / n
  }

  sample <- data.frame(x = x, weights = weights)
  strata_var <- sapply(split(sample, strata), calc_stratum_var)
  total_var <- sum(strata_var)

  total_var
}

# Calculate the total weighted variance for design type PPS.
# Note that strata is unused and that the calculation requires length(x) > 1.
calc_total_var_pps <- function(x, weights, strata) {
  n <- length(x)
  weighted_x <- x * weights
  total_var <- sum((n * weighted_x - sum(weighted_x))^2) / n / (n - 1)

  total_var
}
