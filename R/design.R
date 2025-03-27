# Build default inclusion weights. All weights are equal to `1`.
build_default_weights <- function(sample_cnt) {
  rep(1, sample_cnt)
}

# Build default strata factor. Uses a different stratum each unique weight, up
# to `max_strata_cnt` strata. If there are more than `max_strata_cnt` unique
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
