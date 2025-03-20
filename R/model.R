# Build model for fiting survey data.
build_model <- function(predictor, family) {
  list(predictor = predictor, family = family)
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
