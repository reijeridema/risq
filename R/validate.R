# With with input validation functions.

# Validate risq object.
validate_risq_object <- function(robj) {
  if (!inherits(robj, "risq")) {
    stop("`robj` must be a risq object")
  }
}

# Validate target variable within the context of a valid risq object.
validate_target <- function(robj, target) {
  if (!is.character(target) || length(target) != 1) {
    stop("`target` must be a string")
  }

  predictor_variables <- all.vars(robj$model$predictor)
  is_target_in_predictor <- target %in% predictor_variables
  if (is_target_in_predictor) {
    stop("`target` variable must not be in risq predictor")
  }

  is_target_in_data <- target %in% names(robj$data)
  if (!is_target_in_data) {
    stop("`target` variable must be present in risq data")
  }

  is_target_logical <- is.logical(robj$data[[target]])
  if (!is_target_logical) {
    stop("`target` variable must be logical in risq data")
  }

  is_target_complete <- !any(is.na(robj$data[[target]]))
  if (!is_target_complete) {
    stop("`target` variable must not contain `NA` values")
  }
}

# Validate variables within the context of a valid risq object and target.
validate_variables <- function(robj, target, variables) {
  if (!is.character(variables)) {
    stop("`variables` must be a character vector")
  }

  if (target %in% variables) {
    stop("`variables` must not contain the target variable")
  }

  is_in_data <- variables %in% names(robj$data)
  if (!all(is_in_data)) {
    stop("`variables` must be present in risq data")
  }

  is_factor <- sapply(robj$data[variables], is.factor)
  if (!all(is_factor)) {
    stop("`variables` must be factors in risq data")
  }

  is_complete <- !any(is.na(robj$data[variables]))
  if (!is_complete) {
    stop("`variables` must not contain `NA` values in risq data")
  }

  has_duplicates <- any(duplicated(variables))
  if (has_duplicates) {
    stop("`variables` must not contain duplicates")
  }
}
