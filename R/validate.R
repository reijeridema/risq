# Module with shared input validation functions.

# Validate risq object.
validate_risq_object <- function(x, arg_name = deparse(substitute(x))) {
  if (!inherits(x, "risq")) {
    stop(paste0("`", arg_name, "` must be a risq object"))
  }
}

# Validate target variable for a given valid risq object.
validate_target <- function(x, target, arg_name = deparse(substitute(target))) {
  if (!is.character(target) || length(target) != 1) {
    stop(paste0("`", arg_name, "` must be a string"))
  }

  predictor_variables <- all.vars(x$model$predictor)
  is_target_in_predictor <- target %in% predictor_variables
  if (is_target_in_predictor) {
    stop(paste0("`", arg_name, "` must not be in risq predictor"))
  }

  is_target_in_data <- target %in% names(x$data)
  if (!is_target_in_data) {
    stop(paste0("`", arg_name, "` must be present in risq data"))
  }

  is_target_logical <- is.logical(x$data[[target]])
  if (!is_target_logical) {
    stop(paste0("`", arg_name, "` must be of type logical in risq data"))
  }

  is_target_complete <- !any(is.na(x$data[[target]]))
  if (!is_target_complete) {
    stop(paste0("`", arg_name, "` must not contain `NA` values in risq data"))
  }
}

# Validate non-target variables for a given valid risq object and target.
validate_variables <- function(
  x,
  target,
  variables,
  arg_name = deparse(substitute(variables))
) {
  if (!is.character(variables)) {
    stop(paste0("`", arg_name, "` must be a character vector"))
  }

  if (target %in% variables) {
    stop(paste0("`", arg_name, "` must not contain target variable"))
  }

  is_in_data <- variables %in% names(x$data)
  if (!all(is_in_data)) {
    stop(paste0("`", arg_name, "` must be present in risq data"))
  }

  is_factor <- sapply(x$data[variables], is.factor)
  if (!all(is_factor)) {
    stop(paste0("`", arg_name, "` must be of type factor in risq data"))
  }

  is_complete <- !any(is.na(x$data[variables]))
  if (!is_complete) {
    stop(paste0("`", arg_name, "` must not contain `NA` values in risq data"))
  }

  has_duplicates <- any(duplicated(variables))
  if (has_duplicates) {
    stop(paste0("`", arg_name, "` must not contain duplicates"))
  }
}

# Validate logical value.
validate_logical <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(paste0("`", arg_name, "` must be TRUE or FALSE"))
  }
}
