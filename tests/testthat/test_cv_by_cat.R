source("ref/risq21.r")

data_1 <- data.frame(
  r = c(rep(FALSE, 5), rep(TRUE, 5)),
  s = c(rep(FALSE, 5), rep(TRUE, 5)),
  x = factor(rep(c("one", "two", "three"), length.out = 10)),
  y = c(1, 2, 4, 2, 2, 7, 6, 7, 8, 0),
  z = factor(rep(c("Yes", "No"), length.out = 10))
)

data_2 <- hlc[seq(1, nrow(hlc), 100), ]

get_ref <- function(
  variables, formula, family, data, weights = NULL, strata = NULL
) {
  # Build arguments for reference implementation.
  args <- list(
    formula = formula,
    sampleData = data,
    family = family,
    withPartials = TRUE,
    withPartialCV = TRUE
  )
  if (!is.null(weights)) {
    args$sampleWeights <- weights
  }
  if (!is.null(strata)) {
    args$sampleStrata <- strata
  }
  predictor <- formula[c(1, 3)]
  other_variables <- variables[!(variables %in% all.vars(predictor))]
  if (length(other_variables) > 0) {
    args$otherVariables <- other_variables
  }

  # Get values from reference implementation.
  ref <- do.call(what = getRIndicator, args = args)
  ref <- ref$partialCV$byCategories

  result <- NULL
  for (variable in variables) {
    # Build reference solution from relevant columns.
    ref_single_var <- ref[[variable]]
    result_single_var <- data.frame(
      variable = factor(variable, levels = variables),
      category = as.character(ref_single_var$category),
      cv_u = ref_single_var$CVuUnadj,
      cv_se_u = ref_single_var$CVuUnadjSE,
      cv_c = ref_single_var$CVcUnadj,
      cv_se_c = ref_single_var$CVcUnadjSE
    )

    # Ensure rows are in correct order.
    category_levels <- levels(data[[variable]])
    result_single_var <- result_single_var[
      match(category_levels, result_single_var$category), 
    ]
    row.names(result_single_var) <- NULL

    # Set unconditional values to 0 if variable is outside model.
    if (variable %in% other_variables) {
      result_single_var$cv_c <- 0
      result_single_var$cv_se_c <- 0
    }

    result <- rbind(result, result_single_var)
  }

  result
}

test_cv_vs_ref <- function(family) {
  # Using data_1.
  formula <- s ~ x + y
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- "x"
  robj <- risq(predictor, family, data_1)
  cv_tst <- cv_by_cat(robj, target, variables)
  cv_ref <- get_ref(variables, formula, family, data_1)
  expect_equal(cv_tst, cv_ref)

  # Using data_1 with single model variable.
  formula <- r ~ x
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- c("z", "x")
  robj <- risq(predictor, family, data_1)
  cv_tst <- cv_by_cat(robj, target, variables)
  # Reference solution cannot handle this case. Test NA pattern.
  expect_equal(which(is.na(cv_tst)), c(23, 24, 25, 28, 29, 30))

  # Using data_2 with default weights and strata (SI).
  formula <- response ~ gender + age + job
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- c("gender", "age", "marital_status")
  robj <- risq(predictor, family, data_2)
  expect_equal(robj$design$type, "SI")
  cv_tst <- cv_by_cat(robj, target, variables)
  cv_ref <- get_ref(variables, formula, family, data_2)
  expect_equal(cv_tst, cv_ref)

  # Using data_2 with custom weights and default strata (STSI).
  formula <- response ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:5, length.out = nrow(data_2))
  variables <- c("house_value", "age")
  robj <- risq(predictor, family, data_2, weights)
  expect_equal(robj$design$type, "STSI")
  cv_tst <- cv_by_cat(robj, target, variables)
  cv_ref <- suppressWarnings(
    get_ref(variables, formula, family, data_2, weights)
  )
  expect_equal(cv_tst, cv_ref)

  # Using data_2 with custom weights and custom strata (PPS).
  formula <- response ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:7, length.out = nrow(data_2))
  strata <- factor(rep(1:3, length.out = nrow(data_2)))
  variables <- c("urbanisation", "age", "household")
  robj <- risq(predictor, family, data_2, weights, strata)
  expect_equal(robj$design$type, "PPS")
  cv_tst <- cv_by_cat(robj, target, variables)
  cv_ref <- suppressWarnings(
    get_ref(variables, formula, family, data_2, weights, strata)
  )
  expect_equal(cv_tst, cv_ref)
}

test_that("cv_by_cat values equal reference solution (binomial)", {
  test_cv_vs_ref("binomial")
})

test_that("cv_by_cat values equal reference solution (gaussian)", {
  test_cv_vs_ref("gaussian")
})

test_that("cv_by_cat detects invalid input", {
  # Test invalid risq object.
  robj <- risq(~ x + y, "binomial", data_1)
  class(robj) <- "risk"
  expect_error(cv_by_cat(robj, "r", "x"), "`robj` must be a risq object")

  # Test invalid target argument.
  robj <- risq(~ x, "binomial", data_1)
  expect_error(cv_by_cat(robj, factor("r"), "x"), "`target` must be a string")
  expect_error(cv_by_cat(robj, "x", "x"), "`target` variable must not be in risq predictor")
  expect_error(cv_by_cat(robj, "Q", "x"), "`target` variable must be present in risq data")
  expect_error(cv_by_cat(robj, "y", "x"), "`target` variable must be logical in risq data")

  # Test missing values in target.
  data_missing_r <- data_1
  data_missing_r$r[8] <- NA
  robj <- risq(~ x + y, "gaussian", data_missing_r)
  expect_error(cv_by_cat(robj, "r"), "`target` variable must not contain `NA` values")

  # Test invalid variables argument.
  robj <- risq(~ x + y, "binomial", data_1)
  expect_error(cv_by_cat(robj, "r", 0), "`variables` must be a character vector")
  expect_error(cv_by_cat(robj, "r", c("x", "r")), "`variables` must not contain the target")
  expect_error(cv_by_cat(robj, "r", c("x", "Q")), "`variables` must be present in risq data")
  expect_error(cv_by_cat(robj, "r", c("x", "y")), "`variables` must be factors in risq data")
  expect_error(cv_by_cat(robj, "r", c("s", "x")), "`variables` must be factors in risq data")
  expect_error(cv_by_cat(robj, "r", c("x", "z", "z")), "`variables` must not contain duplicates")

  # Test missing values in variables.
  data_missing_z <- data_1
  data_missing_z$z[10] <- NA
  robj <- risq(~ x + y, "gaussian", data_missing_z)
  expect_error(cv_by_cat(robj, "r", c("z", "x")), "`variables` must not contain `NA` values")
})

test_that("cv_by_cat handles variables with empty levels", {
  # Build data with empty levels in variable x.
  data_2 <- data_1
  levels(data_2$x) <- c(levels(data_2$x), "four", "five")

  # Compare results with and without empty levels.
  robj_1 <- risq(~ x + y, "binomial", data_1)
  result_1 <- cv_by_cat(robj_1, "r", c("z", "x"))
  robj_2 <- risq(~ x + y, "binomial", data_2)
  result_2 <- cv_by_cat(robj_2, "r", c("z", "x"))
  expect_equal(result_1, result_2)
})
