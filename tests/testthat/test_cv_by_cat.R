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

  result_u <- NULL
  result_c <- NULL
  for (variable in variables) {
    # Build reference solution from relevant columns.
    ref_var <- ref[[variable]]
    ref_u <- data.frame(
      variable = factor(variable, levels = variables),
      category = as.character(ref_var$category),
      value = ref_var$CVuUnadj,
      se = ref_var$CVuUnadjSE
    )
    ref_c <- data.frame(
      variable = factor(variable, levels = variables),
      category = as.character(ref_var$category),
      value = ref_var$CVcUnadj,
      se = ref_var$CVcUnadjSE
    )

    # Ensure rows are in correct order.
    category_levels <- levels(data[[variable]])
    ref_u <- ref_u[match(category_levels, ref_u$category), ]
    row.names(ref_u) <- NULL
    ref_c <- ref_c[match(category_levels, ref_c$category), ]
    row.names(ref_c) <- NULL

    # Set conditional values to 0 if variable is outside model.
    if (variable %in% other_variables) {
      ref_c[c("value", "se")] <- 0
    }

    result_u <- rbind(result_u, ref_u)
    result_c <- rbind(result_c, ref_c)
  }

  list("unconditional" = result_u, "conditional" = result_c)
}

test_cv_vs_ref <- function(family) {
  # Using data_1.
  formula <- s ~ x + y
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- "x"
  robj <- risq(predictor, family, data_1)
  cv_ref <- get_ref(variables, formula, family, data_1)
  cv_tst_u <- cv_by_cat(robj, target, variables, "unconditional")
  expect_equal(cv_tst_u, cv_ref$unconditional)
  cv_tst_u_no_se <- cv_by_cat(robj, target, variables, "unconditional", FALSE)
  expect_equal(cv_tst_u_no_se, cv_tst_u[-4])
  cv_tst_c <- cv_by_cat(robj, target, variables, "conditional")
  expect_equal(cv_tst_c, cv_ref$conditional)
  cv_tst_c_no_se <- cv_by_cat(robj, target, variables, "conditional", FALSE)
  expect_equal(cv_tst_c_no_se, cv_tst_c[-4])

  # Using data_1 with single model variable.
  formula <- r ~ x
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- c("z", "x")
  robj <- risq(predictor, family, data_1)
  cv_tst_u <- cv_by_cat(robj, target, variables, "unconditional")
  cv_tst_c <- cv_by_cat(robj, target, variables, "conditional")
  # Reference solution cannot handle this case. Test NA pattern.
  expect_equal(which(is.na(cv_tst_u)), integer(0))
  expect_equal(which(is.na(cv_tst_c)), c(13, 14, 15, 18, 19, 20))

  # Using data_2 with default weights and strata (SI).
  formula <- response_1 ~ gender + age + job
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- c("gender", "age", "marital_status")
  robj <- risq(predictor, family, data_2)
  expect_equal(robj$design$type, "SI")
  cv_ref <- get_ref(variables, formula, family, data_2)
  cv_tst_u <- cv_by_cat(robj, target, variables, "unconditional")
  expect_equal(cv_tst_u, cv_ref$unconditional)
  cv_tst_u_no_se <- cv_by_cat(robj, target, variables, "unconditional", FALSE)
  expect_equal(cv_tst_u_no_se, cv_tst_u[-4])
  cv_tst_c <- cv_by_cat(robj, target, variables, "conditional")
  expect_equal(cv_tst_c, cv_ref$conditional)
  cv_tst_c_no_se <- cv_by_cat(robj, target, variables, "conditional", FALSE)
  expect_equal(cv_tst_c_no_se, cv_tst_c[-4])

  # Using data_2 with custom weights and default strata (STSI).
  formula <- response_2 ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:5, length.out = nrow(data_2))
  variables <- c("house_value", "age")
  robj <- risq(predictor, family, data_2, weights)
  expect_equal(robj$design$type, "STSI")
  cv_ref <- suppressWarnings(
    get_ref(variables, formula, family, data_2, weights)
  )
  cv_tst_u <- cv_by_cat(robj, target, variables, "unconditional")
  expect_equal(cv_tst_u, cv_ref$unconditional)
  cv_tst_u_no_se <- cv_by_cat(robj, target, variables, "unconditional", FALSE)
  expect_equal(cv_tst_u_no_se, cv_tst_u[-4])
  cv_tst_c <- cv_by_cat(robj, target, variables, "conditional")
  expect_equal(cv_tst_c, cv_ref$conditional)
  cv_tst_c_no_se <- cv_by_cat(robj, target, variables, "conditional", FALSE)
  expect_equal(cv_tst_c_no_se, cv_tst_c[-4])

  # Using data_2 with custom weights and custom strata (PPS).
  formula <- response_3 ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:7, length.out = nrow(data_2))
  strata <- factor(rep(1:3, length.out = nrow(data_2)))
  variables <- c("urbanisation", "age", "household")
  robj <- risq(predictor, family, data_2, weights, strata)
  expect_equal(robj$design$type, "PPS")
  cv_ref <- suppressWarnings(
    get_ref(variables, formula, family, data_2, weights, strata)
  )
  cv_tst_u <- cv_by_cat(robj, target, variables, "unconditional")
  expect_equal(cv_tst_u, cv_ref$unconditional)
  cv_tst_u_no_se <- cv_by_cat(robj, target, variables, "unconditional", FALSE)
  expect_equal(cv_tst_u_no_se, cv_tst_u[-4])
  cv_tst_c <- cv_by_cat(robj, target, variables, "conditional")
  expect_equal(cv_tst_c, cv_ref$conditional)
  cv_tst_c_no_se <- cv_by_cat(robj, target, variables, "conditional", FALSE)
  expect_equal(cv_tst_c_no_se, cv_tst_c[-4])
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
  expect_error(cv_by_cat(robj, "r", "x"), "`x` must be a risq object")

  # Test invalid target argument.
  robj <- risq(~ x, "binomial", data_1)
  expect_error(cv_by_cat(robj, factor("r"), "x"), "`target` must be a string")
  expect_error(cv_by_cat(robj, "x", "x"), "`target` must not be in risq predictor")
  expect_error(cv_by_cat(robj, "Q", "x"), "`target` must be present in risq data")
  expect_error(cv_by_cat(robj, "y", "x"), "`target` must be of type logical in risq data")

  # Test missing values in target.
  data_missing_r <- data_1
  data_missing_r$r[8] <- NA
  robj <- risq(~ x + y, "gaussian", data_missing_r)
  expect_error(cv_by_cat(robj, "r"), "`target` must not contain `NA` values in risq data")

  # Test invalid variables argument.
  robj <- risq(~ x + y, "binomial", data_1)
  expect_error(cv_by_cat(robj, "r", 0), "`variables` must be a character vector")
  expect_error(cv_by_cat(robj, "r", c("x", "r")), "`variables` must not contain target variable")
  expect_error(cv_by_cat(robj, "r", c("x", "Q")), "`variables` must be present in risq data")
  expect_error(cv_by_cat(robj, "r", c("x", "y")), "`variables` must be of type factor in risq data")
  expect_error(cv_by_cat(robj, "r", c("s", "x")), "`variables` must be of type factor in risq data")
  expect_error(cv_by_cat(robj, "r", c("x", "z", "z")), "`variables` must not contain duplicates")

  # Test missing values in variables.
  data_missing_z <- data_1
  data_missing_z$z[10] <- NA
  robj <- risq(~ x + y, "gaussian", data_missing_z)
  expect_error(cv_by_cat(robj, "r", c("z", "x")), "`variables` must not contain `NA` values in risq data")

  # Test invalid type argument.
  robj <- risq(~ x + y, "gaussian", data_1)
  expect_error(cv_by_cat(robj, "r", "x", type = NA), "character vector")

  # Test invalid include_se argument.
  robj <- risq(~ x + y, "gaussian", data_1)
  expect_error(cv_by_cat(robj, "r", "x", include_se = NA), "`include_se` must be TRUE or FALSE")
})

test_that("cv_by_cat handles variables with empty levels", {
  # Build data with and without empty levels.
  data_a <- data_1
  data_a$x <- factor(
    rep(c("one", "two", "three"), length.out = 10),
    levels = c("one", "two", "three")
  )
  data_b <- data_1
  data_b$x <- factor(
    rep(c("one", "two", "three"), length.out = 10),
    levels = c("zero", "one", "two", "two_and_half", "three", "four")
  )

  # Compare results with and without empty levels.
  variables <- c("z", "x")
  robj_a <- risq(~ x + y, "binomial", data_a)
  result_a <- cv_by_cat(robj_a, "r", variables)
  robj_b <- risq(~ x + y, "binomial", data_b)
  result_b <- cv_by_cat(robj_b, "r", variables)
  is_nonempty_cat <- (
    result_b$variable != "x" | result_b$category %in% levels(data_a$x)
  )
  result_b_nonempty <- result_b[is_nonempty_cat, ]
  rownames(result_b_nonempty) <- NULL
  expect_equal(result_b_nonempty, result_a)
  result_b_empty <- result_b[!is_nonempty_cat, ]
  rownames(result_b_empty) <- NULL
  expected_empty <- data.frame(
    variable = factor("x", levels = variables),
    category = c("zero", "two_and_half", "four"),
    value = as.numeric(NA),
    se = as.numeric(NA)
  )
  expect_equal(result_b_empty, expected_empty)
})
