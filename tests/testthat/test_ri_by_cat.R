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
    withPartialCV = FALSE
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
  ref <- ref$partialR$byCategories

  result <- NULL
  for (variable in variables) {
    # Build reference solution from relevant columns.
    ref_single_var <- ref[[variable]]
    result_single_var <- data.frame(
      variable = factor(variable, levels = variables),
      category = as.character(ref_single_var$category),
      ri_u = ref_single_var$PuUnadj,
      ri_se_u = ref_single_var$PuUnadjSE,
      ri_c = ref_single_var$PcUnadj,
      ri_se_c = ref_single_var$PcUnadjSE
    )

    # Ensure rows are in correct order.
    category_levels <- levels(data[[variable]])
    result_single_var <- result_single_var[
      match(category_levels, result_single_var$category), 
    ]
    row.names(result_single_var) <- NULL

    # Set unconditional values to 0 if variable is outside model.
    if (variable %in% other_variables) {
      result_single_var$ri_c <- 0
      result_single_var$ri_se_c <- 0
    }

    result <- rbind(result, result_single_var)
  }

  result
}

test_ri_vs_ref <- function(family) {
  # Using data_1.
  formula <- s ~ x + y
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- "x"
  robj <- risq(predictor, family, data_1)
  ri_tst <- ri_by_cat(robj, target, variables)
  ri_ref <- get_ref(variables, formula, family, data_1)
  expect_equal(ri_tst, ri_ref)

  # Using data_2 with default weights and strata (SI).
  formula <- response ~ gender + age + job
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- c("gender", "age", "urbanisation")
  robj <- risq(predictor, family, data_2)
  expect_equal(robj$design$type, "SI")
  ri_tst <- ri_by_cat(robj, target, variables)
  ri_ref <- get_ref(variables, formula, family, data_2)
  expect_equal(ri_tst, ri_ref)

  # Using data_2 with custom weights and default strata (STSI).
  formula <- response ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:5, length.out = nrow(data_2))
  variables <- c("house_value", "age")
  robj <- risq(predictor, family, data_2, weights)
  expect_equal(robj$design$type, "STSI")
  ri_tst <- ri_by_cat(robj, target, variables)
  ri_ref <- suppressWarnings(
    get_ref(variables, formula, family, data_2, weights)
  )
  expect_equal(ri_tst, ri_ref)

  # Using data_2 with custom weights and custom strata (PPS).
  formula <- response ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:7, length.out = nrow(data_2))
  strata <- factor(rep(1:3, length.out = nrow(data_2)))
  variables <- c("household", "age", "marital_status")
  robj <- risq(predictor, family, data_2, weights, strata)
  expect_equal(robj$design$type, "PPS")
  ri_tst <- ri_by_cat(robj, target, variables)
  ri_ref <- suppressWarnings(
    get_ref(variables, formula, family, data_2, weights, strata)
  )
  expect_equal(ri_tst, ri_ref)
}

test_that("ri_by_cat values equal reference solution (binomial)", {
  test_ri_vs_ref("binomial")
})

test_that("ri_by_cat values equal reference solution (gaussian)", {
  test_ri_vs_ref("gaussian")
})

test_that("ri_by_cat detects invalid input", {
  # Test invalid risq object.
  robj <- risq(~ x + y, "binomial", data_1)
  class(robj) <- "risk"
  expect_error(ri_by_cat(robj, "r", "x"), "must be a risq object")

  # Test invalid target argument.
  robj <- risq(~ x, "binomial", data_1)
  expect_error(ri_by_cat(robj, factor("r"), "x"), "must be a string")
  expect_error(ri_by_cat(robj, "x", "x"), "must not be in risq predictor")
  expect_error(ri_by_cat(robj, "Q", "x"), "must be present in risq data")
  expect_error(ri_by_cat(robj, "y", "x"), "must be logical in risq data")

  # Test invalid variables argument.
  robj <- risq(~ x + y, "binomial", data_1)
  expect_error(ri_by_cat(robj, "r", 0), "must be a character vector")
  expect_error(ri_by_cat(robj, "r", c("x", "r")), "must not contain the target")
  expect_error(ri_by_cat(robj, "r", c("x", "Q")), "must be present in risq data")
  expect_error(ri_by_cat(robj, "r", c("x", "y")), "must be factors in risq data")
  expect_error(ri_by_cat(robj, "r", c("s", "x")), "must be factors in risq data")
  expect_error(ri_by_var(robj, "r", c("x", "z", "x")), "must not contain duplicates")
})
