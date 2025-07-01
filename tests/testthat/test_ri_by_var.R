source("ref/risq21.r")

data_1 <- data.frame(
  r = c(rep(FALSE, 5), rep(TRUE, 5)),
  s = c(rep(FALSE, 5), rep(TRUE, 5)),
  x = factor(rep(c("one", "two", "three"), length.out = 10)),
  y = c(1, 2, 4, 2, 2, 7, 6, 7, 8, 0),
  z = factor(c(1, 2, 4, 2, 2, 7, 6, 7, 8, 0))
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
  ref <- ref$partialR$byVariables

  # Build unconditional and conditional reference data frames.
  ref_u <- data.frame(
    variable = ref$variable,
    value = ref$Pu,
    se = ref$PuSE
  )
  ref_c <- data.frame(
    variable = ref$variable,
    value = ref$Pc,
    se = ref$PcSEApprox
  )

  # Select relevent rows in correct order.
  ref_u <- ref_u[match(variables, ref$variable), ]
  row.names(ref_u) <- NULL
  ref_c <- ref_c[match(variables, ref$variable), ]
  row.names(ref_c) <- NULL

  # Set conditional values to 0 for variables outside model.
  if (length(other_variables) > 0) {
    ref_c[ref_c$variable %in% other_variables, c("value", "se")] <- 0
  }

  list("unconditional" = ref_u, "conditional" = ref_c)
}

test_ri_vs_ref <- function(family) {
  # Using data_1.
  formula <- r ~ x + y
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- "x"
  robj <- risq(predictor, family, data_1)
  ri_ref <- get_ref(variables, formula, family, data_1)
  ri_tst_u <- ri_by_var(robj, target, variables, "unconditional")
  expect_equal(ri_tst_u, ri_ref$unconditional)
  ri_tst_u_no_se <- ri_by_var(robj, target, variables, "unconditional", FALSE)
  expect_equal(ri_tst_u_no_se, ri_tst_u[-3])
  ri_tst_c <- ri_by_var(robj, target, variables, "conditional")
  expect_equal(ri_tst_c, ri_ref$conditional)
  ri_tst_c_no_se <- ri_by_var(robj, target, variables, "conditional", FALSE)
  expect_equal(ri_tst_c_no_se, ri_tst_c[-3])

  # Using data_1 with single model variable.
  formula <- r ~ x
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- c("x", "z")
  robj <- risq(predictor, family, data_1)
  ri_tst_u <- ri_by_var(robj, target, variables, "unconditional")
  ri_tst_c <- ri_by_var(robj, target, variables, "conditional")
  # Reference solution cannot handle this case. Test NA pattern.
  expect_equal(which(is.na(ri_tst_u)), integer(0))
  expect_equal(which(is.na(ri_tst_c)), c(3, 5))

  # Using data_2 with default weights and strata (SI).
  formula <- response ~ gender + age + job
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- c("gender", "age", "urbanisation")
  robj <- risq(predictor, family, data_2)
  expect_equal(robj$design$type, "SI")
  ri_ref <- get_ref(variables, formula, family, data_2)
  ri_tst_u <- ri_by_var(robj, target, variables, "unconditional")
  expect_equal(ri_tst_u, ri_ref$unconditional)
  ri_tst_u_no_se <- ri_by_var(robj, target, variables, "unconditional", FALSE)
  expect_equal(ri_tst_u_no_se, ri_tst_u[-3])
  ri_tst_c <- ri_by_var(robj, target, variables, "conditional")
  expect_equal(ri_tst_c, ri_ref$conditional)
  ri_tst_c_no_se <- ri_by_var(robj, target, variables, "conditional", FALSE)
  expect_equal(ri_tst_c_no_se, ri_tst_c[-3])

  # Using data_2 with custom weights and default strata (STSI).
  formula <- response ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:5, length.out = nrow(data_2))
  variables <- c("house_value", "age")
  robj <- risq(predictor, family, data_2, weights)
  expect_equal(robj$design$type, "STSI")
  ri_ref <- suppressWarnings(
    get_ref(variables, formula, family, data_2, weights)
  )
  ri_tst_u <- ri_by_var(robj, target, variables, "unconditional")
  expect_equal(ri_tst_u, ri_ref$unconditional)
  ri_tst_u_no_se <- ri_by_var(robj, target, variables, "unconditional", FALSE)
  expect_equal(ri_tst_u_no_se, ri_tst_u[-3])
  ri_tst_c <- ri_by_var(robj, target, variables, "conditional")
  expect_equal(ri_tst_c, ri_ref$conditional)
  ri_tst_c_no_se <- ri_by_var(robj, target, variables, "conditional", FALSE)
  expect_equal(ri_tst_c_no_se, ri_tst_c[-3])

  # Using data_2 with custom weights and custom strata (PPS).
  formula <- response ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:7, length.out = nrow(data_2))
  strata <- factor(rep(1:3, length.out = nrow(data_2)))
  variables <- c("house_value", "age", "job")
  robj <- risq(predictor, family, data_2, weights, strata)
  expect_equal(robj$design$type, "PPS")
  ri_ref <- suppressWarnings(
    get_ref(variables, formula, family, data_2, weights, strata)
  )
  ri_tst_u <- ri_by_var(robj, target, variables, "unconditional")
  expect_equal(ri_tst_u, ri_ref$unconditional)
  ri_tst_u_no_se <- ri_by_var(robj, target, variables, "unconditional", FALSE)
  expect_equal(ri_tst_u_no_se, ri_tst_u[-3])
  ri_tst_c <- ri_by_var(robj, target, variables, "conditional")
  expect_equal(ri_tst_c, ri_ref$conditional)
  ri_tst_c_no_se <- ri_by_var(robj, target, variables, "conditional", FALSE)
  expect_equal(ri_tst_c_no_se, ri_tst_c[-3])
}

test_that("ri_by_var values equal reference solution (binomial)", {
  test_ri_vs_ref("binomial")
})

test_that("ri_by_var values equal reference solution (gaussian)", {
  test_ri_vs_ref("gaussian")
})

test_that("ri_by_var detects invalid input", {
  # Test invalid risq object.
  robj <- risq(~ x + y, "gaussian", data_1)
  class(robj) <- "risque"
  expect_error(ri_by_var(robj, "r", "x"), "`x` must be a risq object")

  # Test invalid target argument.
  robj <- risq(~ x, "gaussian", data_1)
  expect_error(ri_by_var(robj, c("r", "s"), "x"), "`target` must be a string")
  expect_error(ri_by_var(robj, "x", "x"), "`target` must not be in risq predictor")
  expect_error(ri_by_var(robj, "X", "x"), "`target` must be present in risq data")
  expect_error(ri_by_var(robj, "y", "x"), "`target` must be of type logical in risq data")

  # Test missing values in target.
  data_missing_r <- data_1
  data_missing_r$r[7] <- NA
  robj <- risq(~ x + y, "binomial", data_missing_r)
  expect_error(ri_by_var(robj, "r"), "`target` must not contain `NA` values in risq data")

  # Test invalid variables argument.
  robj <- risq(~ x + y, "gaussian", data_1)
  expect_error(ri_by_var(robj, "r", 0), "`variables` must be a character vector")
  expect_error(ri_by_var(robj, "r", c("x", "r")), "`variables` must not contain target variable")
  expect_error(ri_by_var(robj, "r", c("x", "q")), "`variables` must be present in risq data")
  expect_error(ri_by_var(robj, "r", c("x", "y")), "`variables` must be of type factor in risq data")
  expect_error(ri_by_var(robj, "r", c("s", "x")), "`variables` must be of type factor in risq data")
  expect_error(ri_by_var(robj, "r", c("x", "x")), "`variables` must not contain duplicates")

  # Test missing values in variables.
  data_missing_z <- data_1
  data_missing_z$z[5] <- NA
  robj <- risq(~ x + y, "binomial", data_missing_z)
  expect_error(ri_by_var(robj, "r", c("x", "z")), "`variables` must not contain `NA` values in risq data")

  # Test invalid type argument.
  robj <- risq(~ x + y, "binomial", data_1)
  expect_error(ri_by_var(robj, "r", "x", type = "nonconditional"), "should be one of")

  # Test invalid include_se argument.
  robj <- risq(~ x + y, "binomial", data_1)
  expect_error(ri_by_var(robj, "r", "x", include_se = "FALSE"), "`include_se` must be TRUE or FALSE")
})

test_that("ri_by_var handles variables with empty levels", {
  # Build data with empty levels in variable x.
  data_2 <- data_1
  levels(data_2$x) <- c(levels(data_2$x), "four", "five")

  # Compare results with and without empty levels.
  robj_1 <- risq(~ x + y, "binomial", data_1)
  result_1 <- ri_by_var(robj_1, "r", c("x", "z"))
  robj_2 <- risq(~ x + y, "binomial", data_2)
  result_2 <- ri_by_var(robj_2, "r", c("x", "z"))
  expect_equal(result_1, result_2)
})
