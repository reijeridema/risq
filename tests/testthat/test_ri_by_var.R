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

  # Build reference solution from relevant columns.
  ref <- data.frame(
    variable = ref$variable,
    ri_u = ref$Pu,
    ri_se_u = ref$PuSE,
    ri_c = ref$Pc,
    ri_se_c = ref$PcSEApprox
  )

  # Select relevent rows in correct order.
  ref <- ref[match(variables, ref$variable), ]
  row.names(ref) <- NULL

  # Set unconditional values to 0 for variables outside model.
  if (length(other_variables) > 0) {
    ref[ref$variable %in% other_variables, c("ri_c", "ri_se_c")] <- 0
  }

  ref
}

test_ri_vs_ref <- function(family) {
  # Using data_1.
  formula <- r ~ x + y
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- "x"
  robj <- risq(predictor, family, data_1)
  ri_tst <- ri_by_var(robj, target, variables)
  ri_ref <- get_ref(variables, formula, family, data_1)
  expect_equal(ri_tst, ri_ref)

  # Using data_1 with single model variable.
  formula <- r ~ x
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- c("x", "z")
  robj <- risq(predictor, family, data_1)
  ri_tst <- ri_by_var(robj, target, variables)
  # Reference solution cannot handle this case. Test NA pattern.
  expect_equal(which(is.na(ri_tst)), c(7, 9))

  # Using data_2 with default weights and strata (SI).
  formula <- response ~ gender + age + job
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  variables <- c("gender", "age", "urbanisation")
  robj <- risq(predictor, family, data_2)
  expect_equal(robj$design$type, "SI")
  ri_tst <- ri_by_var(robj, target, variables)
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
  ri_tst <- ri_by_var(robj, target, variables)
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
  variables <- c("house_value", "age", "job")
  robj <- risq(predictor, family, data_2, weights, strata)
  expect_equal(robj$design$type, "PPS")
  ri_tst <- ri_by_var(robj, target, variables)
  ri_ref <- suppressWarnings(
    get_ref(variables, formula, family, data_2, weights, strata)
  )
  expect_equal(ri_tst, ri_ref)
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
  expect_error(ri_by_var(robj, "r", "x"), "`robj` must be a risq object")

  # Test invalid target argument.
  robj <- risq(~ x, "gaussian", data_1)
  expect_error(ri_by_var(robj, c("r", "s"), "x"), "`target` must be a string")
  expect_error(ri_by_var(robj, "x", "x"), "`target` variable must not be in risq predictor")
  expect_error(ri_by_var(robj, "X", "x"), "`target` variable must be present in risq data")
  expect_error(ri_by_var(robj, "y", "x"), "`target` variable must be logical in risq data")

  # Test missing values in target.
  data_missing_r <- data_1
  data_missing_r$r[7] <- NA
  robj <- risq(~ x + y, "binomial", data_missing_r)
  expect_error(ri_by_var(robj, "r"), "`target` variable must not contain `NA` values")

  # Test invalid variables argument.
  robj <- risq(~ x + y, "gaussian", data_1)
  expect_error(ri_by_var(robj, "r", 0), "`variables` must be a character vector")
  expect_error(ri_by_var(robj, "r", c("x", "r")), "`variables` must not contain the target")
  expect_error(ri_by_var(robj, "r", c("x", "q")), "`variables` must be present in risq data")
  expect_error(ri_by_var(robj, "r", c("x", "y")), "`variables` must be factors in risq data")
  expect_error(ri_by_var(robj, "r", c("s", "x")), "`variables` must be factors in risq data")
  expect_error(ri_by_var(robj, "r", c("x", "x")), "`variables` must not contain duplicates")

  # Test missing values in variables.
  data_missing_z <- data_1
  data_missing_z$z[5] <- NA
  robj <- risq(~ x + y, "binomial", data_missing_z)
  expect_error(ri_by_var(robj, "r", c("x", "z")), "`variables` must not contain `NA` values")
})
