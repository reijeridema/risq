source("ref/risq21.r")

data_1 <- data.frame(
  r = c(rep(FALSE, 5), rep(TRUE, 5)),
  x = factor(rep(c("one", "two", "three"), length.out = 10)),
  y = c(1, 2, 4, 2, 2, 7, 6, 7, 8, 0)
)

data_2 <- hlc[seq(1, nrow(hlc), 10), ]

# Get reference value from RISQ 2.1.
get_ref <- function(formula, family, data, weights = NULL, strata = NULL) {
  args <- list(
    formula = formula,
    sampleData = data,
    family = family,
    withPartials = FALSE,
    withPartialCV = FALSE
  )
  if (!is.null(weights)) {
    args$sampleWeights <- weights
  }
  if (!is.null(strata)) {
    args$sampleStrata <- strata
  }
  ref <- do.call(what = getRIndicator, args = args)
  ref$propMean
}

# Test rr() versus reference for the given family.
test_rr_vs_ref <- function(family) {
  # Using data_1.
  formula <- r ~ x + y
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  robj <- risq(predictor, family, data_1)
  rr_tst <- rr(robj, target)
  rr_ref <- get_ref(formula, family, data_1)
  expect_equal(rr_tst, rr_ref)

  # Using data_2 with default weights and strata (SI).
  formula <- response_1 ~ gender + age + urbanisation
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  robj <- risq(predictor, family, data_2)
  expect_equal(robj$design$type, "SI")
  rr_tst <- rr(robj, target)
  rr_ref <- get_ref(formula, family, data_2)
  expect_equal(rr_tst, rr_ref)

  # Using data_2 with custom weights and default strata (STSI).
  formula <- response_2 ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:5, length.out = nrow(data_2))
  robj <- risq(predictor, family, data_2, weights)
  expect_equal(robj$design$type, "STSI")
  rr_tst <- rr(robj, target)
  rr_ref <- suppressWarnings(get_ref(formula, family, data_2, weights))
  expect_equal(rr_tst, rr_ref)

  # Using data_2 with custom weights and custom strata (PPS).
  formula <- response_3 ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:7, length.out = nrow(data_2))
  strata <- factor(rep(1:3, length.out = nrow(data_2)))
  robj <- risq(predictor, family, data_2, weights, strata)
  expect_equal(robj$design$type, "PPS")
  rr_tst <- rr(robj, target)
  rr_ref <- suppressWarnings(get_ref(formula, family, data_2, weights, strata))
  expect_equal(rr_tst, rr_ref)
}

test_that("rr values equal reference solution (binomial)", {
  test_rr_vs_ref("binomial")
})

test_that("rr values equal reference solution (gaussian)", {
  test_rr_vs_ref("gaussian")
})

test_that("rr detects invalid input", {
  # Test invalid risq object.
  robj <- risq(~ gender + age, "binomial", data_2)
  class(robj) <- NULL
  expect_error(rr(robj, "response_1"), "`x` must be a risq object")

  # Test invalid target argument.
  robj <- risq(~ gender + response_1, "binomial", data_2)
  expect_error(rr(robj, 1), "`target` must be a string")
  expect_error(rr(robj, "response_1"), "`target` must not be in risq predictor")
  expect_error(rr(robj, "INVALID"), "`target` must be present in risq data")
  expect_error(rr(robj, "house_value"), "`target` must be of type logical in risq data")

  # Test missing values in target.
  data_missing <- data_1
  data_missing$r[3] <- NA
  robj <- risq(~ x + y, "gaussian", data_missing)
  expect_error(rr(robj, "r"), "`target` must not contain `NA` values in risq data")
})
