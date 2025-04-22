source("ref/risq21.r")

data_1 <- data.frame(
  r = c(rep(FALSE, 5), rep(TRUE, 5)),
  x = factor(rep(c("one", "two", "three"), length.out = 10)),
  y = c(1, 2, 4, 2, 2, 7, 6, 7, 8, 0)
)

data_2 <- hlc[seq(1, nrow(hlc), 10), ]

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
  list(cv = ref$CV, cv_se = ref$CVSE)
}

test_cv_vs_ref <- function(family) {
  # Using data_1.
  formula <- r ~ x + y
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  robj <- risq(predictor, family, data_1)
  cv_tst <- cv(robj, target)
  cv_ref <- get_ref(formula, family, data_1)
  expect_equal(cv_tst, cv_ref)

  # Using data_2 with default weights and strata (SI).
  formula <- response ~ gender + age + urbanisation
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  robj <- risq(predictor, family, data_2)
  expect_equal(robj$design$type, "SI")
  cv_tst <- cv(robj, target)
  cv_ref <- get_ref(formula, family, data_2)
  expect_equal(cv_tst, cv_ref)

  # Using data_2 with custom weights and default strata (STSI).
  formula <- response ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:5, length.out = nrow(data_2))
  robj <- risq(predictor, family, data_2, weights)
  expect_equal(robj$design$type, "STSI")
  cv_tst <- cv(robj, target)
  cv_ref <- suppressWarnings(get_ref(formula, family, data_2, weights))
  expect_equal(cv_tst, cv_ref)

  # Using data_2 with custom weights and custom strata (PPS).
  formula <- response ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:7, length.out = nrow(data_2))
  strata <- factor(rep(1:3, length.out = nrow(data_2)))
  robj <- risq(predictor, family, data_2, weights, strata)
  expect_equal(robj$design$type, "PPS")
  cv_tst <- cv(robj, target)
  cv_ref <- suppressWarnings(get_ref(formula, family, data_2, weights, strata))
  expect_equal(cv_tst, cv_ref)
}

test_that("cv values equal reference solution (binomial)", {
  test_cv_vs_ref("binomial")
})

test_that("cv values equal reference solution (gaussian)", {
  test_cv_vs_ref("gaussian")
})

test_that("cv detects invalid input", {
  # Test invalid risq object.
  robj <- risq(~ gender + age, "binomial", data_2)
  class(robj) <- "frisq"
  expect_error(cv(robj, "response"), "must be a risq object")

  # Test invalid target argument.
  robj <- risq(~ gender + job, "binomial", data_2)
  expect_error(cv(robj, TRUE), "must be a string")
  expect_error(cv(robj, "job"), "must not be in risq predictor")
  expect_error(cv(robj, "jobs"), "must be present in risq data")
  expect_error(cv(robj, "household"), "must be logical in risq data")
})
