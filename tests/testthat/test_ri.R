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
  list(value = ref$R, se = ref$RSE)
}

test_ri_vs_ref <- function(family) {
  # Using data_1.
  formula <- r ~ x + y
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  robj <- risq(predictor, family, data_1)
  ri_ref <- get_ref(formula, family, data_1)
  ri_tst <- ri(robj, target)
  expect_equal(ri_tst, ri_ref)
  ri_tst_no_se <- ri(robj, target, include_se = FALSE)
  expect_equal(ri_tst_no_se, list(value = ri_tst$value))

  # Using data_2 with default weights and strata (SI).
  formula <- response ~ gender + age + urbanisation
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  robj <- risq(predictor, family, data_2)
  expect_equal(robj$design$type, "SI")
  ri_ref <- get_ref(formula, family, data_2)
  ri_tst <- ri(robj, target)
  expect_equal(ri_tst, ri_ref)
  ri_tst_no_se <- ri(robj, target, include_se = FALSE)
  expect_equal(ri_tst_no_se, list(value = ri_tst$value))

  # Using data_2 with custom weights and default strata (STSI).
  formula <- response ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:5, length.out = nrow(data_2))
  robj <- risq(predictor, family, data_2, weights)
  expect_equal(robj$design$type, "STSI")
  ri_ref <- suppressWarnings(get_ref(formula, family, data_2, weights))
  ri_tst <- ri(robj, target)
  expect_equal(ri_tst, ri_ref)
  ri_tst_no_se <- ri(robj, target, include_se = FALSE)
  expect_equal(ri_tst_no_se, list(value = ri_tst$value))

  # Using data_2 with custom weights and custom strata (PPS).
  formula <- response ~ gender + age
  target <- as.character(formula[[2]])
  predictor <- formula[c(1, 3)]
  weights <- rep(1:7, length.out = nrow(data_2))
  strata <- factor(rep(1:3, length.out = nrow(data_2)))
  robj <- risq(predictor, family, data_2, weights, strata)
  expect_equal(robj$design$type, "PPS")
  ri_ref <- suppressWarnings(get_ref(formula, family, data_2, weights, strata))
  ri_tst <- ri(robj, target)
  expect_equal(ri_tst, ri_ref)
  ri_tst_no_se <- ri(robj, target, include_se = FALSE)
  expect_equal(ri_tst_no_se, list(value = ri_tst$value))
}

test_that("ri values equal reference solution (binomial)", {
  test_ri_vs_ref("binomial")
})

test_that("ri values equal reference solution (gaussian)", {
  test_ri_vs_ref("gaussian")
})

test_that("ri detects invalid input", {
  # Test invalid risq object.
  robj <- risq(~ gender + age, "binomial", data_2)
  class(robj) <- "brisq"
  expect_error(ri(robj, "response"), "`x` must be a risq object")

  # Test invalid target argument.
  robj <- risq(~ gender + job, "binomial", data_2)
  expect_error(ri(robj, c("age", "household")), "`target` must be a string")
  expect_error(ri(robj, "job"), "`target` must not be in risq predictor")
  expect_error(ri(robj, "ages"), "`target` must be present in risq data")
  expect_error(ri(robj, "age"), "`target` must be of type logical in risq data")

  # Test missing values in target.
  data_missing <- data_1
  data_missing$r[1] <- NA
  robj <- risq(~ x + y, "binomial", data_missing)
  expect_error(ri(robj, "r"), "`target` must not contain `NA` values in risq data")

  # Test invalid include_se argument.
  robj <- risq(~ gender + job, "binomial", data_2)
  expect_error(ri(robj, "response", include_se = 0), "`include_se` must be TRUE or FALSE")
  expect_error(ri(robj, "response", include_se = c(TRUE, TRUE)), "`include_se` must be TRUE or FALSE")
})
