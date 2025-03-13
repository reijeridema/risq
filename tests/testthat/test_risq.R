risq_test_data <- data.frame(
  r = c(rep(FALSE, 5), rep(TRUE, 5)),
  x = factor(rep(c("one", "two", "three"), length.out = 10)),
  y = 1:10,
  z = rep(0, 10)
)

test_that("risq returns expected object", {
  formula <- r ~ x + y
  data <- risq_test_data
  weights <- rep(7, 10)
  strata <- factor(rep(1, 10))

  expected_class <- "risq"
  expected_model_binomial <- list(formula = formula, family = "binomial")
  expected_model_gaussian <- list(formula = formula, family = "gaussian")
  expected_data <- data
  expected_design <- list(type = "SI", weights = weights, strata = strata)

  result <- risq(formula, "binomial", data, weights, strata)
  expect_equal(class(result), expected_class)
  expect_equal(result$model, expected_model_binomial)
  expect_equal(result$data, expected_data)
  expect_equal(result$design, expected_design)

  result <- risq(formula, "gaussian", data, weights, strata)
  expect_equal(class(result), expected_class)
  expect_equal(result$model, expected_model_gaussian)
  expect_equal(result$data, expected_data)
  expect_equal(result$design, expected_design)
})

test_that("risq returns expected design type", {
  formula <- r ~ x + y
  family <- "binomial"
  data <- risq_test_data

  # Test design type SI.
  weights <- rep(2, 10)
  strata <- factor(rep(1, 10))
  result <- risq(formula, family, data, weights, strata)
  expect_equal(result$design$type, "SI")

  # Test design type STSI.
  weights <- rep(2, 10)
  strata <- factor(c(rep(1, 5), rep(2, 5)))
  result <- risq(formula, family, data, weights, strata)
  expect_equal(result$design$type, "STSI")

  weights <- c(3.2, 1, 1, 3.2, 5, 1, 1, 9, 5, 5)
  strata <- factor(c(2, 1, 1, 2, 3, 1, 1, 4, 3, 3))
  result <- risq(formula, family, data, weights, strata)
  expect_equal(result$design$type, "STSI")

  # Test design type PPS.
  weights <- c(3.2, 1, 1, 3.2, 5, 1, 1, 9, 5, 5)
  strata <- factor(rep(1, 10))
  result <- risq(formula, family, data, weights, strata)
  expect_equal(result$design$type, "PPS")

  weights <- c(3.2, 1, 1, 3.2, 5, 1, 1, 9, 5, 5)
  strata <- factor(c(1, 1, 1, 1, 2, 1, 1, 2, 2, 2))
  result <- risq(formula, family, data, weights, strata)
  expect_equal(result$design$type, "PPS")
})

test_that("risq defaults work", {
  formula <- r ~ x + y
  data = risq_test_data

  # Test default weights and strata.
  all_one_vector <- rep(1, nrow(risq_test_data))
  weights <- all_one_vector
  strata <- factor(all_one_vector)
  test_result <- risq(formula = formula, data = data)
  expected_result <- risq(formula, "binomial", data, weights, strata)
  expect_equal(test_result, expected_result)

  # Test default strata for custom weights.
  weights <- c(3.2, 1, 1, 3.2, 5, 1, 1, 9, 5, 5)
  strata <- factor(c(2, 1, 1, 2, 3, 1, 1, 4, 3, 3))
  test_result <- risq(formula = formula, data = data, weights = weights)
  expected_result <- risq(formula, "binomial", data, weights, strata)
  expect_equal(test_result, expected_result)
})

test_that("risq detects invalid input", {
  formula <- r ~ x + y
  data = risq_test_data

  # Formula
  expect_error(risq(1, data = data), "must be an object of class formula")
  expect_error(risq(~ x + y, data = data), "exactly 1 dependent variable")
  expect_error(risq(1 ~ x + y, data = data), "exactly 1 dependent variable")
  expect_error(risq(r + x ~ y, data = data), "exactly 1 dependent variable")
  expect_error(risq(r ~ r + x, data = data), "must not also be independent")

  # Family
  expect_error(risq(formula, family = 1, data = data), "character vector")
  expect_error(risq(formula, family = "poisson", data = data), "should be one of")

  # Data
  expect_error(risq(a ~ b + c, data = 1), "must be a data frame")
  expect_error(risq(r ~ x + Q, data = data), "must contain all variables")
  expect_error(risq(z ~ x + y, data = data), "must be a logical vector")

  # Weights
  expect_error(risq(formula, data = data, weights = TRUE), "must be a numeric vector")
  expect_error(risq(formula, data = data, weights = 1:9), "length must match")
  expect_error(risq(formula, data = data, weights = 1:11), "length must match")
  expect_error(risq(formula, data = data, weights = c(1:8, 0.9, 1)), "value 1 or greater")

  # Strata
  expect_error(risq(formula, data = data, strata = 1:10), "must be a factor")
  expect_error(risq(formula, data = data, strata = factor(1:9)), "length must match")
  expect_error(risq(formula, data = data, strata = factor(1:11)), "length must match")
})

test_that("risq works with hlc data", {
  response_formula <- formula(response ~ gender + age + job)
  expect_no_error(risq(formula = response_formula, data = hlc))
})
