risq_test_data <- data.frame(
  x = factor(rep(c("a", "b", "c"), length.out = 10)),
  y = 1:10,
  z = rep(0, 10)
)

test_that("risq returns expected object", {
  predictor <- ~ x + y
  data <- risq_test_data
  weights <- rep(7, 10)
  strata <- factor(rep(1, 10))

  expected_class <- "risq"
  expected_model_binomial <- list(predictor = predictor, family = "binomial")
  expected_model_gaussian <- list(predictor = predictor, family = "gaussian")
  expected_data <- data
  expected_design <- list(type = "SI", weights = weights, strata = strata)

  result <- risq(predictor, "binomial", data, weights, strata)
  expect_equal(class(result), expected_class)
  expect_equal(result$model, expected_model_binomial)
  expect_equal(result$data, expected_data)
  expect_equal(result$design, expected_design)

  result <- risq(predictor, "gaussian", data, weights, strata)
  expect_equal(class(result), expected_class)
  expect_equal(result$model, expected_model_gaussian)
  expect_equal(result$data, expected_data)
  expect_equal(result$design, expected_design)
})

test_that("risq returns expected design type", {
  predictor <- ~ x + y
  family <- "binomial"
  data <- risq_test_data

  # Test design type SI.
  weights <- rep(2, 10)
  strata <- factor(rep(1, 10))
  result <- risq(predictor, family, data, weights, strata)
  expect_equal(result$design$type, "SI")

  # Test design type STSI.
  weights <- rep(2, 10)
  strata <- factor(c(rep(1, 5), rep(2, 5)))
  result <- risq(predictor, family, data, weights, strata)
  expect_equal(result$design$type, "STSI")

  weights <- c(3.2, 1, 1, 3.2, 5, 1, 1, 9, 5, 5)
  strata <- factor(c(2, 1, 1, 2, 3, 1, 1, 4, 3, 3))
  result <- risq(predictor, family, data, weights, strata)
  expect_equal(result$design$type, "STSI")

  # Test design type PPS.
  weights <- c(3.2, 1, 1, 3.2, 5, 1, 1, 9, 5, 5)
  strata <- factor(rep(1, 10))
  result <- risq(predictor, family, data, weights, strata)
  expect_equal(result$design$type, "PPS")

  weights <- c(3.2, 1, 1, 3.2, 5, 1, 1, 9, 5, 5)
  strata <- factor(c(1, 1, 1, 1, 2, 1, 1, 2, 2, 2))
  result <- risq(predictor, family, data, weights, strata)
  expect_equal(result$design$type, "PPS")
})

test_that("risq defaults work", {
  predictor <- ~ x + y
  data = risq_test_data

  # Test default weights and strata.
  all_one_vector <- rep(1, nrow(risq_test_data))
  weights <- all_one_vector
  strata <- factor(all_one_vector)
  test_result <- risq(predictor = predictor, data = data)
  expected_result <- risq(predictor, "binomial", data, weights, strata)
  expect_equal(test_result, expected_result)

  # Test default strata for custom weights.
  weights <- c(3.2, 1, 1, 3.2, 5, 1, 1, 9, 5, 5)
  strata <- factor(c(2, 1, 1, 2, 3, 1, 1, 4, 3, 3))
  test_result <- risq(predictor = predictor, data = data, weights = weights)
  expected_result <- risq(predictor, "binomial", data, weights, strata)
  expect_equal(test_result, expected_result)
})

test_that("risq detects invalid input", {
  predictor <- ~ x + y
  data = risq_test_data

  # Predictor
  expect_error(risq(1, data = data), "must be an object of class formula")
  expect_error(risq(1 ~ x + y, data = data), "left-hand side must be empty")
  expect_error(risq(r ~ x + y, data = data), "left-hand side must be empty")
  expect_error(risq(r + x ~ y, data = data), "left-hand side must be empty")

  # Family
  expect_error(risq(predictor, family = 1, data = data), "character vector")
  expect_error(risq(predictor, family = "poisson", data = data), "should be one of")

  # Data
  expect_error(risq(~ b + c, data = 1), "must be a data frame")
  expect_error(risq(~ b + y, data = data), "must contain all variables")
  expect_error(risq(~ x + c, data = data), "must contain all variables")

  # Weights
  expect_error(risq(predictor, data = data, weights = TRUE), "must be a numeric vector")
  expect_error(risq(predictor, data = data, weights = 1:9), "length must match")
  expect_error(risq(predictor, data = data, weights = 1:11), "length must match")
  expect_error(risq(predictor, data = data, weights = c(1:8, 0.9, 1)), "value 1 or greater")

  # Strata
  expect_error(risq(predictor, data = data, strata = 1:10), "must be a factor")
  expect_error(risq(predictor, data = data, strata = factor(1:9)), "length must match")
  expect_error(risq(predictor, data = data, strata = factor(1:11)), "length must match")
})

test_that("risq works with hlc data", {
  predictor <- ~ gender + age + job
  expect_no_error(risq(predictor = predictor, data = hlc))
})
