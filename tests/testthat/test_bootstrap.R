data_1 <- hlc[seq(1, nrow(hlc), 100), ]
robj_1 <- risq(~ age + gender, data = data_1)

test_bootstrap_values <- function(robj, fun, ..., seed) {
  set.seed(seed)
  sample_1 <- sample(robj, size = nrow(robj$data), replace = TRUE)
  result_1 <- fun(sample_1, ..., include_se = FALSE)
  sample_2 <- sample(robj, size = nrow(robj$data), replace = TRUE)
  result_2 <- fun(sample_2, ..., include_se = FALSE)
  expected_values_df <- if (is.data.frame(result_1)) {
    result_1[-ncol(result_1)]
  } else {
    NULL
  }
  expected_values <- matrix(c(result_1$value, result_2$value), ncol = 2)

  bs_result <- bootstrap(robj, fun, ..., seed = seed, iterations = 2)
  expect_equal(class(bs_result), "bootstrap")
  expect_equal(bs_result$seed, seed)
  expect_equal(bs_result$values_df, expected_values_df)
  expect_equal(bs_result$values, expected_values)
}

test_that("bootstrap works with ri", {
  test_bootstrap_values(robj_1, ri, target = "response", seed = 0)
})

test_that("bootstrap works with ri_by_var", {
  variables <- c("age", "urbanisation")
  test_bootstrap_values(
    robj_1, ri_by_var, target = "response", variables = variables, seed = 1
  )
})

test_that("bootstrap works with ri_by_cat", {
  variables <- c("age", "marital_status")
  test_bootstrap_values(
    robj_1, ri_by_cat, target = "response", variables = variables, seed = 2
  )
})

test_that("bootstrap works with cv", {
  test_bootstrap_values(robj_1, cv, target = "response", seed = 42)
})

test_that("bootstrap works with cv_by_var", {
  variables <- c("age", "urbanisation")
  test_bootstrap_values(
    robj_1, cv_by_var, target = "response", variables = variables, seed = 43
  )
})

test_that("bootstrap works with cv_by_cat", {
  variables <- c("age", "marital_status")
  test_bootstrap_values(
    robj_1, cv_by_cat, target = "response", variables = variables, seed = 44
  )
})

test_that("bootstrap detects invalid input", {
  # Test invalid risq object.
  robj <- risq(~ age + gender, data = data_1)
  class(robj) <- "rix"
  expect_error(
    bootstrap(robj, ri, target = "response", iterations = 1),
    "`x` must be a risq object"
  )

  # Test invalid seed argument.
  robj <- risq(~ age + gender, data = data_1)
  expect_error(
    bootstrap(robj, ri, target = "response", seed = "", iterations = 1),
    "`seed` must be an integer"
  )
  expect_error(
    bootstrap(robj, ri, target = "response", seed = c(0, 1), iterations = 1),
    "`seed` must be an integer"
  )

  # Test invalid iterations argument.
  robj <- risq(~ age + gender, data = data_1)
  expect_error(
    bootstrap(robj, ri, target = "response", iterations = 0),
    "`iterations` must be a positive integer"
  )
  expect_error(
    bootstrap(robj, ri, target = "response", iterations = -1),
    "`iterations` must be a positive integer"
  )
  expect_error(
    bootstrap(robj, ri, target = "response", iterations = "ten"),
    "`iterations` must be a positive integer"
  )
})
