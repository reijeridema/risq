test_that("-.bootstrap works without dataframe", {
  set.seed(0)
  values_df <- NULL

  seed <- 0
  values1 <- matrix(runif(10, min=0, max=100), nrow = 1)
  bs1 <- build_bootstrap(seed, values_df, values1)
  values2 <- matrix(runif(10, min=0, max=100), nrow = 1)
  bs2 <- build_bootstrap(seed, values_df, values2)

  expected_diff <- build_bootstrap(seed, values_df, values2 - values1)
  expect_equal(bs2 - bs1, expected_diff)

  seed <- 1
  values1[1, 8] <- NA
  bs1 <- build_bootstrap(seed, values_df, values1)
  values2[1, 1] <- NA
  bs2 <- build_bootstrap(seed, values_df, values2)

  expected_diff <- build_bootstrap(seed, values_df, values2 - values1)
  expect_equal(bs2 - bs1, expected_diff)
})

test_that("-.bootstrap works with dataframe", {
  set.seed(0)
  values_df <- data.frame(a = c("a1", "a2", "a3"), b = c("b1", "b2", "b3"))

  seed = 42
  values1 <- matrix(runif(30, min=0, max=100), nrow = 3)
  bs1 <- build_bootstrap(seed, values_df, values1)
  values2 <- matrix(runif(30, min=0, max=100), nrow = 3)
  bs2 <- build_bootstrap(seed, values_df, values2)

  expected_diff <- build_bootstrap(seed, values_df, values2 - values1)
  expect_equal(bs2 - bs1, expected_diff)

  seed <- 43
  values1[1, 3] <- NA
  values1[3, 1] <- NA
  bs1 <- build_bootstrap(seed, values_df, values1)
  values2[2, 7] <- NA
  values2[3, 1] <- NA
  bs2 <- build_bootstrap(seed, values_df, values2)

  expected_diff <- build_bootstrap(seed, values_df, values2 - values1)
  expect_equal(bs2 - bs1, expected_diff)
})

test_that("-.bootstrap detects invalid input", {
  # Negation not supported.
  bs1 <- build_bootstrap(0, NULL, matrix(1))
  expect_error(-bs1, "Negation not supported for bootstrap objects")

  # Second argument must be a bootstrap object.
  bs1 <- build_bootstrap(0, NULL, matrix(1))
  expect_error(bs1 - 0, "Arguments must be bootstrap objects")
  expect_error(bs1 - matrix(1), "Arguments must be bootstrap objects")

  # Invalid seed.
  bs1 <- build_bootstrap(0, NULL, matrix(1))
  bs2 <- build_bootstrap(NULL, NULL, matrix(1))
  expect_error(bs1 - bs2, "Arguments must have the same non-NULL seed")
  expect_error(bs2 - bs1, "Arguments must have the same non-NULL seed")
  expect_error(bs2 - bs2, "Arguments must have the same non-NULL seed")

  # Invalid values_df.
  bs1 <- build_bootstrap(0, NULL, matrix(1))
  bs2 <- build_bootstrap(0, data.frame(a = "ABC"), matrix(1))
  bs3 <- build_bootstrap(0, data.frame(b = "ABC"), matrix(1))
  expect_error(bs1 - bs2, "Arguments must have the same data rows")
  expect_error(bs2 - bs1, "Arguments must have the same data rows")
  expect_error(bs3 - bs2, "Arguments must have the same data rows")
  expect_error(bs2 - bs3, "Arguments must have the same data rows")

  # Invalid values.
  bs1 <- build_bootstrap(0, NULL, matrix(1:10, nrow = 1))
  bs2 <- build_bootstrap(0, NULL, matrix(1:11, nrow = 1))
  expect_error(bs1 - bs2, "Arguments must have the same number of iterations")
  expect_error(bs2 - bs1, "Arguments must have the same number of iterations")
})