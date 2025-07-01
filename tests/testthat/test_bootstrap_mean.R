mean_by_row <- function(m, ...) {
  apply(m, 1, function(x) mean(x, ...))
}

test_that("mean.bootstrap works without dataframe", {
  set.seed(0)
  values_df <- NULL

  values <- matrix(runif(10, min=0, max=100), nrow = 1)
  bs <- build_bootstrap(NULL, values_df, values)
  expect_equal(mean(bs), mean_by_row(values))

  values[1, 4] <- NA
  bs <- build_bootstrap(NULL, values_df, values)
  expect_equal(mean(bs, na.rm = FALSE), mean_by_row(values, na.rm = FALSE))
  expect_equal(mean(bs, na.rm = TRUE), mean_by_row(values, na.rm = TRUE))
})

test_that("mean.bootstrap works with dataframe", {
  set.seed(0)
  values_df <- data.frame(a = c("a1", "a2", "a3"), b = c("b1", "b2", "b3"))
  expected_mean <- values_df

  values = matrix(runif(30, min=0, max=100), nrow = 3)
  bs <- build_bootstrap(NULL, values_df, values)
  expected_mean$value <- mean_by_row(values)
  expect_equal(mean(bs), expected_mean)

  values[2, 1] <- NA
  bs <- build_bootstrap(NULL, values_df, values)
  expected_mean$value <- mean_by_row(values, na.rm = FALSE)
  expect_equal(mean(bs, na.rm = FALSE), expected_mean)
  expected_mean$value <- mean_by_row(values, na.rm = TRUE)
  expect_equal(mean(bs, na.rm = TRUE), expected_mean)
})
