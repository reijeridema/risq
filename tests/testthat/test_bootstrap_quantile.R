quantile_by_row <- function(m, ...) {
  quantile_func <- function(x) {stats::quantile(x, ...)}
  if (nrow(m) == 1) {
    quantile_func(m[1, ])
  } else {
    do.call(rbind, apply(m, 1, quantile_func, simplify = FALSE))
  }
}

test_without_dataframe <- function(probs) {
  values_df <- NULL

  values <- matrix(runif(10, min=0, max=100), nrow = 1)
  bs <- build_bootstrap(NULL, values_df, values)
  expected_quantile <- quantile_by_row(values, probs = probs)
  expect_equal(quantile(bs, probs = probs), expected_quantile)

  values[1, 4] <- NA
  bs <- build_bootstrap(NULL, values_df, values)
  expect_error(quantile(bs, probs = probs, na.rm = FALSE), "not allowed")
  expected_quantile <- quantile_by_row(values, probs = probs, na.rm = TRUE)
  expect_equal(quantile(bs, probs = probs, na.rm = TRUE), expected_quantile)
}

test_with_dataframe <- function(probs) {
  values_df <- data.frame(a = c("a1", "a2", "a3"), b = c("b1", "b2", "b3"))

  values <- matrix(runif(30, min=0, max=100), nrow = 3)
  bs <- build_bootstrap(NULL, values_df, values)
  expected_quantile <- cbind(values_df, quantile_by_row(values, probs = probs))
  expect_equal(quantile(bs, probs = probs), expected_quantile)

  values[3, 7] <- NA
  bs <- build_bootstrap(NULL, values_df, values)
  expect_error(quantile(bs, probs = probs, na.rm = FALSE), "not allowed")
  expected_values <- quantile_by_row(values, probs = probs, na.rm = TRUE)
  expected_quantile <- cbind(values_df, expected_values)
  expect_equal(quantile(bs, probs = probs, na.rm = TRUE), expected_quantile)
}

test_that("quantile.bootstrap works without dataframe", {
  set.seed(0)
  test_without_dataframe(0.5)
  test_without_dataframe(c(0.05, 0.95))
  test_without_dataframe(0:4 / 4)
})

test_that("quantile.bootstrap works with dataframe", {
  set.seed(0)
  test_with_dataframe(0.5)
  test_with_dataframe(c(0.05, 0.95))
  test_with_dataframe(0:4 / 4)
})
