var_by_row <- function(m, ...) {
  apply(m, 1, function(x) stats::var(x, ...))
}

test_that("var.bootstrap works without dataframe", {
  set.seed(0)
  values_df <- NULL

  values <- matrix(runif(10, min=0, max=100), nrow = 1)
  bs <- build_bootstrap(NULL, values_df, values)
  expect_equal(var(bs), var_by_row(values))

  values[1, 4] <- NA
  bs <- build_bootstrap(NULL, values_df, values)
  expect_equal(var(bs, na.rm = FALSE), var_by_row(values, na.rm = FALSE))
  expect_equal(var(bs, na.rm = TRUE), var_by_row(values, na.rm = TRUE))
})

test_that("var.bootstrap works with dataframe", {
  set.seed(0)
  values_df <- data.frame(a = c("a1", "a2", "a3"), b = c("b1", "b2", "b3"))
  expected_var <- values_df

  values = matrix(runif(30, min=0, max=100), nrow = 3)
  bs <- build_bootstrap(NULL, values_df, values)
  expected_var$value <- var_by_row(values)
  expect_equal(var(bs), expected_var)

  values[2, 1] <- NA
  bs <- build_bootstrap(NULL, values_df, values)
  expected_var$value <- var_by_row(values, na.rm = FALSE)
  expect_equal(var(bs, na.rm = FALSE), expected_var)
  expected_var$value <- var_by_row(values, na.rm = TRUE)
  expect_equal(var(bs, na.rm = TRUE), expected_var)
})
