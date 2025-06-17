data <- data.frame(x = 1:10, y = 10 * (1:10))
weights <- 10 + (1:10)
strata <- factor(20 + (1:10))

test_without_replacement <- function(robj, size) {
  if (size < 1) {
    expect_error(
      sample(robj, size, replace = FALSE), "`size` must be a positive integer"
    )
  } else if (size > nrow(robj$data)) {
    expect_error(
      sample(robj, size, replace = FALSE), "sample larger than the population"
    )
  } else {
    s <- sample(robj, size, replace = FALSE)
    expect_equal(s$model, robj$model)
    expect_equal(nrow(s$data), size)
    expect_equal(all(s$data$x %in% robj$data$x), TRUE)
    expect_equal(s$data$y, 10 * s$data$x)
    expect_equal(s$design$weights, 10 + s$data$x)
    expect_equal(s$design$strata, factor(20 + s$data$x))
    expect_equal(any(duplicated(s$data$x)), FALSE)
  }
}

test_with_replacement <- function(robj, size) {
  if (size < 1) {
    expect_error(
      sample(robj, size, replace = TRUE), "`size` must be a positive integer"
    )
  } else {
    s <- sample(robj, size, replace = TRUE)
    expect_equal(s$model, robj$model)
    expect_equal(nrow(s$data), size)
    expect_equal(all(s$data$x %in% robj$data$x), TRUE)
    expect_equal(s$data$y, 10 * s$data$x)
    expect_equal(s$design$weights, 10 + s$data$x)
    expect_equal(s$design$strata, factor(20 + s$data$x))
  }
}

test_that("sample works without replacement", {
  set.seed(0)
  robj <- risq(~ x, "binomial", data, weights, strata)
  test_without_replacement(robj, 0)
  test_without_replacement(robj, 1)
  test_without_replacement(robj, 5)
  robj <- risq(~ x, "gaussian", data, weights, strata)
  test_without_replacement(robj, 10)
  test_without_replacement(robj, 11)
})

test_that("sample works with replacement", {
  set.seed(0)
  robj <- risq(~ y, "binomial", data, weights, strata)
  test_with_replacement(robj, 0)
  test_with_replacement(robj, 1)
  robj <- risq(~ y, "gaussian", data, weights, strata)
  test_with_replacement(robj, 8)
  test_with_replacement(robj, 20)
})

test_that("sample works with weights", {
  set.seed(0)
  robj <- risq(~ x + y, "binomial", data, weights, strata)
  prob <- rep(c(0, 1), 5)

  size <- 5
  s <- sample(robj, size, replace = FALSE, prob = prob)
  expect_equal(s$model, robj$model)
  expect_equal(nrow(s$data), size)
  expect_equal(all(s$data$x %in% robj$data$x[which(prob != 0)]), TRUE)
  expect_equal(s$data$y, 10 * s$data$x)
  expect_equal(s$design$weights, 10 + s$data$x)
  expect_equal(s$design$strata, factor(20 + s$data$x))
  expect_equal(any(duplicated(s$data$x)), FALSE)

  size <- 6
  expect_error(
    sample(robj, size, prob = prob), "too few positive probabilities"
  )

  size <- 6
  s <- sample(robj, size, replace = TRUE, prob = prob)
  expect_equal(s$model, robj$model)
  expect_equal(nrow(s$data), size)
  expect_equal(all(s$data$x %in% robj$data$x[which(prob != 0)]), TRUE)
  expect_equal(s$data$y, 10 * s$data$x)
  expect_equal(s$design$weights, 10 + s$data$x)
  expect_equal(s$design$strata, factor(20 + s$data$x))

  size <- 6
  expect_error(
    sample(robj, 10, replace = TRUE, prob = rep(0, 10)),
    "too few positive probabilities"
  )
})

test_that("sample defaults work", {
  set.seed(0)
  robj <- risq(~ x + y, "binomial", data, weights, strata)
  set.seed(0)
  s1 <- sample(robj)
  set.seed(0)
  s2 <- sample(robj, size = nrow(robj$data), replace = FALSE, prob = NULL)
  expect_equal(s1, s2)
})

test_that("sample retains design type", {
  # Build risq object with PPS design.
  data_1 <- data.frame(x = 1:6)
  weights_1 <- c(1, 1, 1, 2, 2, 3)
  strata_1 <- factor(c(1, 1, 1, 2, 2, 2))
  robj_1 <- risq(~ x, "binomial", data_1, weights_1, strata_1)
  expect_equal(robj_1$design$type, "PPS")

  # Draw sample for which weight and strata suggest a different design.
  set.seed(1)
  robj_2 <- sample(robj_1, replace = TRUE)
  derived_design <- build_design(robj_2$design$weights, robj_2$design$strata)
  expect_equal(derived_design$type, "STSI")

  # Verify that sample retains original design type.
  expect_equal(robj_2$design$type, "PPS")
})
