test_that("model obj is lm or sfa", {
  set.seed(2019)
  x <- seq(0, 50, 1)
  y <- ((runif(1, 10, 20) * x) / (runif(1, 0, 10) + x)) + rnorm(51, 0, 1)

  mod <- stats::nls(y ~ a * x / (b + x), start = list(a = 0.12345, b = 0.54321))
  expect_error(
    compare_models(mod)$compare,
    regexp = "models must be an object of lm or sfa",
    fixed = TRUE
  )
})
