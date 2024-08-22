# test_that("model obj is lm or sfa", {
#   set.seed(2019)
#   x <- seq(0, 50, 1)
#   y <- ((runif(1, 10, 20) * x) / (runif(1, 0, 10) + x)) + rnorm(51, 0, 1)
#
#   mod <- stats::nls(y ~ a * x / (b + x), start = list(a = 0.12345, b = 0.54321))
#   expect_error(
#     compare_models(mod)$compare,
#     regexp = "models must be an object of lm or sfa",
#     fixed = TRUE
#   )
# })

# test_that("Neither texreg nor broom supports models of class frontier", {
#   library(frontier)
#   data(worldprod, package = "sfaR")
#
#   mod <- frontier::sfa(ly ~ lk + ll, data = worldprod)
#   expect_error(
#     compare_models(mod),
#     regexp = "Neither texreg nor broom supports models of class frontier",
#     fixed = TRUE
#   )
# })


test_that("compare_models with lm", {
  testthat::skip_on_cran()
  set.seed(2019)
  x <- seq(0, 50, 1)
  y <- ((runif(1, 10, 20) * x) / (runif(1, 0, 10) + x)) + rnorm(51, 0, 1)
  dat <- data.frame(x = x, y = y)
  m <- lm(y ~ x, data = dat)

  cm <- compare_models(m)
  expect_equal(dim(cm), c(6, 2))

})
