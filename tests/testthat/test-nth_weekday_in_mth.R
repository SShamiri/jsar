test_that("year must be numeric", {
  expect_error(
    nth_weekday_in_mth ("2024",8,1,"Mon"),
    regexp = "year must be numeric and > 0",
    fixed = TRUE
  )
})

test_that("year > 0", {
  expect_error(
    nth_weekday_in_mth (-2000,8,1,"Mon"),
    regexp = "year must be numeric and > 0",
    fixed = TRUE
  )
})

test_that("month numeric", {
  expect_error(
    nth_weekday_in_mth (2000,"8",1,"Mon"),
    regexp = "months must be numeric and between 1 & 12",
    fixed = TRUE
  )
})

test_that("month between 1 & 12", {
  expect_error(
    nth_weekday_in_mth (2000,18,1,"Mon"),
    regexp = "months must be numeric and between 1 & 12",
    fixed = TRUE
  )
})

test_that("n numeric", {
  expect_error(
    nth_weekday_in_mth (2000, 8, "1","Mon"),
    regexp = "n must be numeric and between -5 & 5",
    fixed = TRUE
  )
})

test_that("n between -5 & 5", {
  expect_error(
    nth_weekday_in_mth (2000, 8, 7,"Mon"),
    regexp = "n must be numeric and between -5 & 5",
    fixed = TRUE
  )
})

test_that("weekday character", {
  expect_error(
    nth_weekday_in_mth (2000, 8, 3,12),
    regexp = "weekdays must be character 'Mon-Sun'",
    fixed = TRUE
  )
})

test_that("weekday between Mon-Sun", {
  expect_error(
    nth_weekday_in_mth (2000, 8, 3,"Monday"),
    regexp = "weekdays must be character 'Mon-Sun'",
    fixed = TRUE
  )
})
