test_that("column must be numeric", {
  df <- data.frame(date = seq(as.Date("2000/1/1"), by = "month", length.out = 5),
                   col = runif(5, min = 2, max = 20))
  expect_error(
    df %>%
      mutate(
        chng = calc_change(as.character(col), 1, date)
      ),
    regexp = "column must be numeric",
    fixed = TRUE
  )
})

test_that("dt must be date", {
  df <- data.frame(date = seq(as.Date("2000/1/1"), by = "month", length.out = 5),
                   col = runif(5, min = 2, max = 20))
  expect_error(
    df %>%
      mutate(
        chng = calc_change(col, 1, as.numeric(date))
      ),
    regexp = "dt must be of class Date",
    fixed = TRUE
  )
})
