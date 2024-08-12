test_that("all columns are non numeric", {
  dat = data.frame(col = LETTERS[1:5],
                  col2 = letters[6:10]
                  # col3 = rnorm(5),
                  # col4 = rnorm(5)
                  )
  expect_error(
    corr_df(dat),
    regexp = "The dataframe must at least has 2 numeric columns",
    fixed = TRUE
  )
})
