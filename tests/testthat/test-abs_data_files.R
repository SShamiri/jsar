library(jsar)

filename <- "../testdata/EQ08.xlsx"

test_that("read abs file with data saved in Data 1-6 sheets", {
  lst <- abs_data_files(filename)
  df <- tidy_abs_data(lst)

  expect_type(lst, "list")
  expect_s3_class(df, "data.frame")
  expect_s3_class(df, "tbl")

})
