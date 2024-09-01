library(jsar)

filename <- "../testdata/6291003.xlsx"

test_that("read abs file with index sheet", {
  lst <- abs_indexed_files(filename)
  df <- tidy_abs_indexed(lst)

  expect_type(lst, "list")
  expect_s3_class(df, "data.frame")
  expect_s3_class(df, "tbl")

})
