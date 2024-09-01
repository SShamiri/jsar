library(jsar)

filename <- "../testdata/MRM1.xlsx"

test_that("read abs file with data saved in Table1-6 sheets", {
  lst <- abs_tables_files(filename)
  df <- tidy_abs_tables(lst)

  expect_type(lst, "list")
  expect_s3_class(df, "data.frame")
  expect_s3_class(df, "tbl")

})
