library(jsar)

filename <- "../testdata/MRM1.xlsx"

test_that("read abs file with data saved in Table1-6 sheets", {
  df <- clean_abs_raw(filename)

  expect_s3_class(df, "data.frame")
  expect_s3_class(df, "tbl")

})

