# Process for adding a function ================================================
# 1- create function ------------------------------
usethis::use_r("separate_series")
# a- this will open R/function_name.r
# b- copy in or write the function
# c- Once fun is written, put mouse cursor anywhere in the fun then go to
     # code -> Inset Roxyegen skeleton
# d- fill in the required fields
# e- check for errors devtool::check()

# 2- document the function -----------------------
devtools::document() # execute this if documents change

# 3- test function ------------------------------
usethis::use_test('clean_abs_raw')
# create unit tests in the file tests/testthat/nth_weekday_in_mth.R
# frequently check the results as you progress with
devtools::test()
devtools::check()

# 4- Once finished. Do the following
devtools::document()
devtools::load_all()
pkgdown::build_site()
pkgdown::build_home() # to check website and if all good push results to github

## Other
# TO silence the note below
#checking for future file timestamps ... NOTE
#unable to verify current time
# 1- usethis::edit_r_environ()
# 2 - Add _R_CHECK_SYSTEM_CLOCK_=0 to the file





