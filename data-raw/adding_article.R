# Process for adding an article ================================================
# 1- create article in the vignette folder ------------------------------
usethis::use_vignette("contributing")
# update the file contributing.Rmd with content

# 2- Once finished. Do the following
devtools::document()
devtools::load_all()
pkgdown::build_site()
pkgdown::build_home() # to check website and if all good push results to github

# 3- Push changes then rise a PR

