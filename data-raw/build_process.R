
##### My setup process
##### Package skeleton ----
### create package and package skeleton
usethis::create_package("/Users/samuelshamiri/projects/utilR")

### we save console process inside raw-data folder
# this folder is ignored during build
usethis::use_data_raw("build_process") # use this file write cmd codes

### update description file
# update Tilte, Authors@R, Description
# License
usethis::use_mit_license()
### Readme file use rmd to modify md
usethis::use_readme_rmd()
# modify the rmd file as required

### setup git to track changes
usethis::use_git()
# to check token
usethis::gh_token_help() # check git
# if you haven't had a token create one
#usethis::create_github_token()
# setup creds
#gitcreds::gitcreds_set() # if password changed

#### Add Data
usethis::use_data_raw("lfs_state")
usethis::use_package("dplyr")
usethis::use_package("magrittr")
usethis::use_package("lubridate")
usethis::use_package("tidyr")
usethis::use_package("corrr","Suggests")

# doc
usethis::use_r("lfs_state") # once modified run doc
devtools::document()

### functions, testing and doc. goes together

# 1- crearte function
# 2- test function
# 3- document the function if for the first time or made change's to the fun
usethis::use_r("calc_change")
devtools::document() # only for first-time or change in fun doc

usethis::use_r("corr_df")
# test
usethis::use_testthat(3) # this done once for package, only for the first-time to create the folder
usethis::use_test('calc_change')

usethis::use_test('fuzzy_join_dates')

usethis::use_test('corr_df')
### create vignettee
usethis::use_vignette("jsar")
devtools::build_vignettes()
browseURL("doc/jsar.html")

###  Set up pkgdown and githup
usethis::use_pkgdown()
#usethis::use_github() # this create a repo on github
pkgdown::build_site()
usethis::use_pkgdown_github_pages() # for github
#pkgdown::init_site()      # copy logo to the right places
# add logo
# first be sure the logo exist in the folder, see Other section 'logo'
usethis::use_logo("inst/figures/logo.png")
pkgdown::build_favicons(overwrite = TRUE) # create favicons from the logo
devtools::build_readme()  # update README.md after changes to the Rmd
pkgdown::build_home()     # update home page
usethis::use_lifecycle_badge("experimental") # add badge
# add citation, see Other section 'citation' to fill CITATION file
usethis::use_citation() # Once the file is updated
citation("jsar")
devtools::document()

### after changes
devtools::document()
devtools::load_all()

pkgdown::build_site()
pkgdown::build_home()


##################
### Set up Git, Github Repository, and Github Actions
# One time set-up with Git, Github, and Github Actions
usethis::use_git(message = "Initial commit")
usethis::use_github(private = FALSE)
usethis::use_github_action("pkgdown")
usethis::use_pkgdown_github_pages()

# locally building site
pkgdown::build_site()

# Install package
#devtools::install_github("FanWangEcon/PkgTestR")
devtools::load_all()
### Initialize Git
usethis::use_git(message = "Initial commit")

### Connect to Github
# 1 Creates a remote repo on github (under the authenticated account) with the same name as the local project folder name
# 2 Set up remote
# 3 Update URL fields in R package
# 4 Commits and pushes local to remote
# 5 Open up remote repo site: https://github.com/FanWangEcon/PkgTestR
#usethis::use_github(private = FALSE)

### Set up Github Actions
usethis::use_github_action("pkgdown")

### Push and Create pkgdown Website
usethis::use_pkgdown_github_pages()

### Continuous Development
# Once these are set up, then in the future, edit functions and vignette files,
#test functions locally, and build site locally with pkgdown::build_site(),
# whose outputs are stored in a gitignored folder and will not be pushed to the remote repo.
# Update the .Rd files if .R files have been modified.
devtools::document()
# locally building site
pkgdown::build_site()

### Process for adding a new function to the repo
#1- create a file with the function name into the ./R with
usethis::use_r("fuzzy_join_dates")
#2- update the document, run this whenever the documentation has changed
devtools::document()
#3- create a unit test for the function



######### Other ----
## testing
 lfs_state %>%
 group_by(date) %>%
 summarise(emp = sum(emp), .groups = 'drop') %>%
 mutate(level_chng = calc_change(as.character(emp), 1, date),
        per_chng = calc_change_per(emp, 1, date)
        )

df <- data.frame(date = seq(as.Date("2000/1/1"), by = "month", length.out = 5),
           col = runif(5, min = 2, max = 20))
## Logo
library(hexSticker)

imgurl <- "/Users/samuelshamiri/Downloads/jobs_skills_australia_logo.jpeg"
# sticker(imgurl,
#         package="jsar",
#         p_size=20,
#         s_x=1,
#         s_y=.75,
#         s_width=.4,
#         h_fill="#2f005f", h_color="#f39c12",
#         #dpi = 1000,
#         filename="inst/figures/logo.png")
hexSticker::sticker(
  subplot = imgurl,
  # image
  s_x = 1, s_y = 1, s_width = 0.4, s_height = 0.3,
  # packageName
  package = "jsar",
  p_size = 20, p_x = 1, p_y = 1.6, p_color = "white",
  # hex
  h_size = 1.2, h_fill = "#2f005f", h_color = "#f39c12",
  dpi = 320,
  filename = "inst/figures/logo.png"
)




## Citation
meta <- packageDescription("jsar")
names(meta)
