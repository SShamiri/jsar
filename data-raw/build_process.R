
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
# doc
usethis::use_r("lfs_state") # once modified run doc
devtools::document()

### functions, testing and doc. goes together
# 1- crearte function
# 2- test function
# 3- document the function if for the first time or made change's to the fun
usethis::use_r("calc_change")
devtools::document() # only for first-time or change in fun doc
# test
usethis::use_testthat(3) # this done once for package, only for the first-time to create the folder
usethis::use_test('calc_change')

### create vignettee
usethis::use_vignette("jsar")
devtools::build_vignettes()
browseURL("doc/jsar.html")

###  Set up pkgdown and githup
usethis::use_github() # this create a repo on github
pkgdown::build_site()
pkgdown::init_site()      # copy logo to the right places
pkgdown::build_favicons() # create favicons from the logo
devtools::build_readme()  # update README.md after changes to the Rmd
pkgdown::build_home()     # update home page
usethis::use_lifecycle_badge("experimental") # add badge




usethis::use_pkgdown()
usethis::use_pkgdown_github_pages() # for github
# add logo
usethis::use_logo("inst/figures/logo.png")




#################
 ### github setup
usethis::use_github() # this create a repo on github



###  Set up pkgdown
#usethis::use_pkgdown()
usethis::use_pkgdown_github_pages() # for github
# add logo
usethis::use_logo("inst/figures/logo.png")

usethis::use_github_action()
usethis::use_github_actions_badge()

### testing

# unit test
#usethis::use_r("fun_name")
#usethis::use_testthat("fun_name")


######### Other ----
 lfs_state %>%
 group_by(date) %>%
 summarise(emp = sum(emp), .groups = 'drop') %>%
 mutate(level_chng = calc_change(as.character(emp), 1, date),
        per_chng = calc_change_per(emp, 1, date)
        )

df <- data.frame(date = seq(as.Date("2000/1/1"), by = "month", length.out = 5),
           col = runif(5, min = 2, max = 20))
## Logo
# library(hexSticker)
#
# imgurl <- "/Users/samuelshamiri/Downloads/jobs_skills_australia_logo.jpeg"
# sticker(imgurl,
#         package="utilityJSA",
#         p_size=20,
#         s_x=1,
#         s_y=.75,
#         s_width=.5,
#         h_fill="#2f005f", h_color="#f39c12",
#         #dpi = 1000,
#         filename="inst/figures/logo.png")
