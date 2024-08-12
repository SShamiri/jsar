
#' Fuzzy join for dates
#'
#'@description
#'Finds the n release dates immediately prior to the extraction date, release dates must be prior to extraction dates .
#'
#' @param extraction_date The extraction_date column from a dataframe as date vector.
#' @param release_date The release_date column from a dataframe as date vector.
#' @param n The number of prior periods to find. Defaults to 1.
#' @param len_hist The length of history to use in months. Ensures that the matching release dates are not too old. Defaults to 18 months.
#'
#' @return A dataframe that enables fuzzy date join.
#'@import tidyr
#'
#' @examples
#' library(dplyr)
#' #data with release dates
#' df <- lfs_state |>
#'      group_by(anzsco1_code, date) |>
#'      summarise(emp = sum(emp), .groups = 'drop')
#'
#' # extraction dates
#' e_dt <- as.Date(c("2016-08-09", "2021-08-10"))
#' # look at 4 prior quarters
#' n_qtr <- 4
#' join_dates <- fuzzy_join_dates(
#' e_dt,
#' df$date,
#' n = 3 * n_qtr,
#' len_hist = (n_qtr + 1) * 12
#' )
#' @export
fuzzy_join_dates <- function(extraction_date, release_date, n = 1, len_hist = 18) {

  # keep series original name
  e_date_name <-  sub('.*\\$', '', substitute(extraction_date))[length(sub('.*\\$', '',substitute(extraction_date)))]
  r_date_name <- sub('.*\\$', '', substitute(release_date))[length(sub('.*\\$', '',substitute(release_date)))]
  org_names <- c(e_date_name, r_date_name)

  # extract unique dates
  extraction_date <- unique(extraction_date)
  release_date <- unique(release_date)

  # Find the corresponding dates to join on
  df <- crossing(extraction_date, release_date) %>%
    filter(
      # Release must be prior to extraction
      extraction_date >= release_date,
      # Make sure that the matching release dates are not too old
      (extraction_date - release_date) <= months(len_hist, abbreviate = FALSE)
    ) %>%
    # Find the n matching dates
    group_by(extraction_date) %>%
    slice_max(release_date, n = n) %>%
    mutate(prior_period = row_number()) %>%
    ungroup()

  names(df)[1:2] <- org_names
  return(df)
}
