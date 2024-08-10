#' Calculates the change
#'@description
#' `calc_change` calculates the level change of a column in a dataframe over a specified amount of time,
#' `calc_change_per` calculates the percentage change of a column in a dataframe over a specified amount of time.
#'
#' @param column The column to use for calculating the change.
#' @param len The number of lags to use.
#' @param dt The date column to order the column values by.
#'
#' @return A vector containing the change values.
#' @export
#' @importFrom magrittr %>%
#' @import dplyr lubridate
#'
#' @examples
#' library(dplyr)
#'
#' lfs_state %>%
#' group_by(date) %>%
#' summarise(emp = sum(emp), .groups = 'drop') %>%
#' mutate(chng = calc_change(emp, 1, date))
calc_change <- function(column, len, dt) {
  # column is numeric
  if(!is.numeric(column)) {
    stop("column must be numeric")
  }
  # dt is date
  if(!is.Date(dt)) {
    stop("dt must be of class Date")
  }

  prev_val <- dplyr::lag(column, n = len, order_by = dt)
  change <- column - prev_val
  return(change)
}

#' @rdname calc_change
#' @export
calc_change_per <- function(column, len, dt) {
  # column is numeric
  if(!is.numeric(column)) {
    stop("column must be numeric")
  }
  # dt is date
  if(!is.Date(dt)) {
    stop("dt must be of class Date")
  }

  prev_val <- lag(column, n = len, order_by = dt)
  change <- ((column - prev_val)/prev_val)

  # Special case when current and previous value are both 0
  change[is.nan(change)] <- 0

  change
}
