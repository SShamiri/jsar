#' Finds the nth occurence of a particular weekday in a given month
#'
#' @param yr The year to use. E.g. "2020"
#' @param mth The month to use as an integer. E.g. "10" for October
#' @param n The occurrence to return. E.g. "1" for the first occurrence, "-2" for the second last occurrence
#' @param wkday The day of the week in abbreviated 3 letter form. E.g. "Fri" for Friday
#'
#' @return The date that is the nth occurrence of the weekday in the given month and year
#' @importFrom dplyr between
#' @export
#'
#' @examples
#'
#' nth_weekday_in_mth (2024,8,1,"Mon")
nth_weekday_in_mth <- function(yr, mth, n, wkday) {
  #binding variables to NULL
  weekday <- NULL
  # numeric years > 0
  if(!is.numeric(yr) | yr < 1) stop("year must be numeric and > 0")
  # numeric month
  if(!is.numeric(mth) | !(mth >= 1 & mth <=12)) stop("months must be numeric and between 1 & 12")
  # numeric n
  if(!is.numeric(n) | !(n >= -5 & n <= 5)) stop("n must be numeric and between -5 & 5")
  # weekday character
  if(!is.character(wkday) | !wkday %in% c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) stop("weekdays must be character 'Mon-Sun'")

  # Create a series of every day in the month
  dates <- seq(from = as.Date(paste(yr, mth, "01", sep = "-")),
               to = as.Date(paste(yr, mth, days_in_month(mth), sep = "-")),
               by = 1)

  # Search for the days of the week
  df <- data.frame(date = dates, weekday = wday(dates, label = TRUE)) %>%
    filter(weekday == wkday)

  # Reverse order for counting backwards
  if (n < 0) {
    df <- df %>%
      arrange(desc(date))
  }

  # Find the occurrence
  df %>%
    filter(row_number() == abs(n)) %>%
    pull(date)

}
