
#' Separate ABS data items
#' @description
#' Separate a character column into multiple columns
#' @param data data frame
#' @param series_name column series to expand
#' @param column_names Optional, names of new column series
#' @param remove_totals Optional, remove totals
#' @param remove_nas Optional, remove missing values
#'
#'@importFrom stringr str_squish
#' @export
#'
#' @examples
#' \dontrun{
#' # Read ABS file with data saved in Data 1 sheet
#' df <- clean_abs_raw("./path_to/6202001.xlsx")
#' df |>
#' separate_series("data_item_description")
#' }
separate_series <- function(data,
                            series_name = "series",
                            column_names = NULL,
                            remove_totals = FALSE,
                            remove_nas = FALSE) {
  # satisfy R CMD check's "no visible binding" error
  series <- NULL
  original_series <- NULL
  # Create copy of series column to restore later
  data <- mutate(data,
                 original_series = series
  )
  # Minor data cleaning of series column
  data <- mutate(data,
                 # Extract everything before trailing
                 series = eval(parse(text = series_name)),
                 series = regmatches(
                   series,
                   regexpr(".+(?= ;)", series, perl = TRUE)
                 ),
                 series = gsub(
                   pattern = ">", series,
                   replacement = "", perl = TRUE
                 ),
                 series = stringr::str_squish(series)
  )

  # Filter totals if specified
  if (remove_totals) {
    data <- filter(
      data,
      !grepl("total", series, ignore.case = T, perl = TRUE)
    )
  }
  # Determine number of ; separators in series column
  n_seps <- max(stringi::stri_count_fixed(data$series, ";"))

  # Determine number of columns to split series into
  n_columns <- n_seps + 1

  # Create new column names if not specified
  if (is.null(column_names)) {
    column_names <- paste("series", 1:n_columns, sep = "_")
  }

  # Separate columns
  data_separated <- tidyr::separate(data, series,
                             into = column_names,
                             sep = ";",
                             remove = FALSE,
                             fill = "left"
  ) %>%
    mutate_at(.vars = vars(column_names), stringr::str_squish)

  # check for columns with NAs
  na_columns <- colnames(data_separated)[colSums(is.na(data_separated)) > 0]

  if (length(na_columns) > 0 & !remove_nas) {
    warning(paste0(na_columns, collapse = ", "),
            " column(s) have NA values.",
            call. = TRUE
    )
  }

  # filter NAs in new series columns
  if (remove_nas & length(na_columns) > 0) {
    remove_na_fn <- function(df, column) {
      col_sym <- dplyr::sym(column)
      df %>%
        filter(!is.na(!!col_sym))
    }

    data_separated <- purrr::map_dfr(
      .x = na_columns,
      .f = remove_na_fn,
      df = data_separated
    )

    message("Rows with NAs in separated series column(s) have been removed.")
  }

  # Replace original series column
  data_separated <- mutate(data_separated,
                           series = original_series
  )

  data_separated <- select(
    data_separated,
    -all_of(original_series)
  )

  return(data_separated)
}
