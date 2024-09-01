#' Tidy list into a data frame
#' This is a helper function to `clean_abs_raw`
#'
#' @param lst list object from `abs_data_files`
#' @importFrom readxl excel_sheets read_excel
#' @importFrom dplyr bind_cols "%>%" relocate
#' @importFrom tidyr pivot_longer
#' @return Clean data frame

tidy_abs_data <- function(lst){
  # global binding for cran
  sheet <- tbl_title <- value <- NULL
  df <- dplyr::bind_cols(lst)
  out <- df |>
    tidyr::pivot_longer(!c(1:4, 'tbl_title','sheet'), names_to = 'data_item_description', values_to = 'value') |>
    # tidying up
    rename(date = 1) |>
    dplyr::relocate(c(sheet, tbl_title), .after = last_col()) |>
    dplyr::mutate(
      date = as.Date(date, origin = "1899-12-30"),
      value = as.numeric(value)
    )

  return(out)
}
#'  Extract data form abs time series sheets
#'
#'This function only work's with data in `Data 1, Data 2, ...` sheets.This is a helper function to `clean_abs_raw`
#'
#' @param file Name and path of input file
#'
#' @return list of data frames
#' @importFrom purrr list_flatten
#'
abs_data_files <- function(file){

  sheets <- readxl::excel_sheets(path = file)
  if(!('Contents' %in% sheets && 'Data 1' %in% sheets)) {
    stop("This file is not in a standard format. sheet 'Contents' & 'Data 1' is missing")
  }
  sheets <- sheets[!grepl("Table |Contents", sheets, ignore.case = T)]
  # get tilte
  tbl_title <- readxl::read_excel(file,
                                  sheet = sheets,
                                  range = "$A$2",
                                  col_names = "tbl_title"
  )
  # get data
  data_sheets <- purrr::map(
    .x = sheets,
    .f = readxl::read_excel,
    path = file,
    trim_ws = TRUE,
    skip = 3,
    .name_repair = "minimal"
  )
  # simplify list out put
  out <- purrr::list_flatten(list(data = data_sheets, title = tbl_title, sheet = as.list( sheets )))
  return(out)

}
