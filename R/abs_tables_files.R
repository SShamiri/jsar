#' Tidy list into a data frame
#'
#' This is a helper function to `clean_abs_raw`
#' @param lst list object from `abs_tables_files`
#'
#' @return Clean data frame
#' @importFrom readxl excel_sheets read_excel
#' @importFrom dplyr filter "%>%"
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map set_names
#'
tidy_abs_tables <- function(lst){
  # global binding for cran
  value <- data_item_description <- NULL

  lst_df <- purrr::map2(lst$data, lst$title, ~cbind(.x, tbl_title = .y))
  lst_df <- purrr::map2(lst_df, lst$sheet, ~cbind(.x, sheet = .y))
  lst_out <- purrr::map(.x = lst_df,
                 .f = tidyr::pivot_longer,
                 cols = !c(1, 'tbl_title','sheet'),
                 names_to = 'date',
                 values_to = 'value'
  )
  out <- dplyr::bind_rows(lst_out) |>
    mutate(  date = suppressWarnings(as.numeric(date)),
             date = as.Date(date, origin = "1899-12-30"),
             value = as.numeric(value)
    ) |>
    rename(data_item_description = 1) |>
    relocate(date) |>
    relocate(value, .after = data_item_description)

  return(out)
}

#'  Extract data form abs time series sheets
#'
#'This function only work's with data in `Table 1, Table 2, ...` sheets. This is a helper function to `clean_abs_raw`
#'
#' @param file Name and path of input file
#'
#' @return list of data frames
abs_tables_files <- function(file){

  sheets <- readxl::excel_sheets(path = file)
  if(!'Contents' %in% sheets) {
    stop("This file is not in a standard format. sheet 'Contents' is missing")
  }

  sheets <- sheets[!sheets %in%  c("Index", "Inquiries", "Enquiries", "Contents", "Explanatory notes")]
  # get table title
  tbl_title <- purrr::map(
    .x = sheets,
    .f = readxl::read_excel,
    path = file,
    trim_ws = TRUE,
    .name_repair = "minimal",
    range = "A4:A4",
    col_names = "tbl_title"
  )
  # get data
  data_sheets <- purrr::map(
    .x = sheets,
    .f = readxl::read_excel,
    path = file,
    trim_ws = TRUE,
    .name_repair = "minimal",
    skip = 4
  )
  # simplify list out put
  #out <- list_flatten(list(data = data_sheets, title = tbl_title))
  out <- list(data = data_sheets, title = tbl_title, sheet = as.list(sheets))
  return(out)

}




