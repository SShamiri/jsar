#' Tidy list into a data frame
#'
#' Note this function takes `abs_indexed_files` object as an input.This is a helper function to `clean_abs_raw`
#'
#' @param lst object from `abs_indexed_files`
#'
#' @return clean data frame
#' @importFrom readxl excel_sheets read_excel
#' @importFrom dplyr filter "%>%"
#' @importFrom purrr map set_names
#' @importFrom stringi stri_detect_fixed stri_replace_all_regex
#'
tidy_abs_indexed <- function(lst){
  # visible binding for cran
  X__1 <- series <- value <- unit <- series_id <- NULL

  out_lst <- list()
  for(i in seq_along(lst$data)){
    df <- lst$data[[i]]
    tbl_title <- as.character(lst$title)
    sheet <- as.character(lst$sheet[[i]])
    colnames(df)[1] <- "X__1"
    # return an error if the sheet is not formatted as we expect
    # from an ABS time series

    if (df[9, 1] != "Series ID") {
      stop(
        "The data frame appears not to be formatted as we expect",
        " from an ABS time series. There should be 9 rows of metadata",
        " after the column names (eg. 'Series ID').",
        " Please check the spreadsheet."
      )
    }
    # a bunch of columns have duplicate names which causes problems; temporarily
    # append the series ID to the colname so they're unique
    cols <- 2:ncol(df)
    new_col_names <- paste(colnames(df)[cols], df[9, cols], sep = "_")
    colnames(df)[2:ncol(df)] <- new_col_names

    df <- df %>%
      tidyr::pivot_longer(cols = !one_of("X__1"), names_to = "series") %>%
      filter(
        !is.na(X__1),
        # This filtering is necessary for cases where the ABS adds notes
        # to the bottom of the data for some reason
        !stringi::stri_detect_fixed(X__1, "Trend Break"),
        !stringi::stri_detect_fixed(X__1, "see")
      ) %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(
        series_type = value[X__1 == "Series Type"],
        data_type = value[X__1 == "Data Type"],
        collection_month = value[X__1 == "Collection Month"],
        frequency = value[X__1 == "Frequency"],
        series_id = value[X__1 == "Series ID"],
        unit = value[X__1 == "Unit"]
      ) %>%
      dplyr::filter(dplyr::row_number() >= 10) %>%
      dplyr::ungroup() |>
      dplyr::rename(date = X__1) %>%
      dplyr::mutate(
        date = suppressWarnings(as.numeric(date)),
        date = as.Date(date, origin = "1899-12-30"),
        unit = as.character(unit),
        value = as.numeric(value)
      ) %>%
      dplyr::filter(!is.na(date)) |>
      # now remove appended series ID from the end of 'series'
      dplyr::mutate(series = stringi::stri_replace_all_regex(series,
                                                             pattern = "_[^_]+$",
                                                             replacement = ""),
                    tbl_title = tbl_title,
                    sheet = sheet) |>
      # tidying up
      relocate(series_id, .after = series) |>
      rename(data_item_description = series)

    out_lst[[i]] <- df
  }

  out <- bind_rows(out_lst)
  return(out)
}
#' Extract data form abs time series sheets
#'
#'This function only work's with files containing `Index` sheet. This is a helper function to `clean_abs_raw`
#'
#' @param file Name and path of input file
#'
#' @return List of data frames
#'
abs_indexed_files <- function(file){

  sheets <- readxl::excel_sheets(path = file)
  if(!'Index' %in% sheets) {
    stop("This file is not in a standard format. sheet 'Index' is missing")
  }
  sheets <- sheets[!sheets %in% c("Inquiries", "Enquiries", "Contents", "Explanatory notes")]
  sheets <- sheets[!grepl("Table ", sheets, ignore.case = T)]
  # get table title
  tbl_title <- readxl::read_excel(file,
                                  range = "Index!B6:B6",
                                  col_names = "tbl_title"
  )
  # get data
  data_sheets <- purrr::map(
    .x = sheets[!sheets %in% 'Index'],
    .f = readxl::read_excel,
    path = file,
    trim_ws = TRUE,
    .name_repair = "minimal"
  )
  # simplify list out put
  # out <- list_flatten(list(data = data_sheets, title = tbl_title, sheet = as.list( sheets[!grepl("Index", sheets, ignore.case = T)] )))
  out <- list(data = data_sheets, title = tbl_title, sheet = as.list( sheets[!grepl("Index", sheets, ignore.case = T)] ))
  return(out)
}
