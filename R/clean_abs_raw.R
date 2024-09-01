#' Clean's ABS timeseries sheets
#'@description
#'This function tidy up ABS excel files and returns a clean data frame.
#' For this function to work the data should be a timeseries and saved in sheets called either Data or Table
#' @param file  Name and path of input file
#'
#' @return clean data frame
#' @export
#'
#' @examples
#' \dontrun{
#' # Read ABS file with data saved in Data 1 sheet
#' df <- clean_abs_raw("./path_to/EQ08.xlsx")
#' }

clean_abs_raw <- function(file){
  # global binding for cran
  #abs_indexed_file <- tidy_abs_indexed <- abs_tables_file <- tidy_abs_tables
  sheets <- readxl::excel_sheets(path = file)
  # file includes Index
  if('Index' %in% sheets) {
    dat = abs_indexed_files(file)
    out <- tidy_abs_indexed(dat)
    return(out)
  }
  # file includes Table 1, Table 2, ....
  else if(all('Contents' %in% sheets & !grepl('Data', sheets, ignore.case = T))) {
    dat = abs_tables_files(file)
    out <- tidy_abs_tables(dat)
    return(out)
  }
  # file includes Table 1, Data, ....
  else if(all('Contents' %in% sheets & any(grepl('Data 1', sheets, ignore.case = T)))) {
    dat = abs_data_files(file)
    out <- tidy_abs_data(dat)
    return(out)
  }
  else warning(paste(file , "is not a supported ABS standard format"))

}
