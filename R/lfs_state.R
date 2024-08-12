#' Labour Force Employment.
#'
#' Sample of Australian labour force by state and occupation.
#'
#' @format A data frame with 9,728 rows and 5 variables:
#' \describe{
#'   \item{state_name}{Australian states}
#'   \item{anzsco1_code}{occupation major group of main job (ANZSCO) code}
#'   \item{anzsco1_name}{occupation major group of main job (ANZSCO) name}
#'    \item{date}{the reference date on which this data was measured, quarterly}
#'    \item{emp}{employed in thousands}
#' }
#' @source \url{https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release#labour-force-status}
"lfs_state"
#' @example
#' lfs_state %>%
#' slice(5)
