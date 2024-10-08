% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nth_weekday_in_mth.R
\name{nth_weekday_in_mth}
\alias{nth_weekday_in_mth}
\title{Finds the nth occurence of a particular weekday in a given month}
\usage{
nth_weekday_in_mth(yr, mth, n, wkday)
}
\arguments{
\item{yr}{The year to use. E.g. "2020"}

\item{mth}{The month to use as an integer. E.g. "10" for October}

\item{n}{The occurrence to return. E.g. "1" for the first occurrence, "-2" for the second last occurrence}

\item{wkday}{The day of the week in abbreviated 3 letter form. E.g. "Fri" for Friday}
}
\value{
The date that is the nth occurrence of the weekday in the given month and year
}
\description{
Finds the nth occurence of a particular weekday in a given month
}
\examples{

nth_weekday_in_mth (2024,8,1,"Mon")
}
