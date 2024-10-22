% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/worldprod.R
\docType{data}
\name{worldprod}
\alias{worldprod}
\title{Data on world production.}
\format{
The dataset is from the World Bank STARS database and has been used in Kumbhakar et al.(2014):
\describe{
\item{country_name}{Country name}
\item{country_id}{Country identification}
\item{year}{Year identification}
\item{gdp}{GDP in 1987 U.S.dollars}
\item{capital}{Physical capital stock in 1987 U.S. dollars}
\item{labour}{Labour (number of individuals in the workforce between the age of15 and 64)}
\item{h_capital}{Human capital adjusted labour}
\item{log_gdp}{Log GDP}
\item{log_capital}{Log capital}
\item{log_labour}{Log labour}
\item{log_h}{Log human capital}
\item{initstat}{Log of the initial capital to labor ratio of each country, log_capital - labour, measured at the beginning of the sample period}
}
}
\source{
\url{https://sites.google.com/site/sfbook2014/home/for-stata-v12-v13-v14}
}
\usage{
worldprod
}
\description{
This dataset provides information on production related variables for eighty-two countries over the period 1960–1987.
}
\references{
Kumbhakar, S.C., H.J. Wang, and A. Horncastle. 2014, Cambridge University Press.
}
\keyword{datasets}
