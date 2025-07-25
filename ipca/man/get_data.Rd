% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_data}
\alias{get_data}
\title{Collect and pre-process macroeconomic data}
\usage{
get_data(quiet = FALSE)
}
\arguments{
\item{quiet}{Collect data silently? The default (\code{FALSE}) prints messages to the Console.}
}
\value{
A tibble
}
\description{
It collects the IPCA time series and other economic variables of the Brazilian economy for use in forecasting models.
}
\examples{
df_macro <- get_data(quiet = TRUE)
}
