% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trail_sum.R
\name{trail_sum}
\alias{trail_sum}
\title{trail_sum}
\usage{
trail_sum(x, p = 4, ivars = unique(x$variable))
}
\arguments{
\item{x}{Your data in long format. Data in 3 columns (Dates, variable and value)}

\item{p}{Number of periods to calculate trailing sum over (defaults to 4)}

\item{ivars}{The variables you want to transform. Defaults to transforming all variables.}
}
\value{
Transformed data, still in long format
}
\description{
Calculate a trailing sum. Useful for calculating annual totals.
}
