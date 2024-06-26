% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oemdlRtools.r
\name{read_oedb}
\alias{read_oedb}
\title{Read Oxford Economics Global Economic Model database files}
\usage{
read_oedb(
  db,
  mnemonic = NULL,
  sector = NULL,
  mnemonic_sector = NULL,
  type = "V",
  model_dir = "C:/OEF",
  start_year = 1980,
  end_year = 2060,
  verbose = TRUE,
  as_xts = TRUE,
  fix_call = TRUE
)
}
\arguments{
\item{db}{Character. Filename of the database file to be imported.}

\item{mnemonic}{Character. One or more mnemonics (variable names) to export.}

\item{sector}{Character. One or more sectors (country names) to export.}

\item{mnemonic_sector}{Data frame with two columns, "Mnemonic" and "Sector". #' These specify custom "Mnemonic"-"Sector" combinations}

\item{type}{Character. Type of values to be exported: (\code{V}) for variable
data (default), \code{R} for residual data.}

\item{model_dir}{Character. Model directory (default: C:/OEF).}

\item{start_year}{Numeric. The first year for which data should be exported
(default: 1980).}

\item{end_year}{Numeric. The last year for which data should be exported
(default: 2050).}

\item{verbose}{Logical.If \code{TRUE} (default), status messages are printed.}

\item{as_xts}{Logical. If \code{TRUE} (default), data is returned in xts
format.}
}
\value{
A list containing the data (\code{$dat}), a header with meta data
(\code{$dat_head}), fix metadata (\code{$fix}), variable metadata
(\code{$var}) including a logical \code{$var$Is.percent} value indicating if
a variable is a percentage value, and a character (\code{type}) storing the #' type of data that has been exported (variable or residual data). Note that
mnemonics are converted to syntactically valid R names.
}
\description{
Reads an Oxford Economics Global Economic Model database file (.db) into R.
}
\seealso{
\code{\link{oe_macromappings}}
}
