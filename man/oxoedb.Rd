% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oxoedb.R
\name{oxoedb}
\alias{oxoedb}
\title{oxoedb}
\usage{
oxoedb(
  db,
  sect = "AUS",
  vars = NULL,
  mod_dir = "C:/OEF",
  ms = NULL,
  var_res = "V",
  start = 1980,
  end = 2050
)
}
\arguments{
\item{db}{The database to load}

\item{sect}{Character. The country/countries you want to download variables for. Defaults to Australia. Set to NULL to import all sectors}

\item{vars}{Character. The variables to download. Defaults to all variables. Or use a custom selection.}

\item{mod_dir}{Directory where your model software lives. Defaults to 'C:/OEF'}

\item{var_res}{Option to import variable values ('V' - default) or residuals ('R')}

\item{start}{Start year for data import}

\item{ennd}{end year for data import}
}
\value{
List with two objects: the data in long format ready for use with oxgraphs package & the h_end vector for automating forecast lines.
These objects need to be renamed after you run this function
}
\description{
Loads in variables from a GEM OE database. Only brings in Australian variables by default.
}
\examples{
\donttest{x <- oxoedb('Oct19.db') data <- x[[1]] h_end <- x[[2]]}
}
