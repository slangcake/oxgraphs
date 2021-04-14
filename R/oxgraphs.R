#' oxgraphs: A package for drawing graphs in the oxford house style
#'
#' The oxgraph package provides three categories of functions:
#' data import, data transformation and graphing. \cr
#' In addition, the \code{\link{ox_setup}} function loads in the required packages and should be run in the preamble of any script.
#' @section Data Import: Data coming from excel or Haver are handled by the data_import function
#' \itemize{\item \code{\link{data_import}}}
#' \cr
#' OE databases can also be directly imported using \itemize{
#' \item \code{\link{oxoedb}} for GEM databases (defaults to importing Australian series only)
#' \item \code{\link{oxaiddb}} for AID databases
#' \item \code{\link{oxgimdb}} for GIM databases }
#' @section Data Transformation: A series of functions to perform data transformations. \cr
#' These functions are set up to accomodate data in 'long' format.
#' \itemize{
#' \item \code{\link{growth}}
#' \item \code{\link{ctg}}
#' \item \code{\link{trail_avg}}
#' \item \code{\link{trail_sum}}
#' \item \code{\link{y_avg_growth}}
#' \item \code{\link{idx}}
#' \item \code{\link{difference}}
#' \item \code{\link{cagr}}
#' }
#' These will switch data quickly from wide-to-long format and vice-versa.
#' \itemize{
#' \item \code{\link{w2l}}
#' \item \code{\link{l2w}}
#' }
#' @section graphing: each graph style (i.e. line, bar) is contained within a separate function.
#' There are some compulsory arguments, and other optional settings
#' \itemize{
#' \item \code{\link{ox_line_graph}}
#' \item \code{\link{ox_dated_bar_graph}}
#' \item \code{\link{ox_dated_bar_line_graph}}
#' \item \code{\link{ox_dated_bar_line_point_graph}}
#' \item \code{\link{ox_area_graph}}
#' \item \code{\link{ox_scatter_graph}}
#' \item \code{\link{ox_column_graph}}
#' \item \code{\link{ox_pie_graph}}}
#' \cr
#' There is also a function for automating forecast lines when data are imported from a .db file
#' \itemize{\item \code{\link{hist_end}}}
#' Function for quickly saving .png files in correct dimensions
#' \itemize{\item \code{\link{ox_save}}}
#' @docType package
#' @name oxgraphs
NULL
