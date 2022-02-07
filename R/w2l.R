#'w2l
#'
#'Converts wide data to long format. Assumes you want the column 'Dates' to repeat
#'@param x Your data in wide format.
#'@param repcol The column to be repeated when moving to long format
#'@return Data in long format
#'@export

w2l <- function(x,rep_col='Dates'){
  y <- reshape2::melt(x,rep_col)
  return(y)}


