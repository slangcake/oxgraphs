#'l2w
#'
#'Converts long data to wide format. Assumes long data has columns named variable and value
#'@param x Your data in wide format.
#'@return Data in wide format
#'@export

l2w <- function(x){
  y <- spread(x,variable,value)
  return(y)}
