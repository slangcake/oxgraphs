#'difference
#'
#'Calculates the difference between opbservations 
#'@param x Your data in long format. Data in 3 columns (Dates, variable and value)
#'@param p Number of periods. Defaults to 4
#'@param ivars The variables you want to transform. Defaults to transforming all variables. 
#'@return Transformed data, still in long format
#'@export
difference <- function(x,p=4,ivars=unique(x$variable)){
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>% mutate(value = c(rep(NA,p),diff(value,p)))
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}