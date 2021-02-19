#'ctg
#'
#'Calculates contribution to growth for multiple variables. User specifies which series is the total the others are contributing to.
#'@param x Your data in long format. Data in 3 columns (Dates, variable and value)
#'@param tot 'Total' variable i.e. GDP in contribution to GDP growth calc. Must be specified.
#'@param p Number of periods 
#'@param ivars The variables you want to transform. Defaults to transforming all variables. 
#'@return Transformed data, still in long format
#'@export

ctg <- function(x,tot,p=4,ivars=unique(x$variable)){
  t <- x[which(x$variable==tot),]
  for(i in 1:length(ivars)){
    
    b <- x[which(x$variable==ivars[i]),]
    b <- b %>%
      mutate(value=(value-lag(value,p))/lag(t$value,p)*100)
    x[which(x$variable==ivars[i]),] <- b
  } 
  return(x)}