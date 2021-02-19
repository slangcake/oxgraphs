#'trail_avg
#'
#'Calculates a trailing average 
#'@param x Your data in long format. Data in 3 columns (Dates, variable and value)
#'@param p Number of periods 
#'@param ivars The variables you want to transform. Defaults to transforming all variables. 
#'@return Transformed data, still in long format
#'@export

trail_avg <- function(x,p=4,ivars=unique(x$variable)){
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>% mutate(value = case_when(!is.na(value)~runmean(value,p,endrule="NA",align="right")))
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}