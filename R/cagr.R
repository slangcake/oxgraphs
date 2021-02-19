#'y_avg_growth
#'
#'Calculates compound annual growth rates. Data must be at quareterly frequency
#'@param x Your data in long format. Data in 3 columns (Dates, variable and value)
#'@param p Number of periods required to calculate compound annual growth rate. 
#'@param ivars The variables you want to transform. Defaults to transforming all variables. 
#'@return Transformed data, still in long format
#'@export
cagr <- function(x,p=5,ivars=unique(x$variable)) {
    for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>%
      mutate(value = case_when(!is.na(value)~runmean(value,4,endrule="NA",align="right"))) %>%
      mutate(value = ((value/lag(value,p*4))^(1/p)-1)*100)
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}