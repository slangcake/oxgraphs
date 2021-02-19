#'y_avg_growth
#'
#'Calculates year average growth rates 
#'@param x Your data in long format. Data in 3 columns (Dates, variable and value)
#'@param p Number of periods required to calculate year average growth. Set to 4 with quarterly data (default), or 12 for monthly data
#'@param ivars The variables you want to transform. Defaults to transforming all variables. 
#'@return Transformed data, still in long format
#'@export
y_avg_growth <- function(x,p=4,ivars=unique(x$variable)) {
  #INPUTS
  #x:   Data frame in long format
  #ivars: variables in the data frame to be transformed (defaults to all)
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>%
      mutate(value = case_when(!is.na(value)~runmean(value,p,endrule="NA",align="right"))) %>%
      mutate(value = gr(value,p))
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}