#'idx
#'
#'Rebases data to your desired period and level 
#'@param x Your data in long format. Data in 3 columns (Dates, variable and value)
#'@param base Base year for indexing
#'@param FY Binary for financial years (defaults to CY)
#'@param ivars The variables you want to transform. Defaults to transforming all variables. 
#'@param i_level Level to set index to in base year (defaults to 100)
#'@return Transformed data, still in long format
#'@export

idx <- function(x,base,FY=0,ivars=unique(x$variable),i_level=100){
    if(FY==1)(dte_end <- as.Date(paste0(base,'-6-1'))) else
    if(FY==0)(dte_end <- as.Date(paste0(base,'-12-1')))
  
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>%
      mutate(value=value/mean(value[(which(b$Dates==dte_end)-3):which(b$Dates==dte_end)])*i_level)
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}