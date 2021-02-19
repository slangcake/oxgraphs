#' hist_end
#'
#' A function that identifies where forecast lines should go for series from the Oxford model
#' @param x A data frame generated using the data_import function and the 'Oxford' option
#' @return h_end A dataframe containing the last date of historical data for series from an Oxford databank. 
#'@export

hist_end <- function(x){
  if(('Historical end quarter' %in% colnames(x)) | ('Historical end year' %in% colnames(x)) | ('Historical end' %in% colnames(x))){}else{stop("You do not have all the necessary columns from the Oxford databank. Have you altered the data since the data_import call?")}
  
  if(('Historical end quarter' %in% colnames(x)) & ('Historical end year' %in% colnames(x))){ 
    x$heq <- x$`Historical end quarter`
  }else{
    x$heq <- str_sub(x$'Historical end',-1)
    x$`Historical end year` <- str_sub(x$'Historical end',1,4)}
  
  for(i in seq(1,4)){x$heq[x$heq==i] <- paste0("/",i*3,"/1")}
  x$hist_end <- paste0(x$`Historical end year`,x$heq) %>% as.Date(.)
  x <- x %>% select(.,variable,hist_end) %>% group_by(variable) %>% filter(row_number(hist_end)==1)
  
  xr <- x
  xr$variable <- paste0(xr$variable,' (RHS)')
  x <- rbind(x,xr)
  return(x)
}