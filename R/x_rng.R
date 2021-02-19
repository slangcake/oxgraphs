#' x_rng
#' 
#' Sets the x axis range to the start and end of the years specified.
#' 
#'@export
x_rng <- function(x,d,FY=0,bar=0){
  
  d_s <- drop_na(d)
  if (FY == 0) {
    m <- paste0(x[1], "-1-1")
    m2 <- paste0(x[2], "-12-31")
    
    
    if (bar == 1) {
      xs <- as.Date(m)
      xf <- d_s$Dates[d_s$Dates <=m2] %>% max(.) 
      xf <- xf%m+%months(1)%m-%days(1) %>% as.Date(.)
    } else {
      xs <- d$Dates[d$Dates >= m] %>% min(.) %>% as.Date(.) 
      xf <- d_s$Dates[d_s$Dates <=m2] %>% max(.) %>% as.Date(.)}
  }
  else if (FY == 1) {
    x1 <- x[1] - 1
    m <- paste0(x1, "-7-1")
    m2 <- paste0(x[2], "-6-30")
    if (bar == 1) {
      xs <- as.Date(m)
      xf <- d_s$Dates[d_s$Dates <=m2] %>% max(.) 
      xf <- xf%m+%months(1)%m-%days(1) %>% as.Date(.)
    }
    else {
      xs <- d$Dates[d$Dates >= m] %>% min(.) %>% as.Date(.)
      xf <- d_s$Dates[d_s$Dates <=m2] %>% max(.) %>% as.Date(.)
    }
  }
  x_rng <- c(xs, xf)

#   if(FY==0){
#     m <- paste0(x[1],"-1-1")
#     if(bar==1){xs <- as.Date(m)} else{
# 	xs <- d$Dates[d$Dates>=m] %>%
#       	min(.) %>%
#       	as.Date(.)}
#     xf <- as.Date(paste0(x[2],"-12-31"))} else
#       
#   if(FY==1){
#     x1 <- x[1]-1
#     m <- paste0(x1,"-7-1")
#     if(bar==1){xs <- as.Date(m)} else{
# 	  xs <- d$Dates[d$Dates>=m] %>%
#           min(.) %>%
#           as.Date(.)}
#     xf <- as.Date(paste0(x[2],"-6-30"))}
# 
#   x_rng <- c(xs,xf)
}