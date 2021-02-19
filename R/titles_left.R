#' titles_left
#' 
#' Flushes titles to the left of chart
#' 
#'@export
titles_left <- function(x){
  x <- ggplotGrob(x)
  x$layout$l[x$layout$name %in% c("title","subtitle")] <- 1 #"caption"
  
  x$layout$clip[x$layout$name=="panel"] <- "off"
  x <- as.ggplot(x)
}