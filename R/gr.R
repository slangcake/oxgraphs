#' gr
#' 
#' Calculates growth rates
#' @param x Your data
#' @param p Number of periods to calculate growth rate over
#' @return Your data in growth rates
#' @export
gr <- function(x,p){
  (x/lag(x,p)-1)*100}