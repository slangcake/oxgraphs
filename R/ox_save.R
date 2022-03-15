#'ox_save
#'
#'Saves graph in png format with correct dimensions
#'@param n Title you want to give chart
#'@return Saved png file
#'@export

ox_save <- function(n){
  if(!is.character(n)){stop('Graph title needs to be in character format')}
  ttl <- paste0(n,'.png')
  ggsave(ttl, w = 9.03, h = 6.67)
}
