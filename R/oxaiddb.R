#'oxaiddb
#'
#'Loads in an AID database.
#'@param db The database to load
#'@param sect Character. The sector(s) you want to download variables for. Defaults to all
#'@param vars Character. The variables to download. Defaults to all variables. Or use a custom selection.
#'@param mod_dir Directory where your model software lives. Defaults to 'C:/AID'
#'@return List with two objects: the data in long format ready for use with oxgraphs package & the h_end vector for automating forecast lines.
#'These objects need to be renamed after you run this function
#' @examples
#' \donttest{x <- oxaiddb('Oct19.db') data <- x[[1]] h_end <- x[[2]]}
#'@export

oxaiddb <- function(db,sect=NULL,vars=NULL,mod_dir='C:/AID'){

  a <- read_oedb(db,sector=sect,mnemonic=vars,model_dir=mod_dir,as_xts=0,fix_call=FALSE)

  data <- a$dat
  colnames(data)[1] = 'Dates'
  data <- data %>% melt(.,'Dates')

  month(data$Dates) <- month(data$Dates)+2
  data$variable <- .revert_names(data$variable)

  b <- a$last_hist %>% as.data.frame()
  colnames(b) <- 'hist_end'
  b$variable <- names(a$last_hist) %>% .revert_names()
  b$hist_end <- data$Dates[b$hist_end]
  rownames(b) <- c()
  # month(b$hist_end) <- month(b$hist_end)+2
  br <- b
  br$variable <- paste0(br$variable, " (RHS)")
  b <- rbind(b, br)

  h_end <- select(b,variable,hist_end)
  x <- list(data,h_end)

  return(x)

}



