#'oxgimdb
#'
#'Loads in a Global industry model (GIM) database.
#'@param db The database to load
#'@param sect Character. The sector(s) you want to download variables for. Defaults to Australia
#'@param vars Character. The variables to download. Defaults to all variables. Or use a custom selection.
#'@param mod_dir Directory where your model software lives. Defaults to 'C:/GIM'
#'@param var_res Option to import variable values ('V' - default) or residuals ('R')
#'@return List with two objects: the data in long format ready for use with oxgraphs package & the h_end vector for automating forecast lines.
#'These objects need to be renamed after you run this function
#' @examples
#' \donttest{x <- oxgimdb('Oct19.db') data <- x[[1]] h_end <- x[[2]]}
#'@export

oxgimdb <- function(db,mod_dir='C:/GIM',vars=NULL,sect='AUSTRALI',var_res = 'V'){

  a <- read_oedb(db,model_dir=mod_dir,
                 sector=sect,
                 mnemonic = vars,
                 as_xts=0,
                 fix_call=FALSE,
                 type = var_res)

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



