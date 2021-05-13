#'oxoedb
#'
#'Loads in variables from a GEM OE database. Only brings in Australian variables by default.
#'@param db The database to load
#'@param vars Character. The variables to download. Defaults to all variables. Or use a custom selection.
#'@param sect Character. The country/countries you want to download variables for. Defaults to Australia. Set to NULL to import all sectors
#'@param mod_dir Directory where your model software lives. Defaults to 'C:/OEF'
#'@return List with two objects: the data in long format ready for use with oxgraphs package & the h_end vector for automating forecast lines.
#'These objects need to be renamed after you run this function
#' @examples
#' \donttest{x <- oxoedb('Oct19.db') data <- x[[1]] h_end <- x[[2]]}
#'@export

oxoedb <- function(db,sect='AUS',vars=NULL,mod_dir='C:/OEF',ms=NULL){

  if(!is.null(sect)){
    if(sect=='AUS'){

      if(is.null(vars)){
        print(paste0('Importing all Australian variables from ',db))
      }else{
        print(paste0('Importing ',vars,' for Australia from ',db))
      }

      a <- read_oedb(db,sector=c('AUSTRALI','AUS_DET'),mnemonic=vars,model_dir=mod_dir,as_xts=0,fix_call=FALSE,mnemonic_sector=ms)
    }else{
      if(is.null(vars)){
        print(paste0('Importing all variables for ',sect,' from ',db))
      }else{
        print(paste0('Importing ',vars,' for ',sect,' from ',db))
      }
      a <- read_oedb(db,sector=sect,mnemonic=vars,model_dir=mod_dir,as_xts=0,fix_call=FALSE,mnemonic_sector=ms)
    }
  }else{
    if(is.null(vars)){
      print(paste0('Attempting to import all variables for all sectors from ',db))
      print('***NOT RECOMMENDED***')
    }else{
      print(paste0('Importing ',vars,' for all sectors from ',db))
    }
    a <- read_oedb(db,sector=sect,mnemonic=vars,model_dir=mod_dir,as_xts=0,fix_call=FALSE,mnemonic_sector=ms)
  }


  data <- a$dat
  colnames(data)[1] = 'Dates'
  data <- data %>% melt(.,'Dates')

  if(!is.null(sect)){
    if(sect=='AUS'){data$variable <- data$variable %>% gsub("_AUSTRALI","",.)%>% gsub("_AUS_DET","",.)
    }
  }

  month(data$Dates) <- month(data$Dates)+2
  data$variable <- .revert_names(data$variable)

  b <- a$last_hist %>% as.data.frame()
  colnames(b) <- 'hist_end'
  b$variable <- names(a$last_hist) %>% .revert_names()
  b$hist_end <- data$Dates[b$hist_end]

  if(!is.null(sect)){
    if(sect=='AUS'){b$variable <- b$variable %>% gsub("_AUSTRALI","",.)%>% gsub("_AUS_DET","",.)
    }
  }
  rownames(b) <- c()
  # month(b$hist_end) <- month(b$hist_end)+2
  br <- b
  br$variable <- paste0(br$variable, " (RHS)")
  b <- rbind(b, br)

  h_end <- select(b,variable,hist_end)

  is_pct <- a$var$Indicator %>% .revert_names() %>% gsub("_AUSTRALI","",.)%>% gsub("_AUS_DET","",.) %>% as.data.frame()
  is_pct$is_pct <- a$var$Is.percent
  colnames(is_pct)[1] <- 'variable'


  x <- list(data,h_end,is_pct)
  return(x)
}
