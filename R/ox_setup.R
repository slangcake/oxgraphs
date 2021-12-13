#' ox_setup
#'
#' A general setup function that clears the console and workspace and loads in the packages you need to create Oxford style graphs
#' @param path Defaults to the current working directory
#'@export
ox_setup <- function(){

  rm(list = ls()[ls()!="path"])   # clear workspace - with the exception of the working directory
  gc()              # garbage collection
  cat("\014")       # Clear console
  graphics.off()    # Clear graphic plots
  Sys.setenv(TZ="Australia/Sydney")

  # Package installation

  list.of.packages <- c("dplyr","plyr","tidyr","timeDate","ggplot2","lubridate","readxl",
                        "tidyverse",'magrittr','gridExtra',
                        "leaps","tseries","urca","writexl",
                        "reshape2","cowplot","ggplotify",
                        "scales","readabs","caTools","zoo",
                        'rmarkdown','rstudioapi','extrafont')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {install.packages(new.packages)}
  invisible(lapply(list.of.packages, require, character.only = TRUE))

  if(!"Haver" %in% installed.packages()){options(install.packages.check.source = FALSE)
    install.packages("Haver", repos="http://www.haver.com/r/") }

  require("Haver")

  if(is.null(haver.path())){haver.path("auto")}

  if(!(file.exists('C:/Windows/Fonts/segoeui.ttf'))){font_import()}

  windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))
}

