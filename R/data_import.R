#' data_import
#'
#' Loads in data from various sources. Transforms data into 'long' format, ready for use with oxford graph functions
#' @param data_source Can only take on 4 values: Excel, Oxford, Haver or ABS. 
#' \itemize{
#' \item Excel - user specified files - not very robust, data need to be in a very neat format with only one row for variable names.
#' \item Oxford - used for databank downloads (useful for plotting our forecasts)
#' \item Haver - pulls in data directly from Haver
#' \item ABS - uses the read_abs package to download series directly from abs.gov.au. Note - this saves all tables downloaded, so not recommended if data are available from Haver.
#' }
#' @param read_path:             If the file you're reading is not in the current working directory (defaults to working directory)
#' @param xl_file (reqd for excel sources): Name of excel file you're reading
#' @param xl_sheet:                         Name of excel sheet to be read (defaults to "Quarterly" for use with databank downloads)
#' @param hvr_start (reqd for Haver):       Start date for Haver data
#' @param hvr_end (reqd for Haver):         End date for Haver data
#' @param hvr_codes (reqd for Haver):       Haver series codes
#' @param hvr_fqcy: (defaults to Qtrly)     Frequency of Haver data.  Accepts "Monthly", "m" or "M" for monthly data. Accepts "Ann","Annual","A","Yearly","Y","a","y" for annual data.
#' If you request mixed frequencies, Haver aggregates data to that of the lowest frequency. I.e., if you ask for a quarterly and monthly series, the query is returned at a quarterly frequency.
#' @param hvr_db:                 HAVER database (defaults to Australia New Zealand "anz")
#' @param abs_cat_no: String. ABS catalogue number to import. Must specify full number with extension as a string.
#' @param abs_table:  Numeric, string. ABS Table to be imported. Must be specified along with abs_cat_no. Can be specified as numeric or string (must be string for alhanumeric tables i.e. '5a').
#' @param abs_series: String. Series name of ABS series you want to import.
#' @return Data frame in 'long' format with column headers: Dates, variable, value
#' @examples 
#' \donttest{data_import("Haver",hvr_start="1990",hvr_end="2019",hvr_codes=c("NSSELE","VISELE"),hvr_db = "anzr")}
#' 
#' \donttest{data_import("Oxford",xl_file="GEM_data.xlsx")}
#' \donttest{data_import('ABS',abs_cat_no = '6345.0')}
#' \donttest{data_import('ABS',abs_cat_no = '6345.0',abs_table = c(1,'5a'))}
#' \donttest{data_import('ABS',abs_series = c('A84423043C','A84423046K'))}
#' @export

data_import <- function(data_source,path=getwd(),read_path=path,xl_file=NULL,xl_sheet="Quarterly",
                        hvr_start=NULL,hvr_end=NULL,hvr_codes=NULL,hvr_fqcy="Qtrly",hvr_db="anz",
                        ox_fqcy='Quarterly',abs_cat_no=NULL,abs_series=NULL,abs_table=NULL){
  
  if(data_source=="Excel"){
    #User specified Excel files (non-Oxford databank or model excel files)
    
    if(is.null(xl_file)){stop("You need to specify which excel file you want to use")}
    
    f <- paste0(read_path,"/",xl_file)
    data <- read_excel(f,sheet = xl_sheet)
    data$Dates <- as.Date(data$Dates)
    
    a <- select(data,Dates,everything()) %>%
      melt(.,"Dates")
    a$value <- as.numeric(a$value)
    
  }else if(data_source=="Haver"){
    
    if(is.null(hvr_start) | is.null(hvr_end) |is.null(hvr_codes)){stop("You haven't provided to requisite Haver info")}
    
    hvr_start <- paste0(hvr_start,"-1-1")
    hvr_end <- paste0(hvr_end,"-12-31")
    
    data <- as.data.frame(haver.data(hvr_codes,hvr_db,sta=as.Date(hvr_start),end=as.Date(hvr_end),aggmode="Relaxed"))
    
    #Change date format
    data$Dates <- row.names(data)
    row.names(data) <- NULL
    if(hvr_fqcy=="Qtrly"){
      
      x1 <- c("-Q1","-Q2","-Q3","-Q4")
      x2 <- c("/3/1","/6/1","/9/1","/12/1")
      
      for(i in seq(1,length(x1))){data$Dates <- gsub(x1[i],x2[i],data$Dates)}
      
    }else if(hvr_fqcy=="Monthly" || hvr_fqcy=="m" || hvr_fqcy=="M"){
      
      x1 <- c("-Jan","-Feb","-Mar","-Apr","-May","-Jun","-Jul","-Aug","-Sep","-Oct","-Nov","-Dec")
      x2 <- NULL
      for(i in seq(1,12)){
        x2[i] <- paste0("/",i,"/1")
      }
      for(i in seq(1,length(x1))){data$Dates <- gsub(x1[i],x2[i],data$Dates)}
    }
    
    if(hvr_fqcy=="Ann" || hvr_fqcy=="Annual" || hvr_fqcy=="A" || hvr_fqcy=="Yearly" || hvr_fqcy=="Y" || hvr_fqcy=="a" || hvr_fqcy=="y"){
      data$Dates <- paste0(data$Dates,"/6/30")
    }
    
    data$Dates <- as.Date(data$Dates)
    
    a <- select(data,everything()) %>%
      melt(.,"Dates")
    
  }else if(data_source=="Oxford"){
    #OXFORD MODEL/DATABANK
    
    f <- paste0(read_path,"/",xl_file)
    
    if(ox_fqcy == "Quarterly"){
      data <- read_excel(f,sheet = xl_sheet) %>%
        gather(ends_with("Q1"),ends_with("Q2"),ends_with("Q3"),ends_with("Q4"),key='Dates',value='value')
    }else{
      data <- read_excel(f, sheet = xl_sheet) %>% 
        gather(matches('\\d'), key = "Dates", value = "value")
    }
    
    if("Indicator Code" %in% colnames(data)){
      data <- select(data,'Dates','value','Indicator Code',everything())
      names(data)[names(data) == 'Indicator Code'] <- 'variable'
      
      if(ox_fqcy == "Quarterly"){
        data$Dates <- gsub("Q1","/3/1",data$Dates) %>% gsub("Q2","/6/1",.) %>% gsub("Q3","/9/1",.) %>% gsub("Q4","/12/1",.) %>% as.Date(.)  
      }else {data$Dates <- paste0(data$Dates, "/12/1") %>% as.Date(.)}
      
    }else{
      data <- select(data,'Dates','value','Indicator code',everything())
      names(data)[names(data) == 'Indicator code'] <- 'variable'
      if(ox_fqcy == "Quarterly"){data$Dates <- gsub(" Q1","/3/1",data$Dates) %>% gsub(" Q2","/6/1",.) %>% 
        gsub(" Q3","/9/1",.) %>% gsub(" Q4","/12/1",.) %>% as.Date(.)
      }else{data$Dates <- paste0(data$Dates, "/12/1") %>% as.Date(.)}
    }
    a <- arrange(data,variable,Dates)
    a$value <- as.numeric(a$value)
  }else if(data_source=="ABS"){
    
    if (is.null(abs_cat_no) & is.null(abs_series)) {
      stop("Specify either an ABS catalogue number, such as '6202.0' or '6401.0', or an ABS time series ID like 'A84423127L'.")
    }
    if (!is.null(abs_cat_no) & !is.null(abs_series)) {
      stop("Please specify either the abs_cat_no OR the abs_series, not both.")
    }
    
    if(is.null(abs_cat_no)){
      a <- read_abs(series_id=abs_series)
    }else if(!is.null(abs_cat_no)){
      if(is.null(abs_table)){
        a <- read_abs(cat_no=abs_cat_no)  
      }else{
        a <- read_abs(cat_no=abs_cat_no,tables=abs_table)}
    }
    
    colnames(a)[which(colnames(a)=='date')] <- 'Dates'
    colnames(a)[which(colnames(a)=='series')] <- 'variable'
    
  }
  else{stop("You've entered a rogue Data source")}
  return(a)
}