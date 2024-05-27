#' Check for availability of Mdl tool on load
.onLoad <- function(libname, pkgname) {
  # Check if Mdl tools is available
  if (Sys.which("mdl") == "") {
    warning("This package requires the Oxford Economics Mdl tool to run.")
  }

  if (system("mdl", ignore.stdout = TRUE, ignore.stderr = TRUE) != 1) {
    warning("This package requires the Oxford Economics Mdl tool to run.")
  }
}

#' Function to clean identifiers by replacing special characters
.clean_names <- function(x) {
  x <- gsub("\\!", "exc", x)
  x <- gsub("\\%", "pct", x)
  x <- gsub("\\$", "usd", x)
  return(x)
}
.revert_names <- function(x) {
  x <- gsub("exc","\\!",x)
  x <- gsub("pct","\\%",x)
  x <- gsub("usd","\\$",x)
  return(x)
}

#' Function to convert Mdl date string into R Date object
.oe_date <- function(x) {
  x[!is.na(x)] <- paste0(substr(x[!is.na(x)], 1, 4), "-",
                         as.numeric(substr(x[!is.na(x)], 6, 6)) * 3 - 2, "-01")
  x <- as.Date(x)
  return(x)
}

#' Function to return Oxford Economics colour palette (discrete)
#'
#' Returns Oxford Economics colour palette. This palette includes 15 unique
#' colours.
#' @return Returns Oxford Economics colour palette.
#' @export
#' @examples
#' oe_palette()
oe_palette <- function(){
  oecols <- c(paste0("#", paste(as.hexmode(c( 0, 52, 105)), collapse = "")),
              paste0("#", paste(as.hexmode(c( 0, 173, 220)), collapse = "")),
              paste0("#", paste(as.hexmode(c(123, 124, 119)), collapse = "")),
              paste0("#", paste(as.hexmode(c(189, 27, 33)), collapse = "")),
              paste0("#", paste(as.hexmode(c(209, 162, 30)), collapse = "")),
              paste0("#", paste(as.hexmode(c( 0, 121, 63)), collapse = "")),
              paste0("#", paste(as.hexmode(c(222, 99, 40)), collapse = "")),
              paste0("#", paste(as.hexmode(c(150, 87, 147)), collapse = "")),
              paste0("#", paste(as.hexmode(c( 75, 199, 231)), collapse = "")),
              paste0("#", paste(as.hexmode(c(173, 224, 242)), collapse = "")),
              paste0("#", paste(as.hexmode(c(169, 166, 162)), collapse = "")),
              paste0("#", paste(as.hexmode(c(208, 205, 201)), collapse = "")),
              paste0("#", paste(as.hexmode(c(197, 137, 48)), collapse = "")),
              paste0("#", paste(as.hexmode(c(218, 177, 119)), collapse = "")),
              paste0("#", paste(as.hexmode(c(233, 212, 180)), collapse = "")))
  return(oecols)
}

#' Return Oxford Economics location identifiers and corresponding ISO codes
#'
#' Returns a data frame containing Oxford Economics location identifiers and
#' corresponding ISO 3-character code.
#' @return Returns a data frame containing Oxford Economics location identifiers
#'         and corresponding ISO 3-character code.
#' @export
#' @examples
#' oe_macromappings()
oe_macromappings <- function() {
  tab <- rbind(c("SAUDARNC", "P29"),
               c("SAUDEM", "P30"),
               c("SAULAB", "P31"),
               c("SAUSUBN", "P32"),
               c("SAUGROIL", "P40"),
               c("ADVANECO", "A05"),
               c("AFRICA", "AFR"),
               c("ASP", "ASIAPAC"),
               c("BRIC", "BRC"),
               c("EASTEUR", "EEU"),
               c("EMERGMAR", "A06"),
               c("EU", "EUR"),
               c("EURO_11", "EUZ"),
               c("GCC", "A18"),
               c("LATAMER", "LAT"),
               c("OPEC", "OPC"),
               c("RESTOECD", "ROD"),
               c("RESTWORL", "RWD"),
               c("WORLD", "WLD"),
               c("CANADA", "CAN"),
               c("MEXICO", "MEX"),
               c("US", "USA"),
               c("AUSTRIA", "AUT"),
               c("BELGIUM", "BEL"),
               c("DENMARK", "DNK"),
               c("FINLAND", "FIN"),
               c("FRANCE", "FRA"),
               c("GERMANY", "DEU"),
               c("GREECE", "GRC"),
               c("IRELAND", "IRL"),
               c("ITALY", "ITA"),
               c("NETH", "NLD"),
               c("NORWAY", "NOR"),
               c("PORTUGAL", "PRT"),
               c("SPAIN", "ESP"),
               c("SWEDEN", "SWE"),
               c("SWITZ", "CHE"),
               c("UK", "GBR"),
               c("RUSSIA", "RUS"),
               c("BULGARIA", "BGR"),
               c("CROATIA", "HRV"),
               c("CZECH", "CZE"),
               c("HUNGARY", "HUN"),
               c("POLAND", "POL"),
               c("ROMANIA", "ROU"),
               c("SLOVAKIA", "SVK"),
               c("TURKEY", "TUR"),
               c("AUSTRALI", "AUS"),
               c("CHINA", "CHN"),
               c("HK", "HKG"),
               c("INDIA", "IND"),
               c("INDONESI", "IDN"),
               c("JAPAN", "JPN"),
               c("MALAYSIA", "MYS"),
               c("PHILIPPI", "PHL"),
               c("SINGPORE", "SGP"),
               c("KOREA", "KOR"),
               c("TAIWAN", "TWN"),
               c("THAILAND", "THA"),
               c("ARGENTIN", "ARG"),
               c("BRAZIL", "BRA"),
               c("SAUDISR", "SAU"),
               c("UAEMOD", "ARE"),
               c("SAFRICA", "ZAF"),
               c("CYPRUS", "CYP"),
               c("ESTONIA", "EST"),
               c("LATVIA", "LVA"),
               c("LITH", "LTU"),
               c("SLOVENIA", "SVN"),
               c("VIETNAM", "VNM"),
               c("VENEZUEL", "VEN"),
               c("IRANSR", "IRN"),
               c("IRAQ_ANN", "IRQ"),
               c("KUWAIT", "KWT"),
               c("QATAR", "QAT"),
               c("ALGERIA", "DZA"),
               c("ANGOLA", "AGO"),
               c("EGYPTSR", "EGY"),
               c("MOROCCO", "MAR"),
               c("NIGERIA", "NGA"),
               c("CHILE", "CHL"),
               c("AZERBAI", "AZE"),
               c("KAZAK", "KAZ"),
               c("TURKMENI", "TKM"),
               c("UKRAINE", "UKR"),
               c("UZBEKIST", "UZB"),
               c("ALBANIA", "ALB"),
               c("BANGLAD", "BGD"),
               c("BRUNEI", "BRN"),
               c("CAMBODIA", "KHM"),
               c("MYANMAR", "MMR"),
               c("NZ_QTR", "NZL"),
               c("PAKISTAN", "PAK"),
               c("BOLIVIA", "BOL"),
               c("COLOMBIA", "COL"),
               c("ECUADOR", "ECU"),
               c("PERU", "PER"),
               c("TRIN_TOB", "TTO"),
               c("URUGUAY", "URY"),
               c("BAHRAIN", "BHR"),
               c("ISRAELSR", "ISR"),
               c("JORDANSR", "JOR"),
               c("OMAN", "OMN"),
               c("YEMEN", "YEM"),
               c("EQGUINEA", "GNQ"),
               c("LIBYA", "LBY"),
               c("TUNISIA", "TUN"),
               c("CAMEROON", "CMR"),
               c("CONGO", "COG"),
               c("COTE_DIV", "CIV"),
               c("ZAIRE", "COD"),
               c("ETHIOPIA", "ETH"),
               c("GABON", "GAB"),
               c("GHANA", "GHA"),
               c("KENYA", "KEN"),
               c("MOZAMBIQ", "MOZ"),
               c("NAMIBIA", "NAM"),
               c("SENEGAL", "SEN"),
               c("SUDAN", "SDN"),
               c("TANZANIA", "TZA"),
               c("AR_WE", "WE"),
               c("AR_CIS", "CIS"),
               c("AR_CEB", "CEB"),
               c("AR_ASIA", "ASIA"),
               c("AR_LATCA", "LATCA"),
               c("AR_ME", "ME"),
               c("AR_AFR", "RAF"))
  tab <- setNames(data.frame(tab, stringsAsFactors = FALSE),
                  nm = c("oesector", "iso3c"))
  return(tab)
}

#' Read Oxford Economics Global Economic Model database files
#'
#' Reads an Oxford Economics Global Economic Model database file (.db) into R.
#'
#' @param db Character. Filename of the database file to be imported.
#' @param mnemonic Character. One or more mnemonics (variable names) to export.
#' @param sector Character. One or more sectors (country names) to export.
#' @param mnemonic_sector Data frame with two columns, "Mnemonic" and "Sector". #' These specify custom "Mnemonic"-"Sector" combinations
#' @param type Character. Type of values to be exported: (\code{V}) for variable
#' data (default), \code{R} for residual data.
#' @param model_dir Character. Model directory (default: C:/OEF).
#' @param start_year Numeric. The first year for which data should be exported
#' (default: 1980).
#' @param end_year Numeric. The last year for which data should be exported
#' (default: 2050).
#' @param verbose Logical.If \code{TRUE} (default), status messages are printed.
#' @param as_xts Logical. If \code{TRUE} (default), data is returned in xts
#' format.
#' @return A list containing the data (\code{$dat}), a header with meta data
#' (\code{$dat_head}), fix metadata (\code{$fix}), variable metadata
#' (\code{$var}) including a logical \code{$var$Is.percent} value indicating if
#' a variable is a percentage value, and a character (\code{type}) storing the #' type of data that has been exported (variable or residual data). Note that
#' mnemonics are converted to syntactically valid R names.
#' @seealso \code{\link{oe_macromappings}}
#' @export
read_oedb <- function(db, mnemonic = NULL, sector = NULL,
                        mnemonic_sector = NULL, type = "V", model_dir = "C:/OEF",
                        start_year = 1980, end_year = 2060, verbose = TRUE,
                        as_xts = TRUE,fix_call=TRUE) {
  # Check if Mdl tool is available
  if (Sys.which("mdl") == "") {
    stop("This function requires the Oxford Economics Mdl tool to run.")
  }

  if (system("mdl", ignore.stdout = TRUE, ignore.stderr = TRUE) != 1) {
    stop("This function requires the Oxford Economics Mdl tool to run.")
  }

  # Check if database file exists
  if (!file.exists(db)) {
    stop("Database file not found.")
  }

  # Check if level/residual switch is being used correctly
  if (!is.element(type, c("V", "R"))) {
    warning("Parameter type must be 'V' for variable data or 'R' for residual
           data. Defaulting 'V'.")
    type <- "V"
  }

  # Print summary of function call
  if (verbose) {
    message("Call: db: ", db, "; mnemonic: ", mnemonic, "; sector: ", sector,
            "; mnemonic_sector: ",
            ifelse(!is.null(mnemonic_sector), "Supplied", ""),
            "; type: ", type, "; model_dir: ", model_dir, "; start_year: ",
            start_year, "; end_year: ", end_year, "; verbose: ", verbose,
            "; as_xts: ", as_xts)
  }

  # Set name of temporary CSV files for data, variable, and fix information
  var_file <- paste0(gsub("\\\\", "", tempfile(tmpdir = "")), "var.csv")
  if(fix_call){fix_file <- paste0(gsub("\\\\", "", tempfile(tmpdir = "")), "fix.csv")}
  dat_file <- paste0(gsub("\\\\", "", tempfile(tmpdir = "")), "dat.csv")

  # Assemble string for system call to Mdl
  if(fix_call){mdl_fix_call <- paste0("mdl export-entities -d ", db, " -m ", model_dir," -o ", fix_file, " -e fixes")}
  mdl_var_call <- paste0("mdl export-entities -d ", db, " -m ", model_dir," -o ", var_file, " -e variables")
  mdl_dat_call <- paste0("mdl export -d ", db, " -m ", model_dir, " -y ",
                         start_year, " -e ", end_year, " -o ", dat_file,
                         " -f Classic_v")
  if(fix_call){
    # Execute Mdl call to export fix metadata
    if (verbose) {
      message("Running Mdl to export fix metadata.")
    }

    if (system(mdl_fix_call) != 0) {
      stop("Mdl tool returned an error.")
    }

    # Read in fix metadata
    if (verbose) {
      message("Reading in fix metadata.")
    }

    fix_dat             <- read.csv(fix_file, header = TRUE,
                                    stringsAsFactors = FALSE, na.strings = "")
    fix_dat$Indicator   <- paste0(fix_dat$Mnemonic, "_", fix_dat$Sector)
    fix_dat$Indicator   <- .clean_names(fix_dat$Indicator)
    fix_dat$StartPeriod <- .oe_date(fix_dat$StartPeriod)
    fix_dat$EndPeriod   <- .oe_date(fix_dat$EndPeriod)
  }
  # Execute Mdl call to export variable metadata
  if (verbose) {
    message("Running Mdl to export variable metadata.")
  }

  if (system(mdl_var_call) != 0) {
    stop("Mdl tool returned an error.")
  }

  # Read in variable metadata
  if (verbose) {
    message("Reading in variable metadata.")
  }

  var_dat                <- read.csv(var_file, header = TRUE,
                                     stringsAsFactors = FALSE, na.strings = "")
  var_dat$Indicator      <- paste0(var_dat$Mnemonic, "_", var_dat$Sector)
  var_dat$Indicator      <- .clean_names(var_dat$Indicator)
  var_dat$End.of.History <- .oe_date(var_dat$End.of.History)
  var_dat$Is.percent     <- grepl(".*\\[%.*", var_dat$Description)

  sel_content <- NULL

  # If mnemonics and sectors have been supplied, build sel file
  if (!is.null(mnemonic) || !is.null(sector) || !is.null(mnemonic_sector)) {
    var_dat_subset <- var_dat
    if(fix_call){fix_dat_subset <- fix_dat}

    if (verbose) {
      message("Building SEL file.")
    }

    # If mnemonics have been supplied, subset variable information by mnemonics
    if (!is.null(mnemonic)) {
      mnemonic <- unique(mnemonic)
      var_dat_subset <- subset(var_dat_subset, Mnemonic %in% mnemonic)
      if(fix_call){fix_dat_subset <- subset(fix_dat_subset, Mnemonic %in% mnemonic)}
    }

    # If sectors have been supplied, subset variable information by sectors
    if (!is.null(sector)) {
      sector  <- unique(sector)
      var_dat_subset <- subset(var_dat_subset, Sector %in% sector)
      if(fix_call){fix_dat_subset <- subset(fix_dat_subset, Sector %in% sector)}
    }

    # If custom mnemonic-sector combinations have been supplied, add these
    if (!is.null(mnemonic_sector)) {
      mnemonic_sector <- unique(mnemonic_sector)

      var_dat_sel <- var_dat
      var_dat_sel <- subset(var_dat_sel,
                            (Mnemonic %in% mnemonic_sector$Mnemonic) &
                              (Sector %in% mnemonic_sector$Sector))
      var_dat_subset <- rbind(var_dat_subset, var_dat_sel)

      if(fix_call){fix_dat_sel <- fix_dat
      fix_dat_sel <- subset(fix_dat_sel,
                            (Mnemonic %in% mnemonic_sector$Mnemonic) &
                              (Sector %in% mnemonic_sector$Sector))
      fix_dat_subset <- rbind(fix_dat_subset, fix_dat_sel)}
    }

    # Reset row IDs
    var_dat_subset <- unique(var_dat_subset)
    if(fix_call){fix_dat_subset <- unique(fix_dat_subset)}
    rownames(var_dat_subset) <- NULL

    # Prepare content for temporary SEL file
    sel_content <- ""

    for (i in 1:nrow(var_dat_subset)) {
      sel_line    <- paste0(unique(var_dat_subset$Mnemonic[i]), ",",
                            unique(var_dat_subset$Sector[i]), ",",
                            type, ",L,_\n")
      sel_content <- paste0(sel_content, sel_line)
    }

    # Set name of temporary SEL file
    sel_file <- paste0(gsub("\\\\", "", tempfile(tmpdir = "")), "sel.sel")

    # Write content to SEL file
    write(sel_content, file = sel_file)

    # Append command to use SEL file to system call
    mdl_dat_call <- paste0(mdl_dat_call, " -s ", sel_file)

    # Overwrite main variable and fix metadata
    var_dat <- var_dat_subset
    if(fix_call){fix_dat <- fix_dat_subset}
  }

  # Execute system call to Mdl
  if (verbose) {
    message("Running Mdl to export data.")
  }

  if (system(mdl_dat_call) != 0) {
    stop("Mdl tool returned an error.")
  }

  # Load data into R
  if (verbose) {
    message("Reading in data.")
  }

  dat <- read.csv(dat_file, header = FALSE, stringsAsFactors = FALSE,
                  na.strings = "")

  # Create column names (format: Indicator_Location)
  mnemonic_row <- which(trimws(dat[, 1]) == "L") - 2
  sector_row   <- which(trimws(dat[, 1]) == "L") - 3
  date_row     <- which(trimws(dat[, 1]) == "L") + 2
  meta_end     <- which(trimws(dat[, 1]) == "L") + 4

  dat_col <- paste0((unname(trimws(dat[mnemonic_row, ]))), "_",
                    (unname(trimws(dat[sector_row, ]))))
  dat_col <- .clean_names(dat_col)
  colnames(dat) <- make.names(dat_col, unique = TRUE)
  colnames(dat)[ncol(dat)] <- "date"

  # Save metadata separately
  dat_head      <- dat[1:meta_end, -ncol(dat)]
  dat           <- dat[-(1:meta_end), ]
  rownames(dat) <- NULL

  # Convert remaining data to numeric format while preserving data frame
  dat[, 1:ncol(dat)] <- sapply(dat[, 1:ncol(dat)],
                               function(x) as.numeric(as.character(x)))

  # Carry forward last date value and append quarter
  year_ticks       <- !is.na(dat[, ncol(dat)])
  dat[, ncol(dat)] <- c(NA, dat[year_ticks, ncol(dat)])[cumsum(year_ticks) + 1]
  dat[, ncol(dat)] <- paste0(dat[, "date"], "-",
                             rep(c(1, 4, 7, 10), (nrow(dat) / 4)), "-", 1)
  dat[, ncol(dat)] <- as.Date(dat[, ncol(dat)])

  # Put date column first
  dat <- dat[, c(ncol(dat), 1:(ncol(dat) - 1))]

  # Save index of last historical data point
  last_hist <- trimws(dat_head[date_row, ])
  last_hist <- replace(last_hist, last_hist == "0", NA)
  last_hist <- .oe_date(last_hist)
  last_hist <- match(last_hist, dat$date, nomatch = NA)
  last_hist <- setNames(last_hist, nm = colnames(dat[, -1]))

  # Save index of first forecast data point
  first_fcst <- last_hist + 1
  first_fcst <- replace(first_fcst, first_fcst > nrow(dat), nrow(dat))

  # Remove temporary CSV files
  if (verbose) {
    message("Removing temporary files.")
  }

  file.remove(var_file)
  if(fix_call){file.remove(fix_file)}
  file.remove(dat_file)

  # Remove temporary SEL file
  if (!is.null(sel_content)) {
    file.remove(sel_file)
  }

  # Return xts object
  if (as_xts) {
    dat <- xts::xts(x = dat[, -1], order.by = dat[, 1])
  }

  # Return xts object and meta data
  if(fix_call){return(list(dat = dat, dat_head = dat_head, fix = fix_dat, var = var_dat,
                           type = type, last_hist = last_hist, first_fcst = first_fcst))
  }else{
    return(list(dat = dat, dat_head = dat_head, var = var_dat,
                type = type, last_hist = last_hist, first_fcst = first_fcst))
  }
}

