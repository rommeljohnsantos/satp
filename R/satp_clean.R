#Load Packages
library(XML)
library(RCurl)
library(zoo)

# # Use the following if switching between Windows and Mac
# Sys.setlocale('LC_ALL', 'C')

#------------------------------------------------------------------------------#
# Load locations data
# Located at http://floods2010.pakresponse.info/MapCenter/GISData.aspx

geo <- read.csv(text = 
  getURL("https://raw.github.com/schaunwheeler/satp/master/data/pakistan_pcodes_20110304.csv"),
  as.is = TRUE)
colnames(geo) <- c("p", "d", "t", "u")
geo[] <- lapply(geo, function(x) gsub("\\s+No\\.?\\s*(\\d|i).*$", "", x)) 
geo[] <- lapply(geo, function(x) gsub("(\\s+|_)?\\d.*$", "", x)) 
geo[] <- lapply(geo, function(x) gsub("(-i|\\().*$", "", x)) 
geo[] <- lapply(geo, function(x) gsub("-(uc)?$", "", x)) 
geo[] <- lapply(geo, function(x) gsub("De-excluded Area (D.g )?", "", x)) 
geo[] <- lapply(geo, function(x) gsub(" (P\\.a\\.|Tc|Uc)$", "", x)) 
geo[] <- lapply(geo, function(x) gsub("\\w\\.\\s*", "", x)) 
geo[] <- lapply(geo, function(x) gsub("\\s*/\\s*", "|", x)) 
geo[] <- lapply(geo, function(x) gsub("-", "", x)) 
geo[] <- lapply(geo, function(x) gsub("^(\\w)", "\\U\\1", x, perl = TRUE)) 
geo[] <- lapply(geo, function(x) gsub("\\b([[:lower:]])", "\\U\\1", x, perl = TRUE))
geo[] <- lapply(geo, function(x) gsub("\\s+\\w?$", "", x)) 

geo <- unique(geo)

#------------------------------------------------------------------------------#
# Load and parse SATP records
satp_url <- "http://www.satp.org/satporgtp/countries/pakistan/database/index.html"
satp <- getHTMLLinks((getURL(satp_url)), baseURL = satp_url, relative = TRUE)
satp <- grep("majorinc", satp, value = TRUE)

satp <- lapply(satp, getURL)

# Maybe include grepl("400 Bad Request", x) to test for scraping failure

satp <- lapply(satp, function(x) {

  if(length(unlist(gregexpr("<td", x))) < 7) {
    
    x <- unlist(strsplit(
      unlist(
        regmatches(
          x, 
          gregexpr('<!-- #BeginEditable \"body\" -->.+<!-- #EndEditable -->', x)
        )
      ),
      "<p.*?>"))
    
    x <- gsub("\\r\\n|<.*?>", "", x)
    x <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", x, perl = TRUE)
    x <- x[nchar(x) > 0]
    
    monthnames <- paste0(c("January", "February", "March", "April", "May", "June", 
      "July", "August", "September", "October", "November", "December"), 
      collapse = "|")
    
    monthday <- regmatches(x, gregexpr(paste0(
      "^(", 
      monthnames, 
      ")\\s*\\d{1,2}-?(", 
      monthnames, 
      ")?\\s*\\d*"), x))
    monthday[sapply(monthday, length) == 0] <- ""
    monthday <- unlist(monthday)
    monthday <- gsub(":.*$", "", monthday)
    monthday[monthday == ""] <- NA
    if(is.na(monthday[1])) {
      monthday[1] <- "X"
    }
    monthday <- na.locf(monthday)
    
    years <- as.character(x[grep("^\\d{4}$", x)[
      na.locf(match(1:length(x), grep("^\\d{4}$", x)))]])
    
    x <- gsub('^.{,15}:\\s+', '', x)
    x <- gsub("[^[:digit:][:alnum:][:punct:][:space:]]", "", x)
    x <- gsub('^\\s+|\\s+$|(?<=\\s)\\s+|\\"', '', x, perl = TRUE)
    
    x <- data.frame(
      "year" = years,
      "month" = gsub("[^[:alpha:]]", "", monthday),
      "day" = gsub("[^[:digit:][:punct:]]", "", monthday),
      "record" = as.character(x),
      stringsAsFactors = FALSE)
    x <- x[!grepl("^\\d{4}$", x$record),]
  } else {

    years <- unlist(regmatches(x, gregexpr('<TITLE.+?TITLE>', x)))
    years <- gsub('\\D', "", years)
    x <- unlist(regmatches(x, 
      gregexpr('<!-- #BeginEditable \"body\" -->.+<!-- #EndEditable -->', x)))
    x <- readHTMLTable(x, stringsAsFactors = FALSE)
    ind <- which.max(sapply(x, function(y) length(unlist(y))))
    x <- x[[ind]]
    
    x$Incidents <- gsub("[^[:digit:][:alnum:][:punct:][:space:]]", "", 
      x$Incidents)
    x$Incidents <- gsub('^\\s+|\\s+$|(?<=\\s)\\s+|\\"', '', 
      x$Incidents, perl = TRUE)
    
    x <- data.frame(
      "year" = rep(years, nrow(x)),
      "month" = gsub("[^[:alpha:]]", "", x$Date),
      "day" = gsub("[^[:digit:][:punct:]]", "", x$Date),
      "record" = x$Incidents,
      stringsAsFactors = FALSE)
  }
  x
})

satp <- do.call("rbind", satp)
