#Load Packages
library(XML)
library(RCurl)
library(zoo)
library(stringr)

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
      "original" = as.character(x),
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
    x <- x[!grepl('Total', x$S.N.),]
    
    x$Incidents <- gsub("[^[:digit:][:alnum:][:punct:][:space:]]", "", 
      x$Incidents)
    x$Incidents <- gsub('^\\s+|\\s+$|(?<=\\s)\\s+|\\"', '', 
      x$Incidents, perl = TRUE)
    
    x <- data.frame(
      "year" = rep(years, nrow(x)),
      "month" = gsub("[^[:alpha:]]", "", x$Date),
      "day" = gsub("[^[:digit:][:punct:]]", "", x$Date),
      "original" = x$Incidents,
      "record" = x$Incidents,
      stringsAsFactors = FALSE)
    
    x
  }
  x
})

satp <- do.call("rbind", satp)

#------------------------------------------------------------------------------#
# Standardize numbers
#------------------------------------------------------------------------------#

ones <- c("one", "two", "three", "four", "five", "six", "seven", "eight", 
  "nine")
teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
  "sixteen", "seventeen", "eighteen", "nineteen")
tens <- c("twenty", "thirty", "fou?rty", "fifty", "sixty", "seventy", "eighty", 
  "ninety")
hundreds <- c("(a|one) hundred", "two hundred", "three hundred", "four hundred", 
  "five hundred", "six hundred", "seven hundred", "eight hundred", 
  "nine hundred")
twodigits <- do.call("c", lapply(tens, function(x) {
  gsub('^\\s+|\\s+$|(?<=\\s)\\s+', '', paste(x, c("", ones)), perl = TRUE)
}))

first_part <- c(ones, teens, twodigits)

second_part <- do.call("c", lapply(hundreds, function(x) {
  gsub('^\\s+|\\s+$|(?<=\\s)\\s+', '', paste(x, c("", first_part)), perl = TRUE)
}))

num_table <- data.frame(
  "words" = c(first_part, second_part),
  "digits" = 1:999, 
  stringsAsFactors = FALSE)

num_table$words <- gsub("(hundred)\\s+", "\\1(and|\\\\s+|-)", num_table$words)
num_table$words <- gsub("\\s+", "(\\\\s+|-)", num_table$words)
num_table$words <- paste0("\\b", num_table$words)
num_table <- num_table[order(num_table$digits, decreasing = TRUE),]

for(i in 1:nrow(num_table)) {
  satp$record <- gsub(num_table$words[i], num_table$digits[i], satp$record, 
    ignore.case = TRUE)
}

# To use if eventually we want to pull specific casualty numbers
# satp$record <- gsub("\\b(no.?|house of) one\\b", " ", satp$record, perl = TRUE)
# satp$record <- gsub("\\bdozens?", " 12 ", satp$record, ignore.case = TRUE)
# satp$record <- gsub("\\bscores of", " 20 ", satp$record, ignore.case = TRUE)
# satp$record <- gsub("\\bas\\s+many\\s+as\\s+(\\d+)", " \\1 ", satp$record, ignore.case = TRUE)
# satp$record <- gsub("\\b(at\\s+least|up\\s+to)\\s+(\\d+)", " \\2 ", satp$record, ignore.case = TRUE)
# satp$record <- gsub("\\b(around|about|nearly)\\s+(\\d+)", " \\2 ", satp$record, ignore.case = TRUE)
# satp$record <- gsub("\\b(many|several|few) others?", " 0 ", satp$record, ignore.case = TRUE)
# satp$record <- gsub("\\ban?\\s+(unspecified)\\s+number(of\\s+)?", " 0 ", satp$record, ignore.case = TRUE)
# satp$record <- gsub("\\b(a|an)\\s+(militant|police(m.n)?|inspector)", " 1 \\2 ", satp$record, ignore.case = TRUE)
# satp$record <- gsub("20\\d{2}", satp$record, value = T, perl = TRUE)

#------------------------------------------------------------------------------#
# Extract location names
#------------------------------------------------------------------------------#
satp$record <- gsub('\\.\\s+([[:upper:]])', '. \\L\\1', satp$record, perl = TRUE)
satp$record <- gsub('^([[:upper:]])', '\\L\\1', satp$record, perl = TRUE)

proper_nouns <- str_extract_all(
  satp$record, 
  '[[:upper:]][[:lower:]]+(\\s+[[:upper:]][[:lower:]]+)*')
proper_nouns[sapply(proper_nouns, length) ==0] <- ""

proper_nouns <- do.call("rbind", lapply(1:length(proper_nouns), function(i) {
  cbind("row" = i, "locs" = proper_nouns[[i]])}))

tst <- lapply(unique(geo$p), function(x) agrep(x, proper_nouns[,"locs"]))

