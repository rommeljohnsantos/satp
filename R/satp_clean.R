#Load Packages
library(XML)
library(RCurl)
library(zoo)
library(stringr)
library(pbapply)
library(tm)
library(missForest)

# # Use the following if switching between Windows and Mac
# Sys.setlocale('LC_ALL', 'C')
#------------------------------------------------------------------------------#
# Load locations data
# Located at http://floods2010.pakresponse.info/MapCenter/GISData.aspx

geo1 <- read.csv(text = 
  getURL(
    "https://raw.github.com/schaunwheeler/satp/master/data/pakistan_pcodes_tehsil_20110304.csv"),
  as.is = TRUE)

geo2 <- read.csv(text = 
    getURL(
      "https://raw.github.com/schaunwheeler/satp/master/data/pakistan_pcodes_unioncouncil_20110304.csv"),
  as.is = TRUE)

geo <- merge(geo1, 
  geo2[, c("PROVINCE_C", "DISTRICT_C", "TEHSIL_C", "Union.Council")],
  by = c("PROVINCE_C", "DISTRICT_C", "TEHSIL_C"),
  all.x = TRUE, all.y = FALSE)
geo <- geo[,!grepl("_C$", colnames(geo))]
colnames(geo) <- c("p", "d", "t", "u")

geo[] <- lapply(geo, function(x) gsub("\\s+No\\.?\\s*(\\d|i).*$", "", x)) 
geo[] <- lapply(geo, function(x) gsub("(\\s+|_)?\\d.*$", "", x)) 
geo[] <- lapply(geo, function(x) gsub("(-i|\\().*$", "", x)) 
geo[] <- lapply(geo, function(x) gsub("-(uc)?$", "", x)) 
geo[] <- lapply(geo, function(x) gsub("De.(e|E)xcluded Area (D.g )?", "", x)) 
geo[] <- lapply(geo, function(x) gsub(" (P\\.a\\.|Tc|Uc)$", "", x)) 
geo[] <- lapply(geo, function(x) gsub("\\w\\.\\s*", "", x)) 
geo[] <- lapply(geo, function(x) gsub("\\s*/\\s*", "|", x)) 
geo[] <- lapply(geo, function(x) gsub("-", "", x)) 
geo[] <- lapply(geo, function(x) gsub("^(\\w)", "\\U\\1", x, perl = TRUE)) 
geo[] <- lapply(geo, function(x) gsub("\\b([[:lower:]])", "\\U\\1", x, perl = TRUE))
geo[] <- lapply(geo, function(x) gsub("\\s+\\w?$", "", x))
geo[] <- lapply(geo, function(x) gsub("\\b(City|Hill|Cantt)\\b", "", x))
geo[] <- lapply(geo, function(x) gsub("\\b(Agency|District|Tehsil)\\b", "", x))
geo[] <- lapply(geo, function(x) gsub("\\b(Union|Council|Province)\\b", "", x))
geo[] <- lapply(geo, function(x) gsub("\\b(Center|Road|Street)\\b", "", x))

geo$p[grepl("Gilgit|Baltistan|Azad|Kashmir|Disputed", geo$p)] <- "Northern Areas"
geo$p[grepl("Federal|Capital", geo$p)] <- "Punjab"
geo[] <- lapply(geo, function(x) {
  gsub('^\\s+|\\s+$|(?<=\\s)\\s+', '', x, perl = TRUE)
})
geo[geo == "" | geo == "Unknown" | geo == "Tribal Area"] <- NA
geo <- unique(geo)
geo$index <- 1:nrow(geo)

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
extract_record <- gsub('\\.\\s+([[:upper:]])', '. \\L\\1', satp$record, 
  perl = TRUE)
extract_record <- gsub('^([[:upper:]])', '\\L\\1', extract_record, perl = TRUE)

proper_nouns <- str_extract_all(
  extract_record, 
  '[[:upper:]][[:lower:]]+(\\s+[[:upper:]][[:lower:]]+)*')

proper_nouns[sapply(proper_nouns, length) ==0] <- ""

proper_nouns <- 
  do.call("rbind", lapply(1:length(proper_nouns), function(i) {
  data.frame("record" = i, "phrase" = proper_nouns[[i]],
    stringsAsFactors = FALSE)}))

geo_matches <- lapply(1:ncol(geo[,c("p", "d", "t", "u")]), function(i) {
  x <- geo[!duplicated(geo[,1:i]) & !is.na(geo[,i]), ]
  if(names(geo)[i] == "p") {
    x_kp <- x[x$p == "Khyber Pakhtunkhwa",]; x_fa <- x[x$p == "Fata",]
    
    add_kp <- do.call("rbind", 
      lapply(c("KP", "NWFP", "North West Frontier Province"), 
        function(y){ out <- x_kp; out[,i] <- y; out}))
      
    add_fa <- do.call("rbind", 
      lapply(c("FATA", "Federally Administered Tribal Areas"), 
        function(y){out <- x_fa; out[,i] <- y; out}))
    
    x <- rbind(x, add_kp, add_fa)
  }
  
  print(names(geo)[i])
  
  extr <- pblapply(x[,i], function(y) {
    if(grepl("\\b(Dir|Waziristan)\\b", y) | names(geo)[i] == "u") {
      out <- grep(paste0(y, "\\b"), proper_nouns$phrase)
    } else {
      out <- agrep(paste0(y, "\\b"), proper_nouns$phrase, fixed = FALSE)
    }
    out
  })
  
  names(extr) <- x$index
  
  extr <- tapply(extr, x$index, function(x) unique(unlist(x)))
  
  extr <- lapply(1:i, function(j) {
    out <- extr
    names(out) <- geo[as.numeric(names(out)), j]
    out[sapply(extr, length) > 0]
    })
  extr <- lapply(extr, function(z){
    tapply(z, names(z), function(a) sort(unique(unlist(a))))
  })
  names(extr) <- colnames(geo)[1:i]
  extr[sapply(extr, length) > 0]
})
names(geo_matches) <- c("province", "district", "tehsil", "unioncouncil")

geo_matches <- rapply(geo_matches, unlist)
geo_matches <- stack(geo_matches)
geo_matches$ind <- as.character(geo_matches$ind)

geo_matches <- cbind(
  proper_nouns[geo_matches$values,], 
  "ind" = geo_matches$ind,
  stringsAsFactors = FALSE)

geo_matches$origin <- gsub("([a-zA-z ]+)\\.([a-zA-z ])\\.([a-zA-z ]+)\\d*", 
  "\\1", geo_matches$ind, perl = TRUE)
geo_matches$target <- gsub("([a-zA-z ]+)\\.([a-zA-z ])\\.([a-zA-z ]+)\\d*", 
  "\\2", geo_matches$ind, perl = TRUE)
geo_matches$label <- gsub("([a-zA-z ]+)\\.([a-zA-z ])\\.([a-zA-z ]+)\\d*", 
  "\\3", geo_matches$ind, perl = TRUE)

geo_check <- geo_matches

geo_matches <- pblapply(c("p", "d", "t", "u"), function(x) {
  cut_df <- geo_matches[geo_matches$target == x,]
  out <- tapply(cut_df$label, cut_df$record, function(y) {
    tab <- table(y)
    tab <- data.frame("values" = as.numeric(tab), "ind" = names(tab),
      stringsAsFactors = FALSE)
    if(nrow(tab) > 1) {
      if(min(tab$values) < max(tab$values) & 
          sum(tab$values == max(tab$values)) == 1) {
        tab <- tab[tab$values == max(tab$values),]
      } else {
        tab$ind <- NA
      }
    }
    unique(tab$ind)
  })
  out_names <- names(out)
  out <- data.frame(
    "record" = as.numeric(names(out)), 
      "assign" = as.character(out), 
      stringsAsFactors = FALSE)
  out <- out[match(1:nrow(satp), out$record),]
  colnames(out)[grep("assign", colnames(out))] <- x
  out$record <- 1:nrow(out)
  out
})

geo_matches <- Reduce(function(...) merge(..., by = c("record"), 
  all = TRUE), geo_matches)

geo_matches$p[is.na(geo_matches$p) & !is.na(geo_matches$d)] <-
  geo$p[match(geo_matches$d[is.na(geo_matches$p) & !is.na(geo_matches$d)],
    geo$d)]

for(x in na.omit(unique(geo_matches$p))) {
  geo_matches$d[
    geo_matches$p == x &
      !(geo_matches$d %in% na.omit(geo$d[geo$p == x])) & 
      !is.na(geo_matches$d)] <- NA
}

cap_frame <- as.data.frame.matrix(table(proper_nouns))
cap_frame <- cap_frame[,colSums(cap_frame != 0) > 1]

word_frame <- as.data.frame(as.matrix(DocumentTermMatrix(Corpus(VectorSource(
  satp$record)))), stringsAsFactors = FALSE)
word_frame <- word_frame[,colMeans(word_frame != 0) > .01 & 
    !grepl("[[:upper:][:digit:]]", colnames(word_frame))]

p_imp_df <- cbind("p" = geo_matches$p, cap_frame, word_frame)
set.seed(42)
p_imp <- as.character(missForest(p_imp_df, verbose = TRUE, 
  ntree = 100)$ximp$p)

p_imp_false <- !sapply(1:length(p_imp), function(i) {
  p_imp[i] %in% unique(geo_check$label[
    geo_check$target == "p" & 
      geo_check$record == i]
  )
})
p_imp_for_d <- p_imp
p_imp_for_d[p_imp_false] <- "Unknown"

d_imp_df <- cbind("d" = geo_matches$d, cap_frame, word_frame)

d_imps <- lapply(unique(p_imp_for_d), function(x) {
  df <- subset(d_imp_df, p_imp_for_d == x)
  if(x != "Unknown" & sum(is.na(df$d)) > 0) {
    df <- droplevels(df)
    set.seed(42)
    d_imp <- as.character(missForest(df, verbose = TRUE)$ximp$d)
    } else {
      d_imp <- df$d
    }
  data.frame(
    "d_imp" = d_imp,
    ind = row.names(df),
    stringsAsFactors = FALSE)  
})
d_imp <- do.call("rbind", d_imps)
d_imp <- d_imp[order(as.numeric(d_imp$ind)),"d_imp"] 

d_imp_df2 <- cbind("p" = p_imp_for_d, "d" = d_imp, cap_frame, word_frame)
set.seed(42)
d_imp2 <- as.character(missForest(d_imp_df2, verbose = TRUE)$ximp$d)

d_imp_false <- !sapply(1:length(d_imp), function(i) {
  d_imp2[i] %in% unique(geo_check$label[
    geo_check$target == "d" & 
      geo_check$record == i]
  )
})

satp_check <- data.frame(
  "p" = geo_matches$p, 
  "p_rf" = p_imp,
  "p_rf_miss" = p_imp_false,
  "d" = geo_matches$d, 
  "d_rf" = d_imp2,
  "d_rf_miss" = d_imp_false,
  "record" = satp$record,
  stringsAsFactors = FALSE)
