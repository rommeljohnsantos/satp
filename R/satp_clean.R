#Load Packages
library(foreach)
library(plyr)
library(RCurl)
library(stringr)
library(tm)
library(XML)
library(XLConnect)
library(zoo)
library(gdata)
library(lubridate)

# Use the following if switching between Windows and Mac
Sys.setlocale('LC_ALL', 'C')

#------------------------------------------------------------------------------#
# Load Functions
cleanrecords <- function(x){
	year <- str_extract(deparse(substitute(x)), "\\d{4}")
	
	out1 <- unlist(strsplit(x, split='<p align=\"JUSTIFY\">', fixed = T))
	out2 <- gsub('([<](/?[^\\>]+)[>])', '', out1)
	out3 <- gsub('\\s{2,}', ' ', out2)
	out4 <- gsub('[^[:alnum:].]+', " ", out3)
	out5 <- gsub('(?<=Note Compiled from news reports and)[[:print:]]*', '', out4, 
							 perl = T)
	out6 <- gsub(paste('[[:print:]]*(?=', year, ' ', year, ')', sep=''), '', out5, 
							 perl = T)
	out7 <- gsub('Note Compiled from news reports and', '', out6)
	out8 <- gsub(paste(year, ' ', year, sep=''), '', out7)
	out9 <- gsub('(^\\s*?|\\s*?$)', '', out8)
	out10 <- out9[!out9==""]
	out11 <- out10[grepl("[[:alpha:]]", out10)]
	out12 <- out11[!grepl("INDIA PAKISTAN NEPAL BHUTAN BANGLADESH SRI LANKA", 
												out11)]
	
	dates <- str_extract(
		out12, 
		'^\\s*(January|February|March|April|May|June|July|August|September|October|November|December) \\d{1,2}')
	
	dates <- na.locf(dates)
	
	month <- match(str_extract(dates, "[[:alpha:]]+"), month.name)
	day <- str_extract(dates, "\\d+")
	
	out13 <- gsub(
		'\\s*(January|February|March|April|May|June|July|August|September|October|November|December) \\d+(\\s*[-]\\s*\\d+)?', 
		'', out12)
	out14 <- gsub("(^|[.])\\s*([[:upper:]][[:lower:]]+)\\b", "\\L \\2", out13, perl=T)
	out15 <- gsub('\\s{2,}', ' ', out14)
	out16 <- gsub("^\\s*\\d+\\s*(?!Taliban|TTP|SF)([[:upper:]])", "\\1", out15, perl = T)
	out17 <- gsub('(^\\s+|\\s+$|[[:punct:]])', '', out16)
	
	
	cbind("year"=year, 
				"month"=month, 
				"day"=day, 
				"original"=out17, 
				"record"=out17)
}

#------------------------------------------------------------------------------#
# Load locations data

# loc.info <- read.xls(
# 	"http://oneresponse.info/Countries/Pakistan/IM/publicdocuments/Pakistan_List%20of_Administrative%20levels%20(with%20PCODE).xls", 
# 	as.is = T)[, c(1, 3, 5, 7)]
tf <- paste(tempfile(), "xls", sep = ".")
url = "http://oneresponse.info/Countries/Pakistan/IM/publicdocuments/Pakistan_List%20of_Administrative%20levels%20(with%20PCODE).xls"
download.file(url, tf, mode="wb") 
wb <- loadWorkbook(tf, create = F) 
loc.info <- readWorksheet(wb, 1)[, c(1, 3, 5, 7)]

loc.info$Province[grepl("AJK|FANA", loc.info$Province)] <- "contestedareas"
loc.info <- loc.info[!grepl("Name Unknown", loc.info$Province), ]
loc.info$Province <- tolower(loc.info$Province)

loc.info1 <- loc.info[, c("Province", "District", "Tehsil")]
loc.info2 <- loc.info[loc.info$Union.Council != "", 
											c("Province", "District", "Union.Council")]

colnames(loc.info1) <- c("province", "district", "subdivision")
colnames(loc.info2) <- c("province", "district", "subdivision")

locations <- rbind(loc.info1, loc.info2)

locations$subdivision <- gsub(" (Tehsil|Sub-division|Taluka)", "", 
															locations$subdivision)
locations$subdivision <- gsub("\\bi{1,3}$|\bNo(\\s[[:punct:]])[[:print:]]+$|Urban..", 
															"", locations$subdivision)
locations$subdivision <- gsub("(\\s|[[:punct:]])*\\d+(\\s|[[:punct:]])*", "", 
															locations$subdivision)
locations$subdivision <- gsub("\\b\\w{1,3}[[:punct:]]+(\\w){1,3}\\b|[[:punct:]]+w{,3}\\b", "", 
															locations$subdivision)
locations$subdivision <- gsub(" City\\s?|\\s?No$| Uc\\s?| Town|[.] ", "", 
															locations$subdivision)
locations$subdivision <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", 
															"", locations$subdivision, perl = TRUE)
locations$subdivision <- gsub("D.\\s?(G|g).\\s?(K|k)han", "Dera Ghazi Khan", 
															locations$subdivision)
locations$subdivision <- gsub("D.\\s?(I|i).\\s?(K|k)han", "Dera Ismail Khan", 
															locations$subdivision)
locations$district <- gsub("Tribal Area Adj.|T.a.adj.| PA$| Agency| Central| West| Distt", "", 
													 locations$district)
locations$district <- gsub("D.\\s?(G|g).\\s?(K|k)han", "Dera Ghazi Khan", 
													 locations$district)
locations$district <- gsub("D.\\s?(I|i).\\s?(K|k)han", "Dera Ismail Khan", 
													 locations$district)
locations$district <- gsub("Chagai", "Chag.?ai", locations$district)
locations$subdivision <- gsub("..Tribe.?", "", locations$subdivision)
locations$province[locations$province=="islamabad"] <- "punjab"
locations$province[locations$province=="sind"] <- "sindh"
locations$subdivision[grepl("Afridy Adam Khel Tribe", 
														locations$subdivision)] <- "Darr?a\\s?Adam\\s?Khe.?l"
locations$district[grepl("\\bdir", locations$district, 
												 ignore.case = T)] <- "Dir"
locations$subdivision[locations$province=="nwfp" & 
												grepl("Matta", locations$subdivision)] <- "Matta"
locations$subdivision[grepl("\\bdir", locations$subdivision, 
														ignore.case = T)] <- "Dir"
locations$subdivision[grepl("\\bkurram", locations$subdivision, 
														ignore.case = T)] <- "Kurram"
locations$subdivision[grepl("\\bMohammad", locations$subdivision, 
														ignore.case = T)] <- "Mohammad"
locations$subdivision[grepl("\\bZhob", locations$subdivision, 
														ignore.case = T)] <- "Zhob"
locations$district[grepl("^Tank$", locations$district) & 
									 	grepl("fata", locations$province)] <- "Tank (tribal )?area"
locations$district[grepl("^Tank$", 
												 locations$district)] <- "Tank (district|city)"

locations$subdivision <- gsub("Tank", "sxxxxxxxx1", locations$subdivision)
locations$subdivision[grepl("Nawagai", locations$subdivision) &
												locations$province == "nwfp"] <- "sxxxxxxxx2"
locations <- rbind(locations, c("punjab", "islamabad", "sxxxxxxxx3"), 
									 c("balochistan", "Musakhel", "sxxxxxxxx4"), 
									 c("balochistan", "Barkhan", "sxxxxxxxx5"), 
									 c("balochistan", "Panjgur", "dxxxxxxxx1"))
locations[grepl("Orakzai", locations$district), 
					"subdivision"] <- paste("sxxxxxxxx", 6:9, sep="")
locations[locations$province=="nwfp" &
						locations$district == "Charsadda" &
						!is.na(locations$subdivision) &
						locations$subdivision=="Matta", "subdivision"] <- "sxxxxxxx10"
locations <- locations[!(grepl("Shamozai", locations$subdivision) &
												 	!grepl("Swat", locations$district)), ]
locations$district[grepl("^Peshawar$", locations$district) & 
									 	grepl("fata", locations$province)] <- "dxxxxxxxx2"
locations$district[grepl("^Dera Ismail Khan$", locations$district) & 
									 	grepl("fata", locations$province)] <- "dxxxxxxxx3"
locations$district[grepl("^Bannu$", locations$district) & 
									 	grepl("fata", locations$province)] <- "dxxxxxxxx4"
locations$district[grepl("^Kohat$", locations$district) & 
									 	grepl("fata", locations$province)] <- "Darra Adam Khel"
locations$district[grepl("^lakki Marwat$", locations$district) & 
									 	grepl("fata", locations$province)] <- "dxxxxxxxx5"
locations$district[grepl("^Nawagai$", locations$district) & 
									 	grepl("fata", locations$province)] <- "dxxxxxxxx6"

toremove <- tolower(locations$subdivision) %in% tolower(locations$district)
locations <- locations[!toremove, ]
locations <- locations[locations$subdivision!="",]
locations <- as.data.frame(apply(locations, FUN=gsub, MARGIN = 2, pattern = "\\s+", replacement = ".?"), stringsAsFactors = F)
locations <- unique(locations)

#------------------------------------------------------------------------------#
# Load and parse SATP records

rec2012 <- toString.XMLNode(
	htmlParse(
		getURL(
			"http://www.satp.org/satporgtp/countries/pakistan/database/majorincidents.htm")))
rec2011 <- toString.XMLNode(
	htmlParse(
		getURL(
			"http://www.satp.org/satporgtp/countries/pakistan/database/majorinci2011.htm")))
rec2010 <- toString.XMLNode(
	htmlParse(
		getURL(
			"http://www.satp.org/satporgtp/countries/pakistan/database/majorinci2010.htm")))
rec2009 <- toString.XMLNode(
	htmlParse(
		getURL(
			"http://www.satp.org/satporgtp/countries/pakistan/database/majorinci2009.htm")))
rec2008 <- toString.XMLNode(
	htmlParse(
		getURL(
			"http://www.satp.org/satporgtp/countries/pakistan/database/majorinci2008.htm")))
rec2007 <- toString.XMLNode(
	htmlParse(
		getURL(
			"http://www.satp.org/satporgtp/countries/pakistan/database/majorinci2007.htm")))
rec2006.a <- toString.XMLNode(
	htmlParse(
		getURL(
			"http://www.satp.org/satporgtp/countries/pakistan/database/majorinc2006.htm")))

rec2006split <- strsplit(rec2006.a, "2005(?![-])", perl = T)

rec2006 <- rec2006split[[1]][1]
rec2005 <- rec2006split[[1]][2]

clean2012 <- cleanrecords(rec2012)
clean2011 <- cleanrecords(rec2011)
clean2010 <- cleanrecords(rec2010)
clean2009 <- cleanrecords(rec2009)
clean2008 <- cleanrecords(rec2008)
clean2007 <- cleanrecords(rec2007)
clean2006 <- cleanrecords(rec2006)
clean2005 <- cleanrecords(rec2005)

pak <- rbind(clean2012, 
						 clean2011, 
						 clean2010, 
						 clean2009, 
						 clean2008, 
						 clean2007, 
						 clean2006, 
						 clean2005)

pak <- as.data.frame(pak, stringsAsFactors = F)
