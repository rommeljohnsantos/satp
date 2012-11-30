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

################################################################################
# Identify fatalities and injuries for each record
################################################################################

pak$record <- record2

# Clean up numbers
pak$record <- gsub("\\bninety\\s?nine", "99", pak$record, ignore.case = T)
pak$record <- gsub("\\bninety\\s?eight", "98", pak$record, ignore.case = T)
pak$record <- gsub("\\bninety\\s?seven", "97", pak$record, ignore.case = T)
pak$record <- gsub("\\bninety\\s?six", "96", pak$record, ignore.case = T)
pak$record <- gsub("\\bninety\\s?five", "95", pak$record, ignore.case = T)
pak$record <- gsub("\\bninety\\s?four", "94", pak$record, ignore.case = T)
pak$record <- gsub("\\bninety\\s?three", "93", pak$record, ignore.case = T)
pak$record <- gsub("\\bninety\\s?two", "92", pak$record, ignore.case = T)
pak$record <- gsub("\\bninety\\s?one", "91", pak$record, ignore.case = T)
pak$record <- gsub("\\bninety", "90", pak$record, ignore.case = T)
pak$record <- gsub("\\beighty\\s?nine", "89", pak$record, ignore.case = T)
pak$record <- gsub("\\beighty\\s?eight", "88", pak$record, ignore.case = T)
pak$record <- gsub("\\beighty\\s?seven", "87", pak$record, ignore.case = T)
pak$record <- gsub("\\beighty\\s?six", "86", pak$record, ignore.case = T)
pak$record <- gsub("\\beighty\\s?five", "85", pak$record, ignore.case = T)
pak$record <- gsub("\\beighty\\s?four", "84", pak$record, ignore.case = T)
pak$record <- gsub("\\beighty\\s?three", "83", pak$record, ignore.case = T)
pak$record <- gsub("\\beighty\\s?two", "82", pak$record, ignore.case = T)
pak$record <- gsub("\\beighty\\s?one", "81", pak$record, ignore.case = T)
pak$record <- gsub("\\beighty", "80", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventy\\s?nine", "79", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventy\\s?eight", "78", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventy\\s?seven", "77", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventy\\s?six", "76", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventy\\s?five", "75", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventy\\s?four", "74", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventy\\s?three", "73", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventy\\s?two", "72", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventy\\s?one", "71", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventy", "70", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixty\\s?nine", "69", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixty\\s?eight", "68", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixty\\s?seven", "67", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixty\\s?six", "66", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixty\\s?five", "65", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixty\\s?four", "64", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixty\\s?three", "63", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixty\\s?two", "62", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixty\\s?one", "61", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixty", "60", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifty\\s?nine", "59", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifty\\s?eight", "58", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifty\\s?seven", "57", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifty\\s?six", "56", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifty\\s?five", "55", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifty\\s?four", "54", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifty\\s?three", "53", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifty\\s?two", "52", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifty\\s?one", "51", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifty", "50", pak$record, ignore.case = T)
pak$record <- gsub("\\b(forty|fourty)\\s?nine", "49", pak$record, ignore.case = T)
pak$record <- gsub("\\b(forty|fourty)\\s?eight", "48", pak$record, ignore.case = T)
pak$record <- gsub("\\b(forty|fourty)\\s?seven", "47", pak$record, ignore.case = T)
pak$record <- gsub("\\b(forty|fourty)\\s?six", "46", pak$record, ignore.case = T)
pak$record <- gsub("\\b(forty|fourty)\\s?five", "45", pak$record, ignore.case = T)
pak$record <- gsub("\\b(forty|fourty)\\s?four", "44", pak$record, ignore.case = T)
pak$record <- gsub("\\b(forty|fourty)\\s?three", "43", pak$record, ignore.case = T)
pak$record <- gsub("\\b(forty|fourty)\\s?two", "42", pak$record, ignore.case = T)
pak$record <- gsub("\\b(forty|fourty)\\s?one", "41", pak$record, ignore.case = T)
pak$record <- gsub("\\b(forty|fourty)", "40", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirty\\s?nine", "39", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirty\\s?eight", "38", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirty\\s?seven", "37", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirty\\s?six", "36", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirty\\s?five", "35", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirty\\s?four", "34", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirty\\s?three", "33", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirty\\s?two", "32", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirty\\s?one", "31", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirty", "30", pak$record, ignore.case = T)
pak$record <- gsub("\\btwenty\\s?nine", "29", pak$record, ignore.case = T)
pak$record <- gsub("\\btwenty\\s?eight", "28", pak$record, ignore.case = T)
pak$record <- gsub("\\btwenty\\s?seven", "27", pak$record, ignore.case = T)
pak$record <- gsub("\\btwenty\\s?six", "26", pak$record, ignore.case = T)
pak$record <- gsub("\\btwenty\\s?five", "25", pak$record, ignore.case = T)
pak$record <- gsub("\\btwenty\\s?four", "24", pak$record, ignore.case = T)
pak$record <- gsub("\\btwenty\\s?three", "23", pak$record, ignore.case = T)
pak$record <- gsub("\\btwenty\\s?two", "22", pak$record, ignore.case = T)
pak$record <- gsub("\\btwenty\\s?one", "21", pak$record, ignore.case = T)
pak$record <- gsub("\\btwenty", "20", pak$record, ignore.case = T)
pak$record <- gsub("\\bnine\\s?hundred", "900", pak$record, ignore.case = T)
pak$record <- gsub("\\beight\\s?hundred", "800", pak$record, ignore.case = T)
pak$record <- gsub("\\bseven\\s?hundred", "700", pak$record, ignore.case = T)
pak$record <- gsub("\\bsix\\s?hundred", "600", pak$record, ignore.case = T)
pak$record <- gsub("\\bfive\\s?hundred", "500", pak$record, ignore.case = T)
pak$record <- gsub("\\bfour\\s?hundred", "400", pak$record, ignore.case = T)
pak$record <- gsub("\\bthree\\s?hundred", "300", pak$record, ignore.case = T)
pak$record <- gsub("\\btwo\\s?hundred", "200", pak$record, ignore.case = T)
pak$record <- gsub("\\b(one|a)\\s?hundred", "100", pak$record, ignore.case = T)
pak$record <- gsub("\\bnineteen", " 19", pak$record, ignore.case = T)
pak$record <- gsub("\\beighteen", " 18", pak$record, ignore.case = T)
pak$record <- gsub("\\bseventeen", " 17", pak$record, ignore.case = T)
pak$record <- gsub("\\bsixteen", " 16", pak$record, ignore.case = T)
pak$record <- gsub("\\bfifteen", " 15", pak$record, ignore.case = T)
pak$record <- gsub("\\bfourteen", " 14", pak$record, ignore.case = T)
pak$record <- gsub("\\bthirteen", " 13", pak$record, ignore.case = T)
pak$record <- gsub("\\btwelve", " 12", pak$record, ignore.case = T)
pak$record <- gsub("\\beleven", " 11", pak$record, ignore.case = T)
pak$record <- gsub("\\bten", " 10", pak$record, ignore.case = T)
pak$record <- gsub("\\bnine", " 9", pak$record, ignore.case = T)
pak$record <- gsub("\\beight", " 8", pak$record, ignore.case = T)
pak$record <- gsub("\\bseven", " 7", pak$record, ignore.case = T)
pak$record <- gsub("\\bsix", " 6", pak$record, ignore.case = T)
pak$record <- gsub("\\bfive", " 5 ", pak$record, perl = T)
pak$record <- gsub("\\bfour", " 4 ", pak$record, perl = T)
pak$record <- gsub("\\bthree", " 3 ", pak$record, perl = T)
pak$record <- gsub("\\btwo", " 2 ", pak$record, perl = T)
pak$record <- gsub("\\b(no.?|house of) one\\b", " ", pak$record, perl = T)
pak$record <- gsub("\\bone", " 1 ", pak$record, perl = T)
pak$record <- gsub("\\bdozens?", " 12 ", pak$record, ignore.case = T)
pak$record <- gsub("\\bscores of", " 20 ", pak$record, ignore.case = T)
pak$record <- gsub("\\bas\\s+many\\s+as\\s+(\\d+)", " \\1 ", pak$record, ignore.case = T)
pak$record <- gsub("\\b(at\\s+least|up\\s+to)\\s+(\\d+)", " \\2 ", pak$record, ignore.case = T)
pak$record <- gsub("\\b(around|about|nearly)\\s+(\\d+)", " \\2 ", pak$record, ignore.case = T)
pak$record <- gsub("\\b(many|several|few) others?", " 0 ", pak$record, ignore.case = T)
pak$record <- gsub("\\ban?\\s+(unspecified)\\s+number(of\\s+)?", " 0 ", pak$record, ignore.case = T)
pak$record <- gsub("^(A|An|a|an)\\s+(militant|(P|p)olice(m.n)?|(I|i)nspector|volunteer|soldier|senior|teenage|woman|lieutenant|Taliban|key|FC|top|Deputy|AP|military|Major|SHO|brother|former|Islamist|commander)", " 1 \\2 ", pak$record)
pak$record <- gsub("2011", "year two thousand eleven", pak$record)
pak$record <- gsub("2010", "year two thousand ten", pak$record)
pak$record <- gsub("2009", "year two thousand nine", pak$record)
pak$record <- gsub("2008", "year two thousand eight", pak$record)
pak$record <- gsub("2007", "year two thousand seven", pak$record)
pak$record <- gsub("2006", "year two thousand six", pak$record)
pak$record <- gsub("2005", "year two thousand five", pak$record)
pak$record <- gsub("2004", "year two thousand four", pak$record)
pak$record <- gsub("2003", "year two thousand three", pak$record)
pak$record <- gsub("2002", "year two thousand two", pak$record)
pak$record <- gsub("2001", "year two thousand one", pak$record)

# get rid of all uppercase words and acronyms
pak$record <- gsub("\\bSF\\w*\\b", "paksfs", pak$record, ignore.case = T)
pak$record <- gsub("\\b(headquarters?)\\s+of\\s+[[:upper:]]\\w+", "", pak$record, ignore.case = T)
pak$record <- gsub("(No|Muhammad Akbar)\\s+\\d+", "", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)
pak$record <- gsub("Intelligence Officials said|3\\s+US\\s+troopers", "", pak$record)

keepers <- c("NATO", "Additional", "Agents?", "Air", "Anti", "Army", "Arabs?", 
						 "Artillery", "Bomb", "Bunker", "Bus", "Camp", "Cantonment", 
						 "Chairman", "Chairperson",	"Christian", "Corps", "Council", 
						 "Depot", "Deobandi", "Drone", "Fighter", "Force", "Forces", 
						 "Foreign", "Frontier", "Fort", "Frontier", "Girls", "Governor", 
						 "Government", "Guards", "Headquarters", "Hellfire", "Hospital", 
						 "Hotel", "House", "Judge", "Justice", "LI", "L(e|E)J", "Militants", 
						 "Military", "Minister", "Militia", "Naval", "Navy", "Officer", 
						 "Officers", "Officials", "Operation", "Operations", "Ordnance", 
						 "Police", "Policem.n", "Predator", "Refugee", "Rangers?",
						 "Scouts", "Security", "Shias?", "Soldiers", "SquadStudents", 
						 "Sunnis?", "Terrorist", "TTP", "Taliban", "Unit", "United States", 
						 "U//s+S", "Uzbeks?", "Shiites?", "\\b[[:upper:]]+\\b")

pak$record <- gsub(paste("\\b(", 
												 paste(keepers, sep = "", collapse = "|"),
												 ")\\b", 
												 sep = ""), 
									 "\\L\\1", 
									 pak$record, perl = T)
pak$record <- gsub("(aboard|about|above|across|after|against|along|amid|among|around|as|before|behind|below|beneath|besides?|beyond|concerning|considering|despite|down|during|except(ing)?|excluding|following|for|from|in(to|side)?|like|minus|near|off?|on(to)?|opposite|outside|over|past|per|plus|regarding|round|since|than|through|to(wards?)?|under(neath)?|unlike|until|up(on)?|versus|via|with(in|out)?|when|where)?\\s*\\b[[:upper:]]\\w+\\b", "", 
									 pak$record, perl = T)

pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# get rid of numbers not having to do with casualties
pak$record <- gsub("\\d+(\\s*\\d{2})?\\s*(am|pm|hrs)\\b", "", pak$record, ignore.case = T)
pak$record <- gsub("\\d+\\s+0+", "", pak$record)
pak$record <- gsub("(\\d+)\\s+\\d+", "\\1", pak$record)
pak$record <- gsub("\\d+\\s*s\\b", "", pak$record)
pak$record <- gsub("(\\d+)\\s+(to|or)\\s+\\d+", "\\1", pak$record)
pak$record <- gsub("between\\s+(\\d+)\\s+and\\s+\\d+", "\\1", pak$record)
pak$record <- gsub("(\\d+\\s+)?\\d+\\s*(st|nd|rd|th)\\b", "", pak$record)
pak$record <- gsub("(?<!rises to )(\\d+\\s+)?\\d+\\s+(at|in)(?! them ((were )?(wounded|injured)))\\b", "", pak$record, ignore.case = T, perl = T)
pak$record <- gsub("(?<!slaughtered)(?<!shot dead)(?<!killing)(?<!killed)(?<!and)\\s+\\d+\\s+of(?! them ((were )?(wounded|injured)))\\b", "", pak$record, ignore.case = T, perl = T)
pak$record <- gsub("(\\d+\\s+)?(\\d+|\\ba\\b|\\ban\\b)\\s*(?<!suicide)(?<!drone)(?<!air)\\s+(incidents|attacks?|strikes?)\\b", "", pak$record, ignore.case = T, perl = T)
pak$record <- gsub("(\\d+\\s+)?(\\d+|\\ba\\b|\\ban\\b)\\s*(attempts?|years?|minutes?|weeks?|months?|seconds?|hours?|days?)\\b", "", pak$record, ignore.case = T)
pak$record <- gsub("(\\d+\\s+)?(\\d+|\\ba\\b|\\ban\\b)\\s*(kilometers?|kilometre|per\\s*cent|kgs?|miles|kms?|(kilom)?m?et..s|(kilo)?gramm?e?s?|mm)\\b", "", pak$record, ignore.case = T)
pak$record <- gsub("(\\d+\\s+)?(\\d+|\\ba\\b|\\ban\\b)\\s*(strategic\\s+mountain\\s+positions?|paramilitary base|Pakistan\\s+Navy\\s+buses?|brigade|armoured personnel carriers?|buses?|wheeler|pick up|vehicles?|helicopters?)\\b", "", pak$record, ignore.case = T)
pak$record <- gsub("(\\d+\\s+)?(\\d+|\\ba\\b|\\ban\\b)\\s*(\\bseparate\\s+places\\b|bunkers?|locations?|storeys?|compounds?|caves?|hideouts?|villages?|houses?|houses?|mosques?|rooms?|districts?|areas?)\\b", "", pak$record, ignore.case = T)
pak$record <- gsub("(\\d+\\s+)?(\\d+|\\ba\\b|\\ban\\b)\\s*(kalashnikovs?|blasts?|magazines?|rounds?|ammunition|light machine guns?|pistols?|rockets?|attacks|mines?|shotguns?|mortars?|missiles?|bombs?|explosives?|rifles?|guns?|bullets|detonators?|grenades?)\\b", "", pak$record, ignore.case = T)
pak$record <- gsub("(\\d+\\s+)?(\\d+|\\ba\\b|\\ban\\b)\\s*(sectors?|different places|regions?|communities|intelligence|sides?|groups?|agencies|murders?|containers?)\\b", "", pak$record, ignore.case = T)
pak$record <- gsub("(\\d+\\s+)?(\\d+|\\ba\\b|\\ban\\b)\\s*(foreign\\s+oil|parking\\s+lots?|tankers?|belonged|bags?|blocks?|wireless|times|small|training|fell|star|caskets?|million|consecutive|yesterday|said|reports)\\b", "", pak$record, ignore.case = T)
pak$record <- gsub("(\\d+\\s+)?(\\d+|\\ba\\b|\\ban\\b)\\s*(sorties?|sects?|yards?|trenches?|axis|wireless\\s+sets?|blew|factions?|pounds?|of\\s+flour|of\\s+shops|since|entry\\s+points|shops?|(different\\s+)?fronts?|homes?|feet|another\\s+city|barrels?|imambargah|partly\\s+demolished|carriages?)\\b", "", pak$record, ignore.case = T)
pak$record <- gsub("\\d+\\s+cadres?", "0 cadres", pak$record)
pak$record <- gsub("\\d{3,}\\s+forces?", "", pak$record)
pak$record <- gsub("from\\s+(a\\s+group\\s+of\\s+)?more\\s+than\\s+\\d+", "", pak$record)
pak$record <- gsub("\\b(unit)\\s+(\\d+)", "", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)
pak$record <- gsub("rescued\\s+\\d+", "", pak$record)
pak$record <- gsub("among\\s+the\\s+(\\d+)\\s+(\\w+)\\s+killed", "\\1 \\2 were killed", pak$record)
pak$record <- gsub("[1234567890]\\s+(\\d+)", " \\1 ", pak$record)
pak$record <- gsub("^tribal leader was", "1 civilian was", pak$record)
pak$record <- gsub("^and were", "2 civilian were", pak$record)

# clean up extranneous descriptions
pak$record <- gsub("(?<!\\d)\\s+(persons|people|casualties|wounded)\\s+(including|mostly)\\s+\\d+\\s+([[:alpha:]]+)(\\sand\\s+(a|an|\\d+)?)?", " \\3 ", pak$record, perl = T)
pak$record <- gsub("^(?<!\\d)\\s+([[:print:]]{0,25})\\s+(including|mostly)\\s+(\\d+)\\s+([[:alpha:]]+)(\\sand\\s+(a|an|\\d+)?)?", " \\3 \\1 ", pak$record, perl = T)
pak$record <- gsub("([[:alpha:]]+)\\s+(including|mostly)\\s+(\\d+)\\s+([[:alpha:]]+)(\\sand\\s+(a|an|\\d+)?)?", " \\1 ", pak$record)
pak$record <- gsub("among\\s+those\\s+(killed|dead|wounded|injured|arrested)\\s+were\\s+\\d+\\s+([[:alpha:]]+|\\d+)(\\sand\\s+(a|an|\\d+)?)?", "", pak$record)
pak$record <- gsub("\\b([[:lower:]]+)were\\b", "\\1 were", pak$record)
pak$record <- gsub("\\b([[:lower:]]+)(militants?|civilians?)\\b", " \\1 \\2 ", pak$record)
pak$record <- gsub("\\b(militants?|civilians?)([[:lower:]]+)\\b", " \\1 \\2 ", pak$record)
pak$record <- gsub("identified(\\s+as\\b)?", "", pak$record)
pak$record <- gsub("identified(\\s+as\\b)?", "", pak$record)
pak$record <- gsub("\\b[[:alpha:]]\\b", "", pak$record)
pak$record <- gsub("\\bnato\\s+(oil\\s+tankers?|trucks?|suppl(y|ies))", "", pak$record)
pak$record <- gsub("(for|of|to|against)(\\s+the)?\\s+nato\\s+(forces?|troops?)", "", pak$record)
pak$record <- gsub("(\\b(us|nato)\\s+and)?\\s*\\b(us|nato)\\s+forces\\s+in", "", pak$record)
pak$record <- gsub("\\bmilitants[[:alpha:]]+", " militants ", pak$record)
pak$record <- gsub("\\b(\\d+)([[:alpha:]]+)\\b", " \\1 \\2 ", pak$record)
pak$record <- gsub("\\b(\\w+)(injured|wounding|injuring)\\b", " \\1 \\2 ", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

#------------------------------------------------------------------------------#

kill_words <- "\\b(killed|shot|burnt|martyred|dead|destroyed|slaughtered|ambushed|executed|assassinated|neutralized|targeted|hit|struck|attacked|shot dead)\\b"
inju_words <- "\\b(injured|wounded|abducted|arrested|kidnapped)\\b"
qualifier_words <- "\\s+(reportedly|reported|subsequently|apparently|reported to have been)?\\s*"

# n-grams
personnel_3gram_1 <- "\\s+(personnel of fc|troops including captain|more security officials|people fc personnel)\\s+"
personnel_3gram_2 <- "\\s+(security|people|persons|frontier)\\s+((para)?military|corps|army|paksfs|force|fc)\\s+(soldiers?|men|personnel|trooper)\\s+"

civilian_3gram_1 <- "\\s+(wom.n|child(ren)?|m.n) and (wom.n|child(ren)?|m.n)\\s+"
civilian_3gram_2 <- "\\s+(students?|members?|civilians?|passerngers?|persons|people) (including|of|mostly) (woman|transvestite|dsp|child(ren)?|boys?|girls?|family|seminary|worshipp?ers?)\\s+"
civilian_3gram_3 <- "\\s+(activists of anp|peace committee members|workers of ngo|persons belonging community|drivers and cleaner)\\s+"

militant_3gram_1 <- "\\s+(suspected|local|other|persons|people|taliban)\\s+(arab|taliban|mostly|many|ttp)\\s+militants?\\s+"
militant_3gram_2 <- "\\s+(taliban|terrorists?|wanted|key|ttp|militants?)\\s+(taliban|militants?|including)\\s+commanders?\\s+"
militant_3gram_3 <- "\\s+(of his handlers|commanders of li|more taliban militant|of his accomplices|taliban militant nationals|heavily armed terrorists|more militant civilian)\\s+"

militia_3gram_1 <- "\\s+(lashkar army men|pro government smen|un tribal militant|volunteers tribal lashkar)\\s+"

personnel_2gram_1 <- "\\s+(frontier|(para)?military|other|police|paramilitary|rangers|fc|army|paksfs|security|persons?)\\s+(officials?|persons?|personnel|soldiers?|police(m.n)?|troops?|troopers?|recruits?)\\s+"
militant_2gram_1 <- "\\s+(militants?|tribal|people|local|imprisoned|other|un|persons|suspected|alleged|suspected|other|li|uzbek|ttp|taliban)\\s+(militants?|terrorists?|fighters?|commanders?|criminals?|taliban)\\s+"
militant_2gram_2 <- "\\s+(terrorists?|militants?)\\s+(bombers?|nationality|nationals?|commanders?|foreigners?|hailing)"
civilian_2gram_1 <- "\\s+(shias?|sunnis?|shiites?|pilgrims|people|persons)\\s+(shias?|sunnis?|shiites?|pilgrims|people|persons)\\s+"
civilian_2gram_2 <- "\\s+(other|more|isi|persons|people|senior)\\s+(civilains?|officials?|community|child(ren)?|workers?|wom.n|students?|relatives?)\\s+"
militia_2gram_1 <- "\\s+((tribesmen|members) lashkar|civilian soldier)\\s+"

personnel_1gram_1 <- "\\s+(soldiers?|police(m.n)?|troopers?|personnel|troo|troopers?|troops?|paksfs|officers?)\\s+"
militant_1gram_1 <- "\\s+(militants?|terrorists?|taliban|accomplices?|combatants?|miscreants?|attackers?|assailants?|criminals?|abductors?)\\s+"
civilian_1gram_1 <- "\\s+(labourers?|civilians?|professors?|officials?|locals?|settlers?|farmers?|bystanders?|parliamentarians?|wom.n|boys?|famil(y|ies)|child(ren)?|passengers?|schoolchild(ren)?|shiites?|shias|sunnis|jawans?|worshippers?)\\s+"

# 3-gram replacements for WEREATTACKED

pak$record <- gsub(paste("(\\d+)\\s+(militant|taliban)\\s+(and|army)\\s+(police(man)?|soldier|officer)\\s+(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 MILITANT WEREATTACKED 1 PERSONNEL WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)\\s+(militant|taliban)\\s+(and|including)\\s+(a |an |the )?(local|foreigner|woman|minor|civilian|villager|child)\\s+(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 MILITANT WEREATTACKED 1 CIVILIAN WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)\\s+(persons|people|civilians?|tribesmen)\\s+(and|including)\\s+(a |an |the )?(police(m.n)?|soldier|paksfs)\\s+(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 CIVILIAN WEREATTACKED 1 PERSONNEL WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)\\s+(persons|people|civilians?|tribesmen)\\s+(and|including)\\s+(a |an |the )?(militant|taliban)\\s+(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 CIVILIAN WEREATTACKED 1 MILITANT WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", personnel_3gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 PERSONNEL WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", personnel_3gram_2, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 PERSONNEL WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_3gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 CIVILIAN WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_3gram_2, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 CIVILIAN WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_3gram_3, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 CIVILIAN WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_3gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 MILITANT WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_3gram_2, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 MILITANT WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_3gram_3, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 MILITANT WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", militia_3gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 MILITIA WEREATTACKED", pak$record)

# 3-gram replacements for WEREINJURED

pak$record <- gsub(paste("(\\d+)\\s+(militant|taliban)\\s+(and|army)\\s+(police(man)?|soldier|officer)\\s+(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 MILITANT WEREINJURED 1 PERSONNEL WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)\\s+(militant|taliban)\\s+(and|including)\\s+(a |an |the )?(local|foreigner|woman|minor|civilian|villager|child)\\s+(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 MILITANT WEREINJURED 1 CIVILIAN WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)\\s+(persons|people|civilians?|tribesmen)\\s+(and|including)\\s+(a |an |the )?(police(m.n)?|soldier|paksfs)\\s+(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 CIVILIAN WEREINJURED 1 PERSONNEL WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)\\s+(persons|people|civilians?|tribesmen)\\s+(and|including)\\s+(a |an |the )?(militant|taliban)\\s+(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 CIVILIAN WEREINJURED 1 MILITANT WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", personnel_3gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 PERSONNEL WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", personnel_3gram_2, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 PERSONNEL WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_3gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 CIVILIAN WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_3gram_2, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 CIVILIAN WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_3gram_3, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 CIVILIAN WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_3gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 MILITANT WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_3gram_2, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 MILITANT WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_3gram_3, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 MILITANT WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", militia_3gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 MILITIA WEREINJURED", pak$record)

# 2-gram replacements for WEREATTACKED
pak$record <- gsub(paste("(\\d+)", personnel_2gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 PERSONNEL WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_2gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 MILITANT WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_2gram_2, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 MILITANT WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_2gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 CIVILIAN WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_2gram_2, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 CIVILIAN WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", militia_2gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 MILITIA WEREATTACKED", pak$record)

# 2-gram replacements for WEREINJURED
pak$record <- gsub(paste("(\\d+)", personnel_2gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 PERSONNEL WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_2gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 MILITANT WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_2gram_2, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 MILITANT WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_2gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 CIVILIAN WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_2gram_2, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 CIVILIAN WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", militia_2gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 MILITIA WEREINJURED", pak$record)

#	1-gram replacements for WEREATTACKED
pak$record <- gsub(paste("(\\d+)", personnel_1gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 PERSONNEL WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_1gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 MILITANT WEREATTACKED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_1gram_1, "(were|was|is|are)", 
												 qualifier_words, kill_words, sep=""), 
									 "\\1 CIVILIAN WEREATTACKED", pak$record)

# 1-gram replacements for WEREINJURED
pak$record <- gsub(paste("(\\d+)", personnel_1gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 PERSONNEL WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", militant_1gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 MILITANT WEREINJURED", pak$record)

pak$record <- gsub(paste("(\\d+)", civilian_1gram_1, "(were|was|is|are)", 
												 qualifier_words, inju_words, sep=""), 
									 "\\1 CIVILIAN WEREINJURED", pak$record)

# flag drones
pak$drone_flag <- grepl("\\bdrones?\\b", pak$original)
pak$drone_flag[grepl("\\b(nato|us|cia|united states|us led)\\s+(forces?|troops?|helicopters?|jets?)", pak$original, ignore.case = T)] <- TRUE
pak$drone_flag[grepl("\\b(operated|united states|unmanned|predator|pilot\\s?less|controlled|spy|aerial|united)\\s+(vehicle|air\\s?craft|planes?|missiles?|predator)\\b", pak$original, ignore.case = T)] <- TRUE
pak$drone_flag[grepl("\\b(operated|united states|unmanned|predator|pilot\\s?less|controlled|spy|drone)\\s+(strikes?)\\b", pak$original, ignore.case = T)] <- TRUE
pak$drone_flag[grepl("\\b(drones?|suspected\\s+missiles)\\b", pak$original, ignore.case = T)] <- TRUE
pak$drone_flag[grepl("(over|about)\\s+(the\\s+)?DRONE", pak$original, ignore.case = T)] <- FALSE
pak$drone_flag[grepl("hours\\s+after\\s+(a\\s+)?DRONE", pak$original, ignore.case = T)] <- FALSE
pak$drone_flag[grepl("if\\s+the\\s+DRONE", pak$original, ignore.case = T)] <- FALSE
pak$drone_flag[grepl("suicide|blast|IED|roadside\\s+bomb", pak$original, ignore.case = T)] <- FALSE
pak$drone_flag[grepl("drones?\\s+(fired|killed|carried\\s+out|hit)", pak$original, ignore.case = T)] <- TRUE
pak$drone_flag[grepl("^A US drone strike", pak$original, ignore.case = T)] <- TRUE

# flag military operations
pak$operations_flag <- grepl("\\b(bombarded|ripped|raided|advanc(e|ing)?|shelling|(re)?gained control|operati|offensive|strikes|combat|joint|clearance|cordon|(fighter\\s*)?jets?|helicopters?(gun\\s+ships?)?|cobra|planes?|gunships?|air\\s?strikes?|missile\\s?strikes?|operations?|choppers?|artillery|tanks?|rocket\\s?(attacks|strikes)|raids?)\\b", pak$original, ignore.case = T)
pak$operations_flag[grepl("\\b(secured|consolidat(ed|ing)|launched|conducted|backed|conducted|strike)\\s+by\\s+(the\\s+)?(PERSONNEL)", pak$original, ignore.case = T)] <- TRUE
pak$operations_flag[grepl("\\b(aerial\\s+bomb(ings?|ardments?)|(ground|air)\\s+attacks?|stepped\\s+up|crack\\s*downs?|aerial\\s+firing|advanced|military\\s+actions?|raids?|hideouts?|aerial\\s+bombings?|jet\\s*fighters?|operations?|war\\s*planes?|positions|strongholds?|training camps?)\\b", pak$original, ignore.case = T)] <- TRUE
pak$operations_flag[grepl("\\bSFs?\\b", pak$original) &
											grepl("\\b(shelling|shelled|pounded|commanders|hideouts?)\\b", pak$original)] <- TRUE

# flag clashes
pak$clash_flag <- grepl("\\b(in\\s+fighting|retaliat\\w+|street\\s+battles?|fighting\\s+(intensified|continued)|rival|renewed|traded|fighting\\s+erupted|cross\\s*fire|clash(es|ed)?|gun\\s*?(battle|fight)|encounter|exchanged (gun)?fire)\\b", pak$original, perl = T, ignore.case = T)
pak$clash_flag[grepl("\\b(ethnic\\s+violence|encounters?|stiff\\s+resistance|(sectarian|religious)\\s(tensions?|killings?|incidents?|fighting|violence|attacks?|CLASH|ATTACKED))\\b", pak$original, ignore.case = T)] <- TRUE
pak$clash_flag[grepl("both sides\\s*(attacked|opened\\s+fire)", pak$original, ignore.case = T)] <- TRUE
pak$clash_flag[grepl("gun\\s+battles|fresh\\s+fighting|during\\s+fighting|exchange\\s+of\\s+((mortar|gun)\\s+)?(fire|attack)|shootout", pak$original, ignore.case = T)] <- TRUE

# flag militant attacks
pak$militant_flag <- grepl("\\b(exploded|bomb\\s+attack|roadside\\s+remote|devices?|remote\\s+controlled\\s+device|blasts?|bomb(ings?|ers?)?|explosive(s| device)?|ieds?|land\\s*mines?|landmines?|explosions?|roadside\\s+(blasts?|bombs?|explos(ives?|ions?))|blasts?)\\b", pak$original, ignore.case = T)
pak$militant_flag[grepl("\\b((car|woman|female|teenaged|suspected|double|brazen|clad)\\s)*suicide(\\s(attack(er)?s?|bombers?|car( bomb(ing)?)?s?|blasts?|missions?|explosion))*\\b", pak$original, ignore.case = T)] <- TRUE
pak$militant_flag[grepl("\\b(train(ed|s)?\\sSUICIDE)\\b", "", pak$original, ignore.case = T)] <- FALSE
pak$militant_flag[grepl("\\bSUICIDE(\\s(masterminds?|jacket( material)?s?|vests?))\\b", "", pak$original, ignore.case = T)] <- FALSE
pak$militant_flag[grepl("blew (himself|herself|themselves) up", pak$original, ignore.case = T)] <- TRUE
pak$militant_flag[grepl("\\b(hotel|after\\s+accusing|public\\s+meeting|cobblers\\s+shop|grenade\\s+attack|tea\\s*shop|foot\\s+patrol(ling)?|spies|spying|shot\\s+dead(?! by SF)|attack\\s+by\\s+unidentified|rocket\\s+attacks?|security\\s+posts?|blasts?|convoys?|targeted\\s+killings?|executed?|(rocket|mortar)\\s+attacks?|van\\s+of\\s+police(men)?|car\\s+carrying\\s+(the\\s+)?police(men)?|ambush(ed)?|bus\\s*stop|((security\\s+)?(out|check))\\s*(posts?|points?)|(?<!limits of )(\\w+\\s+)?police\\s*stations?|PAF\\s+bases?|(Army|Coast\\s+Guards)\\s+camps?|hand\\s+grenades?|stations?|forts?|facilit(ies|y)|installations?|patrols?|convoys?)\\b", pak$original, ignore.case = T, perl = T)] <- TRUE
pak$militant_flag[grepl("by\\s+(a\\s+group\\s+of\\s+)?unidentified\\s+(armed\\s+)?(militants|assailants)", pak$original, ignore.case = T)] <- TRUE
pak$militant_flag[grepl("unidentified\\s+(armed\\s+)?(militants|assailants)\\s+(opened\\sfire|shot|killed)", pak$original, ignore.case = T)] <- TRUE
pak$militant_flag[grepl("\\bbomb\\b", pak$original, ignore.case = T) & 
										grepl("\\bhit\\s+\\w+\\s+vehicle\\b", pak$original, ignore.case = T)] <- TRUE
pak$militant_flag[grepl("\\bambush\\b", pak$original, ignore.case = T) & 
										grepl("\\bheavy\\s+weapon\\b", pak$original, ignore.case = T)] <- TRUE
pak$militant_flag[grepl("fired\\s+from\\s+a\\s+(Predator\\s+)?drone|drone\\s+attack\\s+killed", pak$original)] <- FALSE

# standardize drones (includes nato strikes done not necessarily by UAVs)
pak$record <- gsub("\\bdrone\\b", " DRONE", pak$record)
pak$record <- gsub("\\b(nato|us|cia|united states|us led)\\s+(forces?|troops?|helicopters?|jets?)", " DRONE ", pak$record)
pak$record <- gsub("\\b(operated|united states|unmanned|predator|pilot\\s?less|controlled|spy|aerial|united)\\s+(vehicle|air\\s?craft|planes?|missiles?|predator)\\b", " DRONE ", pak$record) 
pak$record <- gsub("\\b(operated|united states|unmanned|predator|pilot\\s?less|controlled|spy|drone)\\s+(strikes?)\\b", " DRONE ", pak$record)
pak$record <- gsub("\\b(drones?|suspected\\s+missiles)\\b", " DRONE ", pak$record)
pak$record <- gsub("(over|about)\\s+(the\\s+)?DRONE", "", pak$record)
pak$record <- gsub("hours\\s+after\\s+(a\\s+)?DRONE", "", pak$record)
pak$record <- gsub("if\\s+the\\s+DRONE", "", pak$record)
pak$record <- gsub("DRONE\\s+DRONE", " DRONE ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# standardize suicide attacks
pak$record <- gsub("\\b((car|woman|female|teenaged|suspected|double|brazen|clad)\\s)*suicide(\\s(attack(er)?s?|bombers?|car( bomb(ing)?)?s?|blasts?|missions?|explosion))*\\b", " SUICIDE ", pak$record)
pak$record <- gsub("\\b(train(ed|s)?\\sSUICIDE)\\b", "", pak$record)
pak$record <- gsub("\\bSUICIDE(\\s(masterminds?|jacket( material)?s?|vests?))\\b", "", pak$record)
pak$record <- gsub("blew (himself|herself|themselves) up", " SUICIDE ", pak$record)
pak$record <- gsub("SUICIDE\\s+SUICIDE", " SUICIDE ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# standardize militia information
pak$record <- gsub("(\\b(pro\\s+government|militiam.n|(tribal\\s+)?(volunteers?|razakars?)|community|village|anti ttp|pro government|local|anti taliban|militia|armed|rival|tribal)\\s)?\\b(lashkar|militia(m.n)?|tribesmen)\\b(\\s(community|commander|tribal|militia|army|force|lashkar|members?|leaders?|PERSONNEL|volunteers?|force|police|fighters|policem.n|army)\\b)?|anti\\s+MILITANT\\s+force", " MILITIA ", pak$record)
pak$record <- gsub("(\\d+)\\s+volunteers?\\s+of[[:print:]]+MILITIA", "\\1 MILITIA ", pak$record) 
pak$record <- gsub("MILITIA\\s+MILITIA", " MILITIA ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# standardize militant descriptors
pak$record <- gsub("(?<!MILITIA )\\b(extremists?|(suspected|key)\\s+(al|militants?)|heavily\\s+armed|alleged\\s+gangsters|fanatics?|combatants?|arabs?|suspects?|uzbeks?|abductors?|cadres?|attackers|armed m.n|armed motorcyclists?|fighters|armed (men|intruders)|ttp|taliban|al\\s?qaida|\\bli\\b|\\bbla\\b|\\blej\\b|\\bblf\\b|islamists?|gunm.n|terrorists?|militants?|assailants?|accomplices?|miscreants?|insurgents?|criminals?)\\b(?! MILITIA)", " MILITANT ", pak$record, perl = T)
#pak$record <- gsub("(?<!MILITIA )\\d+\\s+MILITANT\\s+\\d+\\s+(?!each)\\w+(\\s+and\\s+\\d+\\s+\\w+)?(?! MILITIA)", "", pak$record, perl = T)
pak$record <- gsub("(deputy)\\s+MILITANT", " MILITANT", pak$record)
pak$record <- gsub("MILITANT\\s+MILITANT", " MILITANT ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# standardize personnel
pak$record <- gsub("and\\s+his\\s+(\\d+)\\s+guards?", "\\1 CIVILIAN", pak$record) 
pak$record <- gsub("(?<!MILITIA )(?<!MILITANT )\\b(sho|frontier|paksfs|paf|security|(para)?\\s?military|army|(frontier )?corps?|personnel|sfs?( fighters?)?|deputy|stations? guards?|deputies|inspectors?|constabulary|coprs?|crops|air\\s?force)\\b(?! (MILITIA|MILITANT))", " PERSONNEL ", pak$record, perl = T)
pak$record <- gsub("(?<!MILITIA )(?<!MILITANT )\\b(wardens?|trainees?|guards?|armed forces|recruits?|soldiers?|soliders?|constables?|police(m.n)?|rangers?|officers?|captains?|colonels?|troopers?|commanders?|troops?)\\b(?! (MILITIA|MILITANT))", " PERSONNEL ", pak$record, perl = T)
pak$record <- gsub("PERSONNEL\\s+uniforms", "MILITANT", pak$record)
pak$record <- gsub("PERSONNEL\\s+PERSONNEL", " PERSONNEL ", pak$record)
pak$record <- gsub("\\d+\\s+PERSONNEL\\s+official\\s+said", "", pak$record)
pak$record <- gsub("fired\\s+by\\s+(the\\s+)?PERSONNEL(\\s+forces?)?(\\s+PERSONNEL)?", "", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# standarize civilians
pak$record <- gsub("(\\d+)\\s+people\\s+(were\\s+)?(persons|followers?|slain\\s+spies|killed|wounded|injured)\\s+(PERSONNEL|MILITIA|MILITANT)", "\\1 \\4 were \\3", pak$record)
pak$record <- gsub("(?<!MILITIA )(?<!MILITANT )(?<!PERSONNEL )\\b(occupants?|aides?|captured|reporters?|occupants?|watch\\s*m.n|devotees?|volunteers?|abductees?|body\\s*guards?|foreigners?|politicians?|(an )?aide|teenagers?|shiites?|doctors?|coal miners?|abducted|colleagues?|mourners?|nationalists|shias|sunnis|hostages?|activists?|facilitators?|prisoners?|committee|community|family|womenchildren|(truck\\s+)?drivers?|boys?|minors?|smen|passers?|farmers?|locals?|relatives?|press|jawans?|servants?|guests?|female|elders?|officials?|associates?|youths?|staffs?|students?|deobandi|traders?|fathers?|nomads?|(prominent )?tribal elders?|wi(fe|ves)|sons?|chiefs?|of (\\w+ )?aides|labou?rers?|commuters?|members?|leaders?|alleged sp(y|ies)|groups? of|m(a|e)ners?|tribesm.n|motorcyclists?|ministers?|members? of a family|passengers?|persons?|people|civilians?|wom.n( workers?| employees?)?|children|professors?|girls?|wife|wives|child|canadians?|school\\s?children|minors|engineers?|employees?|residents?|bystanders?|workers?|worshipp?ers?|brothers?|nephews?|teachers?|nationals?|supporters?|players?|cricketers?|pilgrims?)\\b(?! (MILITIA|MILITANT|PERSONNEL))", " CIVILIAN ", pak$record, perl = T)
pak$record <- gsub("(?<!MILITIA )(?<!MILITANT )(?<!PERSONNEL )(?<!lost their )\\blives\\b", " CIVILIAN ", pak$record, perl = T)
pak$record <- gsub("(\\d+)\\s+(men|people)\\b", "\\1 CIVILIAN", pak$record)
pak$record <- gsub("CIVILIAN\\s+CIVILIAN", " CIVILIAN ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# standardize bodies
pak$record <- gsub("\\b(bullet\\s+riddled\\s+)?(heads?|bod(y|ies|yparts|y parts?)|severed heads?|beheaded bodies)\\b", " BODY ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# get rid of ambiguous place and vehicle names
pak$record <- gsub("\\b(positions|check\\s*points?|strongholds?|residential|outposts?|offices?|hideouts?|centers?|centres?|training camps?|stations?|precincts?|patrols?|headquarters?|check\\s*(point|post)|barricades?|posts?|forts?|check\\s+ posts?|hospital|buildings?|bases?|facility|facilities|installations?|camps?)\\b", "LOCATION", pak$record)
pak$record <- gsub("((a |the )?moving)?\\s*\\b(vans?|patrols?|vehicles?|convoys?|jeeps?|cars?|motorcycles?|trucks?|vans?)\\b", "VEHICLE", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# standardize attack language
pak$record <- gsub(paste("(were|was|is|are)", qualifier_words, kill_words, sep=""),
									 " WEREATTACKED ", pak$record)
pak$record <- gsub(paste("(were|was|is|are)", qualifier_words, inju_words, sep=""),
									 " WEREINJURED ", pak$record)
pak$record <- gsub("(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+toll\\s+rises\\s+to\\s+(\\d+)", "\\2 \\1 WEREATTACKED", pak$record)
pak$record <- gsub("(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+killing\\s+(\\d+)\\s+of\\s+them", "\\2 \\1 WEREATTACKED", pak$record)
pak$record <- gsub("killing\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+and\\s+(injuring|wounding|arresting)\\s+(\\d+)\\s+others", "\\1 \\2 WEREATTACKED \\4 \\2 WEREINJURED", pak$record)
pak$record <- gsub("(?<!\\d)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+was\\s+also\\s+killed\\s+and\\s+another\\s+(injured|wounded)", " 1 \\1 WEREATTACKED 1 \\1 WEREINJURED", pak$record, perl = T)
pak$record <- gsub("(?<!\\d)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+was\\s+also\\s+killed\\s+and\\s+0\\s+(injured|wounded)", " 1 \\1 WEREATTACKED 0 \\1 WEREINJURED", pak$record, perl = T)
pak$record <- gsub("\\b((are|were|was|is)\\s*?(reported\\s+to\\s+have)?)?\\s*(slaughtered|killed|ambushed|executed|assassinated|neutralized|targeted|hit|struck|destroyed|attacked|shot dead)\\b", " ATTACKED ", pak$record)
pak$record <- gsub("\\bATTACKED\\s+(the\\s+)?(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+LOCATION", "", pak$record)
pak$record <- gsub("\\b(are|were|was|is)\\s*?(reported\\s+to\\s+have\\s+been|reportedly)?\\s*(slaughtered|killed|ambushed|executed|assassinated|neutralized|targeted|hit|struck|destroyed|attacked|shot dead)\\b", " WEREATTACKED ", pak$record)
pak$record <- gsub("lost their lives|(blew|blown) (himself|themselves) up", " WEREATTACKED ", pak$record)
pak$record <- gsub("\\bdied\\b", "WEREATTACKED", pak$record)
pak$record <- gsub("\\b(conceded|suffer(ed|ing))\\s+(\\d+)\\s+casualties", "\\1 WEREATTACKED", pak$record)
pak$record <- gsub("\\b(continued( to)?|have)?\\s*?(ambush(ed|ing)?|storm(ed|ing)?|execut(e|ed|ing)?|assassinat(e|ed|ing)?|bombard(ed|ing)?|detonat(ed|ing)|ripped|raid(ed|ing)?|neutraliz(e|ed|ing)?|chas(e|ed|ing)?|operations|kill(ing|ed)?|target(ing|ed)?|\\bhit\\b|struck|stiking|destroy(ing|ed)?|pound(ing|ed)?|slamm(ing|ed)?|bomb(ing|ed)|shell(ed|ing)?|fir(ing|ed)?|unloaded|attack(ing|ed)?|opened fire|shot dead|repulsed?|behead(ing|ed))\\b", " ATTACKED ", pak$record)
pak$record <- gsub("\\b(were|was|is|are|an|been|being|be)\\s*(also)?\\s*ATTACKED\\b", " WEREATTACKED ", pak$record)
pak$record <- gsub("\\b((are|were|was)\\s*?(injured|wounded|arrested))|(sustain(ed)?|suffer(ed)?) injuries\\b", " WEREINJURED ", pak$record)
pak$record <- gsub("\\b(\\d+)\\s*?((injured|wounded|arrested)|(sustain(ed)?|suffer(ed)?) injuries)\\b", " \\1 WEREINJURED ", pak$record)
pak$record <- gsub("\\b(wounded|injured|injuring|wounding|arresting) ((atleast|another) \\d+|\\d+) ([[:alpha:]]+)", "\\2 \\4 WEREINJURED ", pak$record)
pak$record <- gsub("\\b(injured|wounded|sustained\\s+[[:lower:]]+\\s+injuries)\\b", " WEREINJURED ", pak$record)
pak$record <- gsub("\\b(injuring|arresting|wounding|arrested)\\s+(\\d+)\\s+others", " \\1 others WEREINJURED ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)
pak$record <- gsub("(\\d+)\\s+(beheaded|casualt(y|ies)|fatalit(y|ies)|deaths?)", "\\1 WEREATTACKED", pak$record)
pak$record <- gsub("(WEREINJURED\\s+)?succumbed\\s+to\\s+injuries", "WEREATTACKED", pak$record)
pak$record <- gsub("among\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+ATTACKED", "\\1 \\2 WEREATTACKED", pak$record)
pak$record <- gsub("of\\s+their\\s+own\\s+WEREINJURED", "", pak$record)
pak$record <- gsub("were\\s+also\\s+(ATTACKED)", "WEREATTACKED", pak$record)
pak$record <- gsub("were\\s+also\\s+(WEREATTACKED|WEREINJURED)", "\\1", pak$record)
pak$record <- gsub("\\bATTACKED\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+WEREINJURED", "\\1 \\2 WEREATTACKED \\3 \\4 WEREINJURED", pak$record)
pak$record <- gsub("\\bATTACKED\\s+and\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA)", "\\1 \\2 WEREATTACKED", pak$record)
pak$record <- gsub("\\bATTACKED\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+0\\s+and\\s+WEREINJURED", "\\1 \\2 WEREATTACKED 0 \\2 WEREINJURED", pak$record)

# standardize pakistan military operations
pak$record <- gsub("\\b(bombarded|ripped|raided|advanc(e|ing)?|shelling|(re)?gained control|operati|offensive|strikes|combat|joint|clearance|cordon|(fighter\\s*)?jets?|helicopters?(gun\\s+ships?)?|cobra|planes?|gunships?|air\\s?strikes?|missile\\s?strikes?|operations?|choppers?|artillery|tanks?|rocket\\s?(attacks|strikes)|raids?)\\b", " OPERATIONS ", pak$record)
pak$record <- gsub("\\b(launched|conducted|backed|conducted|strike)\\s+by\\s+(the\\s+)?(PERSONNEL)", " OPERATIONS ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

#standardize clash descriptors
pak$record <- gsub("(street\\s+battles?|fighting\\s+(intensified|continued)|rival|renewed|traded|fighting\\s+erupted|cross\\s*fire|clash(es|ed)?|gun\\s*?(battle|fight)|encounter|exchanged (gun)?fire)", " CLASH ", pak$record, perl = T)
pak$record <- gsub("both sides\\s*ATTACKED", " CLASH ", pak$record)
pak$record <- gsub("exchange\\s+of\\s+((mortar|gun)\\s+)?(fire|ATTACKED)|shootout", " CLASH ", pak$record)
pak$record[!grepl("OPERATIONS", pak$record)] <- 
	gsub("\\bretaliat[[:alpha:]]+\\b", " CLASH ", 
			 pak$record[!grepl("OPERATIONS", pak$record)])
pak$record <- gsub("PERSONNEL\\s+OPERATIONS", " OPERATIONS ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# standardize sectarian clash descriptors
pak$record <- gsub("(sectarian|religious)\\s(tensions?|killings?|incidents?|fighting|violence|attacks?|CLASH|ATTACKED)", " SECTARIAN ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

#standardize IED language
pak$record <- gsub("\\b(roadside\\s+remote|devices?|remote\\s+controlled\\s+device|blasts?|bomb(ings?|ers?)?|explosive(s| device)?|\\bied|land mine|landmine|explosions?|roadside\\s+(blast|bomb|explos(ives?|ions?)))\\b", " IED ", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

militant_att <- grepl("MILITANT\\s+ATTACKED", pak$record) | 
	grepl("\\bof\\s+spying\\b", pak$record) | 
	grepl("rockets?|missiles?|mortars?|grenades?", pak$record)
militia_att <- grepl("MILITIA\\s+ATTACKED", pak$record)
personnel_att <- grepl("PERSONNEL\\s+ATTACKED", pak$record)
civilian_att <- grepl("CIVILIAN\\s+ATTACKED", pak$record)

# Clean up things that were messed up in the conversion
pak$record <- gsub("\\b([[:upper:]]+)\\s+[[:lower:]]{1,2}\\b", "\\1", pak$record)
pak$record <- gsub("\\b[[:lower:]]{1,2}\\s+([[:upper:]]+)\\b", "\\1", pak$record)
pak$record <- gsub("[[:lower:]]+\\s+(aboard|about|above|across|after|against|along|amid|among|around|as|before|behind|below|beneath|besides?|beyond|concerning|considering|despite|down|during|except(ing)?|excluding|following|for|from|in(to|side)?|like|minus|near|off|on(to)?|opposite|outside|over|past|per|plus|regarding|round|since|than|through|to(wards?)?|under(neath)?|unlike|until|up(on)?|versus|via|with(in|out)?|when|where)\\s+(the\\s+)?(CIVILIAN|MILITANT|PERSONNEL|MILITIA)", "", pak$record)
pak$record <- gsub("(?<!\\d )(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+([[:lower:]]+\\s+){0,2}said\\b", "", pak$record, perl = T)
pak$record <- gsub("(\\d+\\s+)?(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+([[:lower:]]+\\s+){0,2}(told|claimed)\\b", "", pak$record, perl = T)
pak$record <- gsub("ATTACKED\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+LOCATION", "", pak$record)
pak$record <- gsub("(?<!MILITIA )\\b(combatants?|arabs?|suspects?|uzbeks?|abductors?|cadres?|attackers|armed m.n|armed motorcyclists?|fighters|armed (men|intruders)|ttp|taliban|al\\s?qaida|\\bli\\b|\\bbla\\b|\\blej\\b|\\bblf\\b|islamists?|gunm.n|terrorists?|militants?|assailants?|accomplices?|miscreants?|insurgents?|criminals?)\\b(?! MILITIA)", " MILITANT ", pak$record, perl = T)
pak$record <- gsub("(?<!MILITIA )\\d+\\s+MILITANT\\s+\\d+\\s+(?!each)\\w+(\\s+and\\s+\\d+\\s+\\w+)?(?! MILITIA)", "", pak$record, perl = T)
pak$record <- gsub("(?<!MILITIA )(?<!MILITANT )\\b(frontier|paksfs|paf|security|(para)?\\s?military|army|(frontier )?corps?|personnel|sfs?( fighters?)?|deputy|stations? guards?|deputies|inspectors?|constabulary|coprs?|crops|air\\s?force)\\b(?! (MILITIA|MILITANT))", " PERSONNEL ", pak$record, perl = T)
pak$record <- gsub("(?<!MILITIA )(?<!MILITANT )\\b(guards?|armed forces|recruits?|soldiers?|soliders?|constables?|police(m.n)?|rangers?|officers?|captains?|colonels?|troopers?|commanders?|troops?)\\b(?! (MILITIA|MILITANT))", " PERSONNEL ", pak$record, perl = T)
pak$record <- gsub("(?<!MILITIA )(?<!MILITANT )(?<!PERSONNEL )\\b(foreigners?|politicians?|(an )?aide|teenagers?|shiites?|doctors?|coal miners?|abducted|colleagues?|mourners?|nationalists|shias|sunnis|hostages?|activists?|facilitators?|prisoners?|committee|community|family|womenchildren|(truck\\s+)?drivers?|boys?|minors?|smen|passers?|farmers?|locals?|relatives?|press|jawans?|abductee|servants?|guests?|female|elders?|officials?|associates?|youths?|staffs?|students?|deobandi|traders?|fathers?|nomads?|(prominent )?tribal elders?|wi(fe|ves)|sons?|chiefs?|of (\\w+ )?aides|labou?rers?|commuters?|members?|leaders?|alleged sp(y|ies)|groups? of|m(a|e)ners?|tribesm.n|motorcyclists?|ministers?|members? of a family|passengers?|persons?|people|civilians?|wom.n( workers?| employees?)?|children|professors?|girls?|wife|wives|child|canadians?|school\\s?children|minors|engineers?|employees?|residents?|bystanders?|workers?|worshipp?ers?|brothers?|nephews?|teachers?|nationals?|supporters?|players?|cricketers?|pilgrims?)\\b(?! (MILITIA|MILITANT|PERSONNEL))", " CIVILIAN ", pak$record, perl = T)
pak$record <- gsub("(?<!MILITIA )(?<!MILITANT )(?<!PERSONNEL )(?<!lost their )\\blives\\b", " CIVILIAN ", pak$record, perl = T)
pak$record <- gsub("(\\w+)?(PERSONNEL|WEREATTACKED|CIVILIAN|WEREINJURED|MILITANT|LOCATION|CLASH|VEHICLE|IED|BODY|DRONE|MILITIA|OPERATIONS|SUICIDESECTARIAN)(\\w+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("\\bA(TTACKED)?ATTACKED", "ATTACKED", pak$record)
pak$record <- gsub("([[:lower:]]+)?([[:upper:]]+)([[:lower:]]+)?", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)
pak$record <- gsub("(a|an)\\s+PERSONNEL\\s+(LOCATION|VEHICLE|spokesperson|press|statement|said|unit|forces?|team|training|led|owned)", "", pak$record)
pak$record <- gsub("(a|an)\\s+PERSONNEL\\s+(MILITANT|CIVILIAN|OPERATIONS)", "\\2", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# Change other stuff
pak$record <- gsub("\\d+\\s+(each\\s+)?from\\s+both\\s+sides", "", pak$record)
pak$record <- gsub("(\\d+)\\s+(CIVILIAN|MILITANT|PERSONNEL|MILITIA)\\s+who\\s+ATTACKED", "\\1 \\2", pak$record)
pak$record <- gsub("^(CIVILIAN|MILITANT|PERSONNEL|MILITIA|DRONE|IED|SUICIDE)\\s+ATTACKED\\s+(\\d+)\\s+(MILITANT|PERSONNEL|MILITIA|BODY|CIVILIAN)", "\\1 \\2 \\3 WEREATTACKED", pak$record)
pak$record <- gsub("(PERSONNEL|WEREATTACKED|CIVILIAN|WEREINJURED|MILITANT|LOCATION|CLASH|VEHICLE|IED|BODY|DRONE|MILITIA|OPERATIONS|SUICIDESECTARIAN|ATTACKED)\\s+\\1", "\\1", pak$record)
pak$record <- gsub("(DRONE|IED|CLASH|OPERATIONS)\\s+ATTACKED\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA)", "\\1 \\2 \\3 WEREATTACKED", pak$record)
pak$record <- gsub("\\bATTACKED\\s+(a|the)?\\s*(VEHICLE|LOCATION)", "", pak$record)
pak$record <- gsub("\\bATTACKED\\s+(a|the)?\\s*(PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA)", "1 \\2 WEREATTACKED ", pak$record)
pak$record <- gsub("during\\s+(ATTACKED)", "", pak$record)
pak$record <- gsub("(\\d+\\s(PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA))\\s(including|mostly|included)\\s(\\d+|a|an|the)\\b(\\s([[:lower:]]+|\\2))+", "\\1", pak$record)
pak$record <- gsub("(leaving\\s+\\d+\\s+(PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA))\\s+dead", "\\1 WEREATTACKED", pak$record)
pak$record <- gsub("(leaving\\s+\\d+\\s+(PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA))\\s+(wounded|injured)", "\\1 WEREINJURED", pak$record)
pak$record <- gsub("[[:upper:]]+\\s*said", "", pak$record)
pak$record <- gsub("(MILITANT|CIVILIANMILITIA)\\s+PERSONNEL", "\\1", pak$record)
pak$record <- gsub("from both sides", "CLASH", pak$record)
pak$record <- gsub("((PERSONNEL )+of (the )?)?\\banti MILITANT", "MILITIA", pak$record, perl =T)
pak$record <- gsub("((PERSONNEL )+of (the )?)?\\bpro MILITANT", "MILITANT", pak$record, perl =T)
pak$record <- gsub("\\bPERSONNEL\\s+(of\\s+the\\s+)?(MILITANT|CIVILIAN|MILITIA)", "\\2", pak$record)
pak$record <- gsub("including\\s+\\d+\\s+(PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA)", "", pak$record)
pak$record <- gsub("^1\\s+PERSONNEL\\s+(?!and)", "PERSONNEL", pak$record, perl = T)
pak$record <- gsub("\\d+(\\s+[[:lower:]]+)*\\s*(SUICIDE|DRONE|IED|CLASH|OPERATIONS)\\s+[[:lower:]]+", "\\2", pak$record)
pak$record <- gsub("(?<!\\bATTACKED )\\d+(\\s+of)?\\s+(MILITANT|PERSONNEL|MILITIADRONE|IED|SUICIDE)(\\s+[[:lower:]]+){,2}\\s*ATTACKED", "\\2 ATTACKED", pak$record, perl = T)
pak$record <- gsub("\\d+\\s+(MILITANT|PERSONNEL|MILITIA)\\s+(LOCATION|VEHICLE|forces|houses?)", "\\1", pak$record)
pak$record <- gsub("(\\d+)\\s+(MILITANT|PERSONNEL|MILITIADRONE|IED|SUICIDE)\\s+\\2\\s+ATTACKED", "\\1 \\2 ATTACKED", pak$record)
pak$record <- gsub("used by(\\s+[[:lower:]]+)*\\s*\\d+\\s+(MILITANT|PERSONNEL|MILITIA)", "", pak$record)
pak$record <- gsub("(\\bATTACKED\\s+\\d+\\s+(MILITANT|PERSONNEL|MILITIACIVILIAN))([[:print:]]+)\\1", "\\1 \\3", pak$record)
pak$record <- gsub("\\b\\d+\\s+of\\s+the\\s+(\\d+)\\b", "\\1", pak$record)
pak$record <- gsub("\\b\\d+\\s+relatives\\s+of\\s+(\\d+)", "\\1 CIVILIAN", pak$record)
pak$record <- gsub("reached\\s+(\\d+)", "\\1 WEREATTACKED", pak$record)
pak$record <- gsub("(\\d+)\\s+were\\s+reportedly\\s*ATTACKED", "\\1 WEREATTACKED", pak$record)
pak$record <- gsub("\\bATTACKED\\s+(\\d+)\\s+and", "\\1 WEREATTACKED", pak$record)
pak$record <- gsub("(\\d+)([[:alpha:]]+)", "\\1 \\2", pak$record)
pak$record <- gsub("([[:alpha:]]+)(\\d+)", "\\1 \\2", pak$record)
pak$record <- gsub("\\d+(\\s+separate)?\\s+(VEHICLE|LOCATION|IED|SUICIDE|CLASH|OPERATIONS|SECTARIAN)", "\\1", pak$record)
pak$record <- gsub("arrested\\s+(\\d+)\\s+others", "\\1 WEREINJURED", pak$record)
pak$record <- gsub("\\bATTACKED\\s+at\\s+least\\s+(\\d+)", "\\1 WEREATTACKED", pak$record)
pak$record <- gsub("at\\s+(\\d+)", "\\1 WEREATTACKED", pak$record)
pak$record <- gsub("\\bATTACKED\\s+(\\d+)\\s+(\\d+)\\s+(\\w+)\\s+WEREINJURED", "\\1 WEREATTACKED \\2 \\3 WEREINJURED", pak$record)
pak$record <- gsub("and\\s+(\\d+)\\s+was", "", pak$record)
pak$record <- gsub("among\\s+the\\s+(\\d+)", "", pak$record)
pak$record <- gsub("of\\s+the\\s+(\\d+)\\s+ATTACKED\\s+\\d+", "", pak$record)
pak$record <- gsub("\\d+\\s+(\\w+)?(\\s+and\\s+\\d+\\s+(\\w+)?)?were\\s+among", "", pak$record)
pak$record <- gsub("(a(nother)?|mother) WEREINJURED", "1 WEREINJURED", pak$record)
pak$record <- gsub("(others|several|many)\\s+(got )?WEREINJURED", "0 WEREINJURED", pak$record)
pak$record <- gsub("(scores) WEREINJURED", "20 WEREINJURED", pak$record)
pak$record <- gsub("and WEREINJURED others", "0 WEREINJURED", pak$record)
pak$record <- gsub("(\\d+) 0\\b", "\\1", pak$record)
pak$record <- gsub("^(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+and\\s+(a|one)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED", "1 \\1 WEREATTACKED 1 \\3 WEREATTACKED", pak$record)
pak$record <- gsub("^(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+and\\s+(others|several|many)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED", "1 \\1 WEREATTACKED 0 \\3 WEREATTACKED", pak$record)
pak$record <- gsub("(a|one)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED", "1 \\2 WEREATTACKED", pak$record)
pak$record <- gsub("(a|one)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED", "1 \\2 WEREINJURED", pak$record)
pak$record <- gsub("(others|several|many)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED", "1 \\2 WEREATTACKED", pak$record)
pak$record <- gsub("(others|several|many)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED", "0 \\2 WEREINJURED", pak$record)
pak$record <- gsub("and\\s+a\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+of(\\s+(the|a))?\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED", "1 \\1 WEREATTACKED", pak$record)
pak$record <- gsub("and\\s+a\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+of(\\s+(the|a))?\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED", "1 \\1 WEREINJURED", pak$record)
pak$record <- gsub("with\\s+almost\\s+\\d+\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+putting\\s+up\\s+(a\\s+)?(fierce\\s+)?resistance", "", pak$record)
pak$record <- gsub("rose\\s+to\\s+\\d+", "", pak$record)
pak$record <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+and\\s+\\d+\\s+(WEREATTACKED|WEREINJURED)", "\\1 \\2 \\3", pak$record)
pak$record <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+and\\s+(a|an)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+WEREATTACKED", "\\1 \\2 WEREATTACKED 1 \\4 WEREATTACKED", pak$record)
pak$record <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+and\\s+ATTACKED", "\\1 \\2 WEREATTACKED", pak$record)
pak$record <- gsub("and\\s+ATTACKED\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)", "\\1 \\2 WEREATTACKED", pak$record)
pak$record <- gsub("and\\s+ATTACKED\\s+(\\d+)\\s+[[:lower:]]+", "\\1 WEREATTACKED", pak$record)
pak$record <- gsub("WEREINJURED\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+and\\s+(\\d+)\\s+([[:lower:]]+\\s+)*\\1", "\\2 \\1 WEREINJURED", pak$record)
pak$record <- gsub("^(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+and\\s+([[:lower:]]+\\s+){,2}(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(WEREATTACKED|WEREINJURED)", "1 \\1 \\4 1 \\3 \\4", pak$record)
pak$record <- gsub("(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(a|the)?\\s*(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s*(and)?\\s*(a|the)?\\s*(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(WEREATTACKED|WEREINJURED)", "1 \\1 \\7 1 \\3 \\7 1 \\6 \\7", pak$record)
pak$record <- gsub("(?<=[[:lower:]])\\s*ATTACKED\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s*(?=[[:lower:]])", " \\1 \\2 WEREATTACKED ", pak$record, perl = T)
pak$record <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+ATTACKED\\s+when\\s+(their|his)\\s+VEHICLE\\s+ATTACKED", "\\1 \\2 WEREATTACKED", pak$record)
pak$record <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED\\s+and\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s*(?!WEREINJURED)", "\\1 \\2 WEREATTACKED \\3 \\4 WEREATTACKED ", pak$record, perl = T)
pak$record <- gsub("\\bATTACKED\\s+him\\s*((and\\s+)?(his\\s+)?\\s*CIVILIAN)?\\s*(and\\s+)?(his\\s+)?\\s*CIVILIAN", "2 CIVILIAN WEREATTACKED", pak$record)
pak$record <- gsub("CIVILIAN\\s*((and\\s+)?(his\\s+)?\\s*CIVILIAN)?\\s*(and\\s+)?(his\\s+)?\\s*CIVILIAN\\s+([[:lower:]]+\\s+)?WEREATTACKED", "2 CIVILIAN WEREATTACKED", pak$record)
pak$record <- gsub("a\\s+prominent\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(WEREATTACKED|WEREINJURED)", "1 \\1 \\2", pak$record)
pak$record <- gsub("reporting\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+ATTACKED", "\\1 \\2 WEREATTACKED", pak$record)
pak$record[grepl("SUICIDE", pak$record) & !grepl("\\d", pak$record)] <- 
	gsub("^\\w+", "1 MILITANT WEREATTACKED", 
			 pak$record[grepl("SUICIDE", pak$record) & !grepl("\\d", pak$record)])

pak$record[grepl("(as many |equal number of them )WEREINJURED", pak$record)] <- 
	gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)(\\s+|[[:lower:]]+|\\2)*\\s+WEREATTACKED(\\s+|[[:lower:]]+|\\2)*(as many |equal number (of them )?)WEREINJURED", 
			 "\\1 \\2 WEREATTACKED \\1 \\2 WEREINJURED", 
			 pak$record[grepl("(as many |equal number of them )WEREINJURED", pak$record)])

pak$record[grepl("as many(\\s+|[[:lower:]]+)*(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED", pak$record)] <- 
	gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)(\\s+|[[:lower:]]+|\\2)*\\s+WEREATTACKED(\\s+|[[:lower:]]+|\\2)*as many(\\s+|[[:lower:]]+|\\2)*(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED", 
			 "\\1 \\2 WEREATTACKED \\1 \\6 WEREINJURED", 
			 pak$record[grepl("as many(\\s+|[[:lower:]]+)*(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED", pak$record)])

pak$record <- gsub("((PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA)\\s+WEREATTACKED\\s+and\\s+\\d+\\s+)others\\s+WEREINJURED", "\\1 \\2 WEREINJURED", pak$record)
pak$record <- gsub("((PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA)\\s+WEREATTACKED\\s+and\\s+\\d+\\s+)others", "\\1 \\2", pak$record)
pak$record <- gsub("(PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA)[[:print:]]+WEREATTACKED\\s+and\\s+(\\d+)\\s+others", "\\1 WEREATTACKED and \\2 \\1", pak$record)
pak$record <- gsub("ATTACKED\\s+(\\d+)\\s+(MILITANT|PERSONNEL|CIVILIANMILITIA)\\s+(and\\s+)?WEREINJURED\\s+(another\\s+)?(\\d+)", "\\1 \\2 WEREATTACKED \\5 \\2 WEREINJURED", pak$record)
pak$record <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+ATTACKED\\s+and\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)?\\s*WEREINJURED", "\\1 \\2 WEREATTACKED \\3 \\4 WEREINJURED", pak$record)
pak$record <- gsub("(\\d+)([[:alpha:]]+)", "\\1 \\2", pak$record)
pak$record <- gsub("([[:alpha:]]+)(\\d+)", "\\1 \\2", pak$record)
pak$record <- gsub("^(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)([[:print:]]+)more\\s+th(an|at)?\\s+(\\d+)\\s+WEREATTACKED\\s+0\\s+WEREINJURED", "\\1 \\2 WEREATTACKED \\3 \\5 \\2 WEREINJURED", pak$record)
pak$record <- gsub("more\\s+th(an|at)?\\s+(\\d+)\\s+WEREATTACKED\\s+0\\s+WEREINJURED", "\\2 WEREINJURED", pak$record)
pak$record <- gsub("^(the|a|an)?\\s*(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+and\\s+(the|a|an)?\\s*(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+WEREATTACKED", "1 \\2 WEREATTACKED 1 \\4 WEREATTACKED", pak$record)
pak$record <- gsub("involved\\s+in\\s+(the|a|an)?\\s*(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+(WEREATTACKED|WEREINJURED)", "", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

militant_words <- grepl("\\bMILITANT\\b", pak$record)  
militia_words <- grepl("\\bMILITIA\\b", pak$record)
personnel_words <- grepl("\\bPERSONNEL\\b", pak$record)
civilian_words <- grepl("\\bCIVILIAN\\b", pak$record)

#------------------------------------------------------------------------------#
# Remove all un-flagged (lower-case) words

pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", 
										gsub("[^[:upper:][:digit:][:space:]]", " ", pak$record), 
										perl = T)
pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)

pak$reduced <- gsub("[[:print:]]+(BODY \\d+ [[:alpha:]]+)[[:print:]]+", "\\1", pak$reduced)
pak$reduced <- gsub("ATTACKED\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+((VEHICLE|LOCATION)\\s+)?ATTACKED", "\\1 \\2 WEREATTACKED", pak$reduced)
pak$reduced <- gsub("\\d+\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+((VEHICLE|LOCATION)\\s+)?ATTACKED", "\\1 ATTACKED", pak$reduced)
pak$reduced <- gsub("\\b1\\s+(PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA)\\s+(VEHICLE|LOCATION)", "", pak$reduced)
pak$reduced <- gsub("(VEHICLE|LOCATION)\\s+1\\s+(PERSONNEL|MILITANT|CIVILIAN|BODY|MILITIA)", "", pak$reduced)
pak$reduced <- gsub("(VEHICLE|LOCATION)", "", pak$reduced)

# Add in WEREATTACKED where it can be reasonably inferred
pak$reduced[grepl("\\b(SUICIDE|OPERATIONS|IED|CLASH|DRONE|SECTARIAN)\\b", pak$reduced) &
							!grepl("\\bWEREATTACKED\\b", pak$reduced)] <- 
	gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)(?! WEREINJURED)", "\\1 \\2 WEREATTACKED", 
			 pak$reduced[grepl("\\b(SUICIDE|OPERATIONS|IED|CLASH|DRONE|SECTARIAN)\\b", pak$reduced) &
			 							!grepl("\\bWEREATTACKED\\b", pak$reduced)], perl = T)

# Create indices for different types of attacks
pak$suicide <- grepl("\\bSUICIDE\\b", pak$reduced)
pak$operations <- grepl("\\bOPERATIONS\\b", pak$reduced)
pak$ied <- grepl("\\bIED\\b", pak$reduced)
pak$clash <- grepl("\\bCLASH\\b", pak$reduced)
pak$drone <- grepl("\\bDRONE\\b", pak$reduced)
pak$sectarian <- grepl("\\bSECTARIAN\\b", pak$reduced)
pak$reduced <- gsub("\\b(SUICIDE|OPERATIONS|IED|CLASH|DRONE|SECTARIAN)\\b", "", pak$reduced)
pak$reduced <- gsub("CIVILIAN PERSONNEL", "CIVILIAN", pak$reduced, perl = T)
pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl = T)

# loop through changes to appropriately convert "ATTACKED" to "WEREATTACKED"
before_char <- rep(0, nrow(pak))
after_char <- rep(1, nrow(pak))
n <- 0

while(mean(before_char == after_char) < 1){
	before_char <- nchar(pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("\\b(\\d+)([[:alpha:]]+)\\b", " \\1 \\2 ", pak$reduced, perl =T)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced[grepl("\\b(DRONE|IED|SUICIDE|SECTARIAN|CLASH)\\b", pak$reduced)] <- 
		gsub("\\bATTACKED\\b", " ", 
				 pak$reduced[grepl("\\b(DRONE|IED|SUICIDE|SECTARIAN|CLASH)\\b", pak$reduced)])
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("(PERSONNEL|CIVILIAN|MILITANT|MILITIA|BODY)\\sATTACKED\\s(\\d+\\s(PERSONNEL|CIVILIAN|MILITANT|MILITIA|BODY))", "\\2 \\1 WEREATTACKED ", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("(\\d+\\s(PERSONNEL|CIVILIAN|MILITANT|MILITIA|BODY)\\s(WEREATTACKED|WEREINJURED))([[:print:]]+)\\1", "\\1 \\4", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("\\bATTACKED\\s(\\d+\\s(PERSONNEL|CIVILIAN|MILITANT|MILITIA|BODY))(?! WEREINJURED)", "\\1 WEREATTACKED", pak$reduced, perl = T)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("(?<!\\d\\s)(PERSONNEL )?OPERATIONS (ATTACKED)?", "OPERATIONS ", pak$reduced, perl =T)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("\\bATTACKED([[:print:]]+WEREATTACKED)", "\\1", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	#pak$reduced <- gsub("([[:print:]]+\\s)\\bATTACKED([[:print:]]+\\bATTACKED)", "\\2 WEREATTACKED \\1", pak$reduced)
	#pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("([[:print:]]+)?(WEREATTACKED)([[:print:]]+)\\bATTACKED([[:print:]]+)?", "\\1 \\2 \\3 \\4", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("([[:print:]]+)?(WEREINJURED)([[:print:]]+)\\bATTACKED([[:print:]]+)?", "\\1 \\2 \\3 \\4", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("MILITANT CIVILIAN ATTACKED (\\d+) (\\d+) WEREINJURED", "\\1 CIVILIAN WEREATTACKED \\2 CIVILIAN WEREINJURED MILITANT", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("BODY (\\d+) (PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY) ATTACKED", "BODY \\1 \\2 WEREATTACKED", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("(\\d+) BODY (PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY) ATTACKED", "BODY \\1 \\2 WEREATTACKED", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("BODY (\\d+) (PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)[[:print:]]+\\bATTACKED", "BODY \\1 \\2 WEREATTACKED", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("(\\b\\w+)(?<!WERE)ATTACKED", "\\1", pak$reduced, perl = T)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)
	pak$reduced <- gsub("(PERSONNEL|MILITANT|CIVILIAN|MILITIA|WEREATTACKED|WEREINJURED)\\s+\\1", "\\1", pak$reduced)
	pak$reduced <- gsub("(\\d+ )(PERSONNEL |MILITANT |CIVILIAN |MILITIA  |BODY )(WEREATTACKED )(\\d+ )(WEREINJURED)", 
											"\\1\\2\\3\\4\\2\\5", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl = T)
	
	pak$reduced <- gsub("(\\d+ )(PERSONNEL |MILITANT |CIVILIAN |MILITIA  |BODY )(WEREATTACKED )(\\d+ )(\\2 )(?!WEREINJURED)", 
											"\\1\\2\\3\\4\\5 WEREINJURED", pak$reduced, perl = T)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl = T)
	pak$reduced <- gsub("(\\d+)\\s+(\\d+)", "\\1", pak$reduced)
	
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED\\s+(\\d+)\\s+WEREINJURED", "\\1 \\2 WEREATTACKED \\3 \\2 WEREINJURED", pak$reduced)
	pak$reduced <- gsub("(?<!\\d )(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(\\d+)\\s+WEREATTACKED", "\\2 \\1 WEREATTACKED", pak$reduced, perl = T)
	pak$reduced <- gsub("(?<!\\d )(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(\\d+)\\s+WEREINJURED", "\\2 \\1 WEREINJURED", pak$reduced, perl = T)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(\\d+)\\s+WEREINJURED", "\\1 \\2 WEREATTACKED \\3 \\2 WEREINJURED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED\\s+\\d+\\s+WEREATTACKED", "\\1 \\2 WEREATTACKED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED\\s+\\d+\\s+WEREINJURED", "\\1 \\2 WEREINJURED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED", "\\1 \\2 WEREATTACKED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED", "\\1 \\2 WEREINJURED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED", "\\1 \\2 WEREATTACKED \\3 \\4 WEREATTACKED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED", "\\1 \\2 WEREINJURED \\3 \\4 WEREINJURED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED", "\\1 \\2 WEREATTACKED \\3 \\4 ATTACKED \\5 \\6 WEREATTACKED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED", "\\1 \\2 WEREINJURED \\3 \\4 WEREINJURED \\5 \\6 WEREINJURED", pak$reduced)
	pak$reduced <- gsub("((PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(WEREATTACKED|WEREINJURED))\\s+\\1", "\\1", pak$reduced)
	pak$reduced <- gsub("^(\\d+)\\s+WEREATTACKED\\s+(\\d)+\\s+WEREINJURED\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)", "\\1 \\3 WEREATTACKED \\2 \\3 WEREINJURED", pak$reduced)
	pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl = T)
	pak$reduced <- gsub("^(\\d+)\\s+(WEREATTACKED|WEREINJURED)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)", "\\1 \\3 \\2", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+WEREATTACKED\\s+(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREINJURED", "\\1 \\3 WEREATTACKED \\2 \\3 WEREINJURED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+\\1\\s+(WEREATTACKED|WEREINJURED)", "\\1 \\2 \\3", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED\\s+WEREINJURED\\s+(\\d+)", "\\1 \\2 WEREATTACKED \\3 \\2 WEREINJURED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED(\\s+|[[:upper:]]+)+(\\d+)\\s+WEREINJURED", "\\1 \\2 WEREATTACKED \\4 \\2 WEREINJURED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+WEREATTACKED\\s+\\1\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+WEREATTACKED", "\\1 \\2 WEREATTACKED", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(WEREATTACKED|WEREINJURED)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+\\2", "\\1 \\3 \\2", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(WEREATTACKED|WEREINJURED)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)", "\\1 \\3 \\2", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+([[:print:]]+)\\1\\s+\\2", "\\1 \\2 \\3", pak$reduced)
	pak$reduced <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+([[:print:]]+)\\1\\s+\\2", "\\1 \\2 \\3", pak$reduced)
	pak$reduced <- gsub("BODY\\s+(\\d+)", "\\1 BODY", pak$reduced)
	pak$reduced <- gsub("WERE\\s+WEREATTACKED", "WEREATTACKED", pak$reduced)
	after_char <- nchar(pak$reduced)
	n <- n + 1
}

pak$reduced[grepl("BODY", pak$reduced) & !grepl("\\d", pak$reduced)] <-
	gsub("BODY", "1 BODY", pak$reduced[grepl("BODY", pak$reduced) & 
																		 	!grepl("\\d", pak$reduced)])
pak$reduced[grepl("^(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(WEREATTACKED|WEREINJURED)", pak$reduced) & 
							!grepl("\\d", pak$reduced)] <-
	gsub("^(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(WEREATTACKED|WEREINJURED)", 
			 "1 \\1 \\2", 
			 pak$reduced[grepl("^(PERSONNEL|MILITANT|CIVILIAN|MILITIA|BODY)\\s+(WEREATTACKED|WEREINJURED)", pak$reduced) & 
			 							!grepl("\\d", pak$reduced)])

have <- grepl("\\d+\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+(WEREATTACKED|WEREINJURED)", pak$reduced) | grepl("\\d+\\s+BODY", pak$reduced) | grepl("no\\s+(casualties|injuries|deaths)", pak$original)

pak$reduced[!have] <- gsub("WEREATTACKED\\s+(\\d+)\\s+CIVILIAN", "\\1 CIVILIAN WEREATTACKED", pak$reduced[!have])
pak$reduced[!have] <- gsub("(\\d+)\\s+(PERSONNEL|MILITANT|CIVILIAN|MILITIA)", "\\1 \\2 WEREATTACKED", pak$reduced[!have])
pak$reduced[!have] <- gsub("(PERSONNEL|MILITANT|CIVILIAN|MILITIA)\\s+(WERE)?ATTACKED\\s+(\\d+)", "\\3 \\1 WEREATTACKED", pak$reduced[!have])
pak$reduced[!have] <- gsub("^(WERE)?ATTACKED\\s+(\\d+)$", "\\2 CIVILIAN WEREATTACKED", pak$reduced[!have])
pak$reduced[!have] <- gsub("^WEREATTACKED\\s*(CIVILIAN)?$", "1 CIVILIAN WEREATTACKED", pak$reduced[!have])

pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", 
										gsub("[^[:upper:][:digit:][:space:]]", " ", pak$reduced), 
										perl = T)
pak$reduced <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$reduced, perl =T)

numbers_record <- str_count(pak$record, "\\b\\d+\\b")
numbers_reduced <- str_count(pak$reduced, "\\b\\d+\\b")

pak$fat_personnel <- as.numeric(
	str_extract(pak$reduced,perl("\\d+(?=\\sPERSONNEL\\sWEREATTACKED)")))
pak$fat_militant <- as.numeric(
	str_extract(pak$reduced,perl("\\d+(?=\\sMILITANT\\sWEREATTACKED)")))
pak$fat_civilian <- as.numeric(
	str_extract(pak$reduced,perl("\\d+(?=\\sCIVILIAN\\sWEREATTACKED)")))
pak$fat_militia <- as.numeric(
	str_extract(pak$reduced,perl("\\d+(?=\\sMILITIA\\sWEREATTACKED)")))

pak$inj_personnel <- as.numeric(
	str_extract(pak$reduced,perl("\\d+(?=\\sPERSONNEL\\sWEREINJURED)")))
pak$inj_militant <- as.numeric(
	str_extract(pak$reduced,perl("\\d+(?=\\sMILITANT\\sWEREINJURED)")))
pak$inj_civilian <- as.numeric(
	str_extract(pak$reduced,perl("\\d+(?=\\sCIVILIAN\\sWEREINJURED)")))
pak$inj_militia <- as.numeric(
	str_extract(pak$reduced,perl("\\d+(?=\\sMILITIA\\sWEREINJURED)")))

pak$body <- as.numeric(
	str_extract(pak$reduced,perl("\\d+(?=\\sBODY)")))


pak$cas_personnel <- !is.na(pak$fat_personnel) | !is.na(pak$inj_personnel)
pak$cas_militant <- !is.na(pak$fat_militant) | !is.na(pak$inj_militant)
pak$cas_civilian <- !is.na(pak$fat_civilian) | !is.na(pak$inj_civilian)
pak$cas_militia <- !is.na(pak$fat_militia) | !is.na(pak$inj_militia)
pak$cas_body <- !is.na(pak$body)

have_numbers <- pak$cas_personnel | 
	pak$cas_militant | 
	pak$cas_civilian |
	pak$cas_militia |
	pak$cas_body


TYPE <- rowSums(pak[,c("suicide", "operations", "ied", "clash", "drone", "sectarian")])!=0

remove <- (pak$cas_body & !pak$cas_personnel & !pak$cas_militant & 
					 	!pak$cas_civilian & !pak$cas_militia) |
	(!pak$cas_personnel & !pak$cas_militant & 
	 	!pak$cas_civilian & !pak$cas_militia)

# p_p <- !remove & pak$operations & 
# 	pak$cas_personnel & 
# 	!(pak$clash | pak$sectarian)
# p_n <- !remove & pak$operations & 
# 	(pak$cas_militant | pak$cas_civilian | pak$cas_militia) & 
# 	!(pak$clash | pak$sectarian)
# m_p <- !remove & (pak$suicide | pak$ied) & 
# 	pak$cas_personnel & 
# 	!(pak$clash | pak$sectarian)
# m_n <- !remove & (pak$suicide | pak$ied) & 
# 	(pak$cas_militant | pak$cas_civilian | pak$cas_militia) & 
# 	!(pak$clash | pak$sectarian)
# u_p <- !remove & (pak$clash | pak$sectarian) & 
# 	pak$cas_personnel
# u_n <- !remove & (pak$clash | pak$sectarian) & 
# 	(pak$cas_militant | pak$cas_civilian | pak$cas_militia)
# d_a <- !remove & pak$drone & 
# 	!(pak$suicide | pak$ied) & 
# 	(pak$cas_body | pak$cas_personnel | pak$cas_militant | pak$cas_civilian  | 
# 	pak$cas_militia)

p_p <- !remove & pak$operations_flag & 
	pak$cas_personnel & 
	!pak$clash_flag & !pak$militant_flag & !pak$drone_flag
p_n <- !remove & pak$operations_flag & 
	(pak$cas_militant | pak$cas_civilian | pak$cas_militia) & 
	!pak$clash_flag & !pak$militant_flag & !pak$drone_flag
m_p <- !remove & pak$militant_flag & 
	pak$cas_personnel & 
	!pak$clash_flag & !pak$operations_flag & !pak$drone_flag
m_n <- !remove & pak$militant_flag & 
	(pak$cas_militant | pak$cas_civilian | pak$cas_militia) & 
	!pak$clash_flag & !pak$operations_flag & !pak$drone_flag
u_p <- !remove & (pak$clash_flag | (pak$operations_flag & pak$militant_flag)) & 
	pak$cas_personnel
u_n <- !remove & (pak$clash_flag | (pak$operations_flag & pak$militant_flag)) & 
	(pak$cas_militant | pak$cas_civilian | pak$cas_militia)
d_a <- !remove & pak$drone_flag & 
	(pak$cas_body | pak$cas_personnel | pak$cas_militant | pak$cas_civilian  | 
	 	pak$cas_militia)



sum(p_p | p_n | m_p | m_n | u_p | u_n | d_a | remove)

have <- p_p | p_n | m_p | m_n | u_p | u_n | d_a | remove

summary_w1 <- sapply(list(p_p, p_n, m_p, m_n, u_p, u_n, d_a, remove,
													p_p | p_n | m_p | m_n | u_p | u_n | d_a | remove), 
										 sum)

p_p[!have & personnel_att & pak$cas_personnel & !militant_att & !militia_att] <- TRUE
p_n[!have & personnel_att & pak$cas_militant & !militant_att & !militia_att] <- TRUE
m_p[!have & militant_att & pak$cas_personnel & !personnel_att & !militia_att] <- TRUE
m_n[!have & militant_att & pak$cas_militant & !personnel_att & !militia_att] <- TRUE

sum(p_p | p_n | m_p | m_n | u_p | u_n | d_a | remove)

have <- p_p | p_n | m_p | m_n | u_p | u_n | d_a | remove

summary_w2 <- sapply(list(p_p, p_n, m_p, m_n, u_p, u_n, d_a, remove,
													p_p | p_n | m_p | m_n | u_p | u_n | d_a | remove), 
										 sum)


u_p[!have & pak$cas_personnel] <- TRUE
u_n[!have & (pak$cas_militant | pak$cas_militia | pak$cas_civilian)] <- TRUE

sum(p_p | p_n | m_p | m_n | u_p | u_n | d_a | remove)

have <- p_p | p_n | m_p | m_n | u_p | u_n | d_a | remove

summary_w3 <- sapply(list(p_p, p_n, m_p, m_n, u_p, u_n, d_a, remove,
													p_p | p_n | m_p | m_n | u_p | u_n | d_a | remove), 
										 sum)

pak$p_p <- p_p
pak$p_n <- p_n
pak$m_p <- m_p
pak$m_n <- m_n
pak$u_p <- u_p
pak$u_n <- u_n
pak$d_a <- d_a

pak$district[pak$province == "contestedareas"] <- "unknown"

pak$date <- as.Date(paste(pak$year, pak$month, pak$day, 
													sep = "-"), format = "%Y-%m-%d")

write.csv(pak[!remove & !provs.missing & !dists.missing,], "pak_output.csv", row.names = F)
