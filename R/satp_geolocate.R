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
# Identify province and district for each record
################################################################################

#------------------------------------------------------------------------------#
# Change idiosyncractic location names to more standard province/district names
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)
pak$record <- gsub("baloch[[:alpha:]]+", " Balochistan ", pak$record, ignore.case = T)
pak$record <- gsub("fata\\b|federally administered tribal agency|tribal (region|agency)", " FATA ", pak$record, ignore.case = T)
pak$record <- gsub("nwfp|frontier province| kp|(khyber )?pakhtunkhwa", " NWFP ", pak$record, ignore.case = T)
pak$record <- gsub("kashmir|gilgit", " contestedareas ", pak$record, ignore.case = T)
pak$record <- gsub("dera ismail di khan", "Dera Ismail Khan", pak$record, ignore.case = T)
pak$record <- gsub("lakki city", "Lakki Marwat", pak$record, ignore.case = T)
pak$record <- gsub("torghar|torgar", "Tor Ghar", pak$record, ignore.case = T)
pak$record <- gsub("rahim khan", "Rahim Yar Khan", pak$record, ignore.case = T)
pak$record <- gsub("national capital", "Islamabad", pak$record, ignore.case = T)
pak$record <- gsub("\\bswa\\b|\\bwana\\b|makeen|sararogha", "South Waziristan", pak$record, ignore.case = T)
pak$record <- gsub("\\bnwa\\b|miranshah|\\mir\\s?ali", "North Waziristan", pak$record, ignore.case = T)
pak$record <- gsub("khurram", "Kurram", pak$record, ignore.case = T)
pak$record <- gsub("mohamad|mohammad", "Mohmand", pak$record, ignore.case = T)
pak$record <- gsub("orajzai", "Orakzai", pak$record, ignore.case = T)
pak$record <- gsub("\\bqila\\b|\\bqulla\\b", "killa", pak$record, ignore.case = T)
pak$record <- gsub("chaman", "Killa Abdullah", pak$record, ignore.case = T)
pak$record <- gsub("bara district", "Khyber", pak$record, ignore.case = T)
pak$record <- gsub("chabahar|pasni", "Gwadar", pak$record, ignore.case = T)
pak$record <- gsub("chagai", "Chaghai", pak$record, ignore.case = T)
pak$record <- gsub("charmang|salarzai", "Bajaur", pak$record, ignore.case = T)
pak$record <- gsub("jaffarabad", "Jafarabad", pak$record, ignore.case = T)
pak$record <- gsub("kachhi", "Bolan", pak$record, ignore.case = T)
pak$record <- gsub("landhi", "Karachi", pak$record, ignore.case = T)
pak$record <- gsub("minawali", "Mianwali", pak$record, ignore.case = T)
pak$record <- gsub("naseerabad", "Nasirabad", pak$record, ignore.case = T)
pak$record <- gsub("naushki|noshki", "Nushki", pak$record, ignore.case = T)
pak$record <- gsub("shawal", "Sahiwal", pak$record, ignore.case = T)
pak$record <- gsub("sheerani", "Sherani", pak$record, ignore.case = T)
pak$record <- gsub("turbat", "Kech", pak$record, ignore.case = T)
pak$record <- gsub("Thall picket", "Hangu", pak$record)
pak$record <- gsub("Parachinar", "Kurram", pak$record)
pak$record <- gsub("Manshera", "Mansehra", pak$record)
pak$record <- gsub("Balochistan MPA", "person", pak$record)
pak$record <- gsub("Jawakai tribal area", "Kohat", pak$record)
pak$record <- gsub("the provincial capital of NWFP", "Peshawar NWFP", pak$record)
pak$record <- gsub("\\b(PIB|Surjani|Gulshan-e-Iqbal|Baldia|Bhimpura)\\b", "Karachi", pak$record)
pak$record <- gsub("\\b(Banaras|SITE|Mominabad|Orangi)\\b", "Karachi", pak$record)
pak$record <- gsub("\\bShaheed Benazirabad\\b", "Nawabshah", pak$record)
pak$record <- gsub("\\bTirah\\b", "Khyber", pak$record)
pak$record <- gsub("\\b(Mamozai|Dabori)\\b", "Orakzai", pak$record)
pak$record <- gsub("\\bJafarabad\\b", "Jaffarabad", pak$record)
pak$record <- gsub("\\bBattagram\\b", "Batagram", pak$record)
pak$record <- gsub("\\bTal\\b", "Tall", pak$record)
pak$record <- gsub("\\b(Dandy Darpakhel|Mosakki|Ghazlami)\\b", "North Waziristan", pak$record)
pak$record <- gsub("\\bChinarak\\b", "Kurram", pak$record)
pak$record <- gsub("\\bHatango\\b", "Hyderabad", pak$record)
pak$record <- gsub("\\bChapri Ferozkhel\\b", "Orakzai", pak$record)
pak$record <- gsub("\\bMamo+nd\\b", "Bajaur", pak$record)
pak$record <- gsub("\\b(Kaniguram|Laddah fort)\\b", "South Waziristan", pak$record)
pak$record <- gsub("\\bBaizai\\b", "Mohman", pak$record)
pak$record <- gsub("\\b(Banjot|Mingora|Kanju|Malam Jabba|Peuchar)\\b", "Swat", pak$record)
pak$record <- gsub("\\bQissa Khwani\\b", "Peshawar", pak$record)
pak$record <- gsub("\\bGaddafi Stadium\\b", "Lahore", pak$record)
pak$record <- gsub("\\bTally area\\b", "Bajaur", pak$record)
pak$record <- gsub("\\bkilla Saifullah\\b", "Zhob", pak$record)
pak$record <- gsub("\\bMulta\\b", "Multan", pak$record) 
pak$record <- gsub("Kooza Bandai", "Kabal", pak$record)
pak$record <- gsub("Totano", "Swat", pak$record)
pak$record <- gsub("Malkand", "Malakand", pak$record)
pak$record <- gsub("Azafi Abadi", "Faisalabad", pak$record)
pak$record <- gsub("Mohman sub-division", "Mohmand", pak$record)
pak$record <- gsub("Jandola", "Tank tribal area", pak$record)
pak$record <- gsub("Razmak", "Waziristan", pak$record)
pak$record <- gsub("the\\s+(provincial\\s+capital|capital\\s+city)\\s+of\\s+NWFP", "Peshawar", pak$record, ignore.case = T)
pak$record <- gsub("\\b(Matani|Garhi\\s?Qamardin)\\b", "Peshawar", pak$record, ignore.case = T)
pak$record <- gsub("\\bTor\\s?Ghar\\b", "Manshera", pak$record, ignore.case = T)
pak$record <- gsub("Dara Adam Khel", "Darra Adam Khel", pak$record)
pak$record <- gsub("one Balochistan MPA identified as Mir Bakhtiar Domki", "one politician", pak$record)
pak$record <- gsub("Lower Kurrams", "Lower Kurram", pak$record)
pak$record <- gsub("the Dawezai area of Mohman sub division", "Mohmand subdivision", pak$record)
pak$record <- gsub("Ziarat District", "Loralai Balochistan", pak$record)
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

# remove non-location info that can confuse location identification
pak$record <- gsub("\\s+", " ", pak$record)
pak$record <- gsub("([[:upper:]][[:lower:]]+)\\s+([[:upper:]][[:lower:]]+)\\s+((r|R)oad)", "", pak$record)
pak$record <- gsub("([[:upper:]][[:lower:]]+\\s)+(b|B)azaar", "", pak$record)
pak$record <- gsub("([[:upper:]][[:lower:]]+\\s)+(b|B)order", "", pak$record)
pak$record <- gsub("([[:upper:]][[:lower:]]+\\s)+(r|R)egiment", "", pak$record)
pak$record <- gsub("([[:upper:]][[:lower:]]+\\s)+(s|S)couts", "", pak$record)
pak$record <- gsub("([[:upper:]][[:lower:]]+\\s)+(t|T)ribe", "", pak$record)
pak$record <- gsub("([[:upper:]][[:lower:]]+\\s)+(g|G)eneral (h|H)ospital", "", pak$record)
pak$record <- gsub("(j|J)udge(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(m|M)ajor( (g|G)eneral)?(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(c|C)aptain(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("((d|D)istrict\\s)?(p|P)olice\\s+((o|O)fficial|(s|S)tation)?(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(p|P)olitical (s|S)ecretary(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(c|C)ommander(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(l|L)ieutenant (g|G)eneral(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(d|d)eputy (s|S)uperintendent of (p|P)olice(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(c|C)adet (c|C)ollege(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(c|C)hief of the(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(h|H)ouse of(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(d|D)eputy (d|D)irector(\\s[[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("(identified as|between)(\\s[[:upper:]][[:lower:]]+ and [[:upper:]][[:lower:]]+)+", "", pak$record)
pak$record <- gsub("going (to|from)(\\s[[:upper:]][[:lower:]]+ (to|from) [[:upper:]][[:lower:]]+)", "", pak$record)
pak$record <- gsub("([[:upper:]][[:lower:]]+\\s)+ area of", "", pak$record)
pak$record <- gsub("\\s+", " ", pak$record)
pak$record <- gsub("on Bannu Road", "", pak$record, ignore.case = T)
pak$record <- gsub("near a market in Matani about 20 kilometres 12 miles south of Peshawar in Khyber Pakhtunkhwa", "Matani Punjab", pak$record)
pak$record <- gsub("said an unnamed source in the provincial capital Quetta", "", pak$record, ignore.case = T)
pak$record <- gsub("the 2 rival groups of Khyber Agency", "", pak$record, ignore.case = T)
pak$record <- gsub("Qadir Gul Hamid Hussain Rafique Taib Khan and Abdul Hanan", "", pak$record, ignore.case = T)
pak$record <- gsub("Saeedur Rehman Hussain Ahmad, Sanaullah Marwat", "", pak$record, ignore.case = T)
pak$record <- gsub("Balishkhel Sadda Karman Para Chamkani Pewar and Teri Mengal", "", pak$record, ignore.case = T) 
pak$record <- gsub("amia Imdadul Alum Mullah Bakhtair Adda, Benazir Bhutto, Mirza Qasim Baig Imambargah", "", pak$record, ignore.case = T)
pak$record <- gsub("Kaza Panga Dher Narai Shaga Treekh Narai Wrasta Bazeena and Shaktoi", "", pak$record, ignore.case = T)
pak$record <- gsub("Osama bin Laden and Ayman Al Zawahiri", "", pak$record, ignore.case = T)
pak$record <- gsub("Laden Zawahiri and Taliban leader Mullah Omar", "", pak$record, ignore.case = T)
pak$record <- gsub("Laden Zawahiri and Taliban leader Mullah Omar", "", pak$record, ignore.case = T)
pak$record <- gsub("Umer Khan", "", pak$record, ignore.case = T)
pak$record <- gsub("revenge (\\w+\\s)+?(at|in) [[:upper:]][[:lower:]]+ in (the)? \\w+\\b", "", pak$record, ignore.case = T)
pak$record <- gsub("by the Interior Ministry in Islamabad", "", pak$record, ignore.case = T)
pak$record <- gsub("warring groups of the Mangal Bagh led Lashkar e Islam and the Mahoob led Ansar ul Islam", "", pak$record, ignore.case = T)
pak$record <- gsub("in Ziarat Mountain Ghaziabad Bagh hill Bhawatha Shawa Farsh Mamad Gatt Alingar Hazeena Chinari and Karer areas", "", pak$record, ignore.case = T)
pak$record <- gsub("a road linking Bajaur Agency with Peshawar", "", pak$record, ignore.case = T)
pak$record <- gsub("Director General of the Inter Services Public Relations ISPR said in Islamabad", "", pak$record, ignore.case = T)
pak$record <- gsub("he told reporters at a press briefing in Islamabad", "", pak$record, ignore.case = T)
pak$record <- gsub("Bajaur Agency Malakand Division and Bannu District during the past", "", pak$record, ignore.case = T)
pak$record <- gsub("rivals belonging to Balishkhel and Khar villages", "", pak$record, ignore.case = T)
pak$record <- gsub("attack by Khar villagers near the Balishkhel checkpoint", "", pak$record, ignore.case = T)
pak$record <- gsub("of the Mangal Bagh led group in Sandapal and Akakhel areas of Tirah Valley", "", pak$record, ignore.case = T)
pak$record <- gsub("The News from an undisclosed location Lashkar e Islam spokesman Zar Khan denied any losses to his group", "", pak$record, ignore.case = T)
pak$record <- gsub("Mastokhel Hamzakhel Ghundikhel Alizai and Shia Bangash of the and Parachamkani Masozai Ali Sherzai Zehmasht Mangal and Sunni Bangash were still at war with 1 another in Balishkhel Sangeena Khar Killay Sadda city Mingak Makhzai and Tangai areas of Lower Kurram", "", pak$record, ignore.case = T)
pak$record <- gsub("Idrees and Sher Qanoon", "", pak$record, ignore.case = T)
pak$record <- gsub("and Younus were found near the Fauji bridge Gul Pir a supporter of Baitullah Mehsud was killed during an operation in the Sheikh Utar area 2 days ago", "", pak$record, ignore.case = T)
pak$record <- gsub("Three persons identified as Gul Mohmand Sher Mohmand and Welayat were killed when stray shells fired from an unknown direction hit their houses in Musa Kor Two persons were killed in shelling in Ghaljo Dara", "", pak$record, ignore.case = T)
pak$record <- gsub("a house in Spim Wam", "", pak$record, ignore.case = T)
pak$record <- gsub("Babri mosque in Baldia town locality of ", "", pak$record, ignore.case = T)
pak$record <- gsub("They were guests of Ayaz younger brother of Riaz", "", pak$record, ignore.case = T)
pak$record <- gsub("were identified as Riazul Hasan Mohmand Husain Shahbaz Lahori Asif and Zahid Ayaz was also killed in the blast while his brother Ibrar was injured", "", pak$record, ignore.case = T)
pak$record <- gsub("Baldia town Superintendent of Police Zahid Husain said Interior Minister Rehman Malik said that the militants were ", "", pak$record, ignore.case = T)
pak$record <- gsub("Manizoo area of", "", pak$record, ignore.case = T)
pak$record <- gsub(" in Hazarnao area", "", pak$record, ignore.case = T)
pak$record <- gsub("Mastung Cadet College", "", pak$record, ignore.case = T)
pak$record <- gsub("Mohmand Munir was going to Quetta", "", pak$record, ignore.case = T)
pak$record <- gsub("the Quetta Karachi highway", "", pak$record, ignore.case = T)
pak$record <- gsub("the spot The cadet was identified as Abu Hamza The 2 other deceased were identified as Mohmand Munir and Ahmed Ali", "", pak$record, ignore.case = T)
pak$record <- gsub("identified as Mohmand Rasul Misal Khan and Khoedad ", "", pak$record, ignore.case = T)
pak$record <- gsub("Ghambeer Gul area near the in Arandu area of", "", pak$record, ignore.case = T)
pak$record <- gsub("between Mangal Bagh", "", pak$record, ignore.case = T)
pak$record <- gsub("The LI militants had reportedly tried to capture some of the posts in Soor Ghar and Alwayi but the Zakha Khel militia repulsed their attack administration sources said", "", pak$record, ignore.case = T)
pak$record <- gsub("in the Khyber Super Market", "", pak$record, ignore.case = T)
pak$record <- gsub("the Musazai and Jogi areas of ", "", pak$record, ignore.case = T)
pak$record <- gsub("identified as Mohmand Arshad alias Taliban", "", pak$record, ignore.case = T)
pak$record <- gsub("Mohmand Jamshed Mohmand Mushtaq and Mohmand Ishtiaq", "", pak$record, ignore.case = T)
pak$record <- gsub("belonging to the Dawoodi Bohra community", "", pak$record, ignore.case = T)
pak$record <- gsub("Qasr e Kutbuddin and Burhani Bagh", "", pak$record, ignore.case = T)
pak$record <- gsub("in Block C of North Nazimabad commonly called Bohra Compound ", "", pak$record, ignore.case = T)
pak$record <- gsub("(A suicide bomber targeted Pakistan s largest procession of Shiite Muslims on their holiest day of Ashura killing at least 30 people and injuring more than 63 persons The blast sparked riots in Karachi)[[:print:]]+", "\\1", pak$record, ignore.case = T)
pak$record <- gsub("had secured a part of the city from the Circuit House to Makan Bagh", "", pak$record, ignore.case = T)
pak$record <- gsub("Deputy Superintendent of Police Lakki Marwat Javed Iqbal Khan ", "", pak$record, ignore.case = T)
pak$record <- gsub("District Police Officer Waqif Khan said ", "", pak$record, ignore.case = T)
pak$record <- gsub("Deputy Superintendent of Police Javed Iqbal who died in a bomb blast along with 3 other policemen in the troubled southern Lakki Marwat district on morning belonged to Makan Bagh in Mingora city", "", pak$record, ignore.case = T)
pak$record <- gsub("in the Michini area ", "", pak$record, ignore.case = T)
pak$record <- gsub("in Shno Ghondai area near Mohmand Agency", "", pak$record, ignore.case = T)
pak$record <- gsub("NWFPs Inspector General of Police Malik Naveed", "", pak$record, ignore.case = T)
pak$record <- gsub("The fighting is still going on a Security official based in the neighbouring garrison city of Kohat said by telephone", "", pak$record, ignore.case = T)
pak$record <- gsub("Militants shot dead 8 policemen near Swat in the", "", pak$record, ignore.case = T)
pak$record <- gsub("on the main Peshawar Kohat road", "", pak$record, ignore.case = T)
pak$record <- gsub("alias Mustafa Mohmand Ahmad", "", pak$record, ignore.case = T)
pak$record <- gsub("carrying them to Mohmand Agency", "", pak$record, ignore.case = T)
pak$record <- gsub("They were identified as Malik Dastan Malik Jamaldar Khan Malik Aman Gul and Malik Sana Gul", "", pak$record, ignore.case = T)
pak$record <- gsub("Assistant Political Agent Muhammad Jamil", "", pak$record, ignore.case = T)
pak$record <- gsub("said local administration official Mohmand Jamil", "", pak$record, ignore.case = T)
pak$record <- gsub("on the main Bannu Kohat road", "", pak$record, ignore.case = T)
pak$record <- gsub("Mandal Dag area of ", "", pak$record, ignore.case = T)
pak$record <- gsub("on the Peshawar Bajaur road", "", pak$record, ignore.case = T)
pak$record <- gsub("to provincial capital Peshawar from the militancy-hit Swat valley", "", pak$record, ignore.case = T)
pak$record <- gsub("Khyber Agency Political Agent Tariq Hayat told Reuters", "", pak$record, ignore.case = T)
pak$record <- gsub("on the Dir Kohistan road", "", pak$record, ignore.case = T)
pak$record <- gsub("were identified as Abdullah Abdul Latif and Mohmand Shoaib Maulvi Gul Nazeer", "", pak$record, ignore.case = T)
pak$record <- gsub("which were busy in the operations against militants in the neighbouring Orakzai Agency", "", pak$record, ignore.case = T)
pak$record <- gsub("Quetta soldier Mohmand Arif", "", pak$record, ignore.case = T)
pak$record <- gsub("in areas bordering Swat", "", pak$record, ignore.case = T)
pak$record <- gsub("on Kalat Street Jail Road", "", pak$record, ignore.case = T)
pak$record <- gsub("on (the )?Kohat road", "", pak$record, ignore.case = T)
pak$record <- gsub("A private TV channel said key TNSM supporter in Shangla District identified as Waliullah Bilgarami has surrendered", "", pak$record, ignore.case = T)
pak$record <- gsub("in the daylong clashes in different areas of the semi FATA", "", pak$record, ignore.case = T)
pak$record <- gsub("and were consolidating their positions in parts of the Swat Valley", "", pak$record, ignore.case = T)
pak$record <- gsub("southeast of provincial capital Quetta", "", pak$record, ignore.case = T)
pak$record <- gsub("Sibi and Quetta Sources said", "", pak$record, ignore.case = T)
pak$record <- gsub("was on its way to Swat Agency", "", pak$record, ignore.case = T) 
pak$record <- gsub("southeast of provincial capital Quetta", "", pak$record, ignore.case = T)
pak$record <- gsub("a telephone call to reporters in Quetta", "", pak$record, ignore.case = T)
pak$record <- gsub("said DSP Shafqatullah Khattak posted in the Anti Terrorist Squad in Bannu District", "", pak$record, ignore.case = T)
pak$record <- gsub("and later shifted to the District Headquarters Hospital in Bannu", "", pak$record, ignore.case = T)
pak$record <- gsub("a spokesman for the FC in provincial capital Quetta", "", pak$record, ignore.case = T)
pak$record <- gsub("in contestedareas Chowk area", "", pak$record, ignore.case = T)
pak$record <- gsub("Karachi SP Kurram Waris", "", pak$record, ignore.case = T)
pak$record <- gsub("Three of the slain militants were identified as Fazal Hussain of Kotta Sher Ali of Aboha and Mohmand Zeb of Charbagh", "", pak$record, ignore.case = T)
pak$record <- gsub("along the border areas with Lower Orakzai Agency of FATA", "", pak$record, ignore.case = T)
pak$record <- gsub("including nephew of LI chief Mangal Bagh", "", pak$record, ignore.case = T)
pak$record <- gsub("and Orakzai Agency Thall Doaba Shahu Khel Kahi Naryab and Kotki", "", pak$record, ignore.case = T)
pak$record <- gsub("and his 4 aides Latif Arsal Khan Mohmand Anwar and Roshan", "", pak$record, ignore.case = T)
pak$record <- gsub("held in the Orakzai Agency of FATA", "", pak$record, ignore.case = T)
pak$record <- gsub("The shura was also reportedly attended by members from Waziristan", "", pak$record, ignore.case = T)
pak$record <- gsub("when they were attacked near Khyber Takya", "", pak$record, ignore.case = T)
pak$record <- gsub("against Taliban hideouts in Malakand division", "", pak$record, ignore.case = T)
pak$record <- gsub("The Senior Superintendent of Police Mian Ghulam Mohmand", "", pak$record, ignore.case = T)
pak$record <- gsub("Addressing a press conference in Islamabad", "", pak$record, ignore.case = T)
pak$record <- gsub("He also said the number of casualties might increase in Buner and Lower Dir Abbas added that the militants were still holding positions at Sultanwas and Pir Baba", "", pak$record, ignore.case = T)
pak$record <- gsub("in different parts of the Kohat region", "", pak$record, ignore.case = T)
pak$record <- gsub("An official statement issued by the Frontier Corps NWFP headquarters", "", pak$record, ignore.case = T)
pak$record <- gsub("on Kohat Road", "", pak$record, ignore.case = T)
pak$record <- gsub("in the town bordering the restive NWFP", "", pak$record, ignore.case = T)
pak$record <- gsub("in Ali Khel area near the border of Bhakkar District", "", pak$record, ignore.case = T)
pak$record <- gsub("on Charsadda Road", "", pak$record, ignore.case = T)
pak$record <- gsub("on Peshawar Kohat road", "", pak$record, ignore.case = T)
pak$record <- gsub("at Bolan canal area", "", pak$record, ignore.case = T)
pak$record <- gsub("at Multan Khanewal road", "", pak$record, ignore.case = T)
pak$record <- gsub("on Sibi Road", "", pak$record, ignore.case = T)
pak$record <- gsub("belonging to Pakistan s Punjab province", "", pak$record, ignore.case = T)
pak$record <- gsub("bound for Quetta from Mongechar", "", pak$record, ignore.case = T)
pak$record <- gsub("residents of Multan District", "", pak$record, ignore.case = T)
pak$record <- gsub("resident of Bahawalpur District", "", pak$record, ignore.case = T)
pak$record <- gsub("on Nowshera Road", "", pak$record, ignore.case = T)
pak$record <- gsub("on Mastung Road", "", pak$record, ignore.case = T)
pak$record <- gsub("of Khuzdar District Police Officer Nazir Kurd", "", pak$record, ignore.case = T)
pak$record <- gsub("who were wanted over an uprising in Swat", "", pak$record, ignore.case = T)
pak$record <- gsub("n an encounter at Pattan town", "", pak$record, ignore.case = T)
pak$record <- gsub("All of my family members are safe and are in Lahore he told a private television channel", "", pak$record, ignore.case = T)
pak$record <- gsub("in the Bannu Division of the North West", "", pak$record, ignore.case = T)
pak$record <- gsub("Islamabad Consequently", "", pak$record, ignore.case = T)
pak$record <- gsub("Upper Dir tribal Lashkar", "", pak$record, ignore.case = T)
pak$record <- gsub("near the Kotki area", "", pak$record, ignore.case = T)
pak$record <- gsub("Baltistan on the Karakoram Highway", "", pak$record, ignore.case = T)
pak$record <- gsub("in Kisankuri area", "", pak$record, ignore.case = T)
pak$record <- gsub("at the ANP Rahim Yar Khan Swati in Qasba Colony", "", pak$record, ignore.case = T)
pak$record <- gsub("between 2 apartment buildings Qasr e Kutbuddin and Burhani Bagh", "", pak$record, ignore.case = T)
pak$record <- gsub("reached the Quetta Karachi highway", "", pak$record, ignore.case = T)
pak$record <- gsub("of the Mangal Bagh led group", "", pak$record, ignore.case = T)
pak$record <- gsub("Director General of the Inter Services Public Relations ISPR said in Islamabad", "", pak$record, ignore.case = T)
pak$record <- gsub("the Mangal Bagh led", "", pak$record, ignore.case = T)
pak$record <- gsub("belonged to Makan Bagh", "", pak$record, ignore.case = T)
pak$record <- gsub("belonging to Tariq group of Darra Adamkhel", "", pak$record)
pak$record <- gsub("the slain commanders were identified as Qari Ikramullah second in command of TTP Amir for Darra Adamkhel and Khyber Agency Sarfraz Ayubi Qari Mubeen Qari Javed and Abdul Manan", "", pak$record)
pak$record <- gsub("launched by the Pakistan Army and Police in Pattan the Military s media centre in Swat said", "", pak$record)
pak$record <- gsub("malakand Deputy Inspector General Idress Khan told reporters", "", pak$record)
pak$record <- gsub("the Judicial Complex on the Khyber Road", "", pak$record)
pak$record <- gsub("Sahibzada Mohmand Anees said", "", pak$record)
pak$record <- gsub("he said Mohmand Tanveer told Dawn", "", pak$record)
pak$record <- gsub("attacked the service centre on Kalat at around 11pm", "", pak$record)
pak$record <- gsub("following a suicide attack on a military camp near the Kohat Tunnel", "", pak$record)
pak$record <- gsub("they said militants had escaped from the area to the Orakzai Agency", "", pak$record)
pak$record <- gsub("might be in retaliation to the recent US air strikes in the Bajaur Agency of the FATA the Tehrik i Taliban in Darra Adamkhel claimed responsibility for the attack", "", pak$record)
pak$record <- gsub("who died in a bomb blast along with three other policemen in the troubled southern Lakki Marwat district on morning in Swat city", "", pak$record)
pak$record <- gsub("the Janikhel area of Frontier Region", "", pak$record)
pak$record <- gsub("were identified as Mohmand Munir and Ahmed Ali", "", pak$record)

# Remove location information preceeded by numbers, directions, etc.
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)
provs.all <- paste(sort(unique(locations$province)), collapse = "|")
dists.all <- paste(sort(unique(locations$district)), collapse = "|")
subds <- sort(unique(locations$subdivision))
subds.all1 <- paste(subds[1:ceiling(length(subds)/2)], collapse = "|")
subds.all2 <- paste(subds[ceiling(length(subds)/2):length(subds)], 
										collapse = "|")

pak$record <- gsub(paste("\\d+\\s+(", provs.all, ")\b", sep = ""), "", 
									 pak$record, ignore.case = T)
pak$record <- gsub(paste("\\d+\\s+(", dists.all, ")\b", sep = ""), "", 
									 pak$record, ignore.case = T)
pak$record <- gsub(paste("\\d+\\s+(", subds.all1, ")\b", sep = ""), "", 
									 pak$record, ignore.case = T)
pak$record <- gsub(paste("\\d+\\s+(", subds.all2, ")\b", sep = ""), "", 
									 pak$record, ignore.case = T)

pak$record <- gsub(paste("from\\s+(the\\s+)?(", provs.all, ")\\b", sep = ""), "", 
									 pak$record, ignore.case = T)
pak$record <- gsub(paste("from\\s+(the\\s+)?(", dists.all, ")\\b", sep = ""), "", 
									 pak$record, ignore.case = T)
pak$record <- gsub(paste("from\\s+(the\\s+)?(", subds.all1, ")\\b", sep = ""), "", 
									 pak$record, ignore.case = T)
pak$record <- gsub(paste("from\\s+(the\\s+)?(", subds.all2, ")\\b", sep = ""), "", 
									 pak$record, ignore.case = T)

pak$record <- gsub(paste("(north|south|west|east)\\s+of\\s+(", provs.all, ")\\b", 
												 sep = ""), 
									 "", pak$record, ignore.case = T)
pak$record <- gsub(paste("(north|south|west|east)\\s+of\\s+(", dists.all, ")\\b", 
												 sep = ""), 
									 "", pak$record, ignore.case = T)
pak$record <- gsub(paste("(north|south|west|east)\\s+of\\s+(", subds.all1, ")\\b", 
												 sep = ""), 
									 "", pak$record, ignore.case = T)
pak$record <- gsub(paste("(north|south|west|east)\\s+of\\s+(", subds.all2, ")\\b", 
												 sep = ""), 
									 "", pak$record, ignore.case = T)

pak$record <- gsub(paste("\\b(a|an)\\s+(", provs.all, ")\\b", sep = ""), "", 
									 pak$record, ignore.case = T)
pak$record <- gsub(paste("\\b(a|an)\\s+(", dists.all, ")\\b", sep = ""), "", 
									 pak$record, ignore.case = T)
pak$record <- gsub(paste("\\b(a|an)\\s+(", subds.all1, ")\\b", sep = ""), "", 
									 pak$record, ignore.case = T)
pak$record <- gsub(paste("\\b(a|an)\\s+(", subds.all2, ")\\b", sep = ""), "", 
									 pak$record, ignore.case = T)

# Remove extra spaces
pak$record <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", pak$record, perl =T)

record2 <- pak$record
#------------------------------------------------------------------------------#
# Create indices for province and district identifiers

# Create regex patterns to search for location informaiton
locs.p <- paste(
	"\\b(", 
	sort(unique(locations$province)), 
	")\\b", sep = "")

locs.pd <- ddply(locations, .variables = c("province"), 
								 .fun = function(x){unique(paste(
								 	"\\b(", 
								 	paste(unique(x$district), collapse = "|"), 
								 	")\\b", sep = ""))})$V1

locs.ps <- ddply(locations, .variables = c("province"), 
								 .fun = function(x){unique(paste(
								 	"\\b(", 
								 	paste(unique(x$subdivision), collapse = "|"), 
								 	")\\b", sep = ""))})$V1

locs.d <- paste(
	"\\b(", 
	sort(unique(locations$district)), 
	")\\b", sep = "")

locs.ds <- ddply(locations, .variables = c("district"), 
								 .fun = function(x){unique(paste(
								 	"\\b(", 
								 	paste(unique(x$subdivision), collapse = "|"), 
								 	")\\b", sep = ""))})$V1

# Get counts of locations in each record
pp <- ldply(locs.p, .fun = function(x){grepl(x, pak$record, ignore.case = T)}, 
						.progress="text")

ppd <- ldply(locs.pd, .fun = function(x){grepl(x, pak$record, ignore.case = T)}, 
						 .progress="text")

pps <- ldply(locs.ps, .fun = function(x){grepl(x, pak$record, ignore.case = T)}, 
						 .progress="text")

dd <- ldply(locs.d, .fun = function(x){grepl(x, pak$record, ignore.case = T)}, 
						.progress="text")

dds <- ldply(locs.ds, .fun = function(x){grepl(x, pak$record, ignore.case = T)}, 
						 .progress="text")

pp1 <- as.data.frame(t(pp), stringsAsFactors = F)
ppd1 <- as.data.frame(t(ppd), stringsAsFactors = F)
pps1 <- as.data.frame(t(pps), stringsAsFactors = F)
dd1 <- as.data.frame(t(dd), stringsAsFactors = F)
dds1 <- as.data.frame(t(dds), stringsAsFactors = F)

colnames(pp1) <- c("balochistan", "contestedareas", "fata", 
									 "kp", "punjab", "sindh")
colnames(ppd1) <- c("balochistan", "contestedareas", "fata", 
										"kp", "punjab", "sindh")
colnames(pps1) <- c("balochistan", "contestedareas", "fata", 
										"kp", "punjab", "sindh")
colnames(dd1) <- gsub(" ", "_", tolower(sort(unique(locations$district))))
colnames(dds1) <- gsub(" ", "_", tolower(sort(unique(locations$district))))

provs <- as.data.frame(matrix(rep(FALSE, nrow(pak)*ncol(pp1)), 
															ncol = ncol(pp1)))
dists <- as.data.frame(matrix(rep(FALSE, nrow(pak)*ncol(dd1)), 
															ncol = ncol(dd1)))
colnames(provs) <- c("balochistan", "contestedareas", "fata", 
										 "kp", "punjab", "sindh")
colnames(dists) <- gsub(" ", "_", tolower(sort(unique(locations$district))))

# create district indices
fata.dists <- colnames(dists) %in% 
	unique(tolower(locations$district[locations$province == "fata"]))
cont.dists <- colnames(dists) %in% 
	unique(tolower(locations$district[locations$province == "contestedareas"]))
balo.dists <- colnames(dists) %in% 
	unique(tolower(locations$district[locations$province == "balochistan"]))
nwfp.dists <- colnames(dists) %in% 
	unique(tolower(locations$district[locations$province == "nwfp"]))
punj.dists <- colnames(dists) %in% 
	unique(tolower(locations$district[locations$province == "punjab"]))
sind.dists <- colnames(dists) %in% 
	unique(tolower(locations$district[locations$province == "sindh"]))
junk.dists <- grepl("xxxxxx", colnames(dists))

fata.dists[junk.dists] <- FALSE
cont.dists[junk.dists] <- FALSE
balo.dists[junk.dists] <- FALSE
nwfp.dists[junk.dists] <- FALSE
punj.dists[junk.dists] <- FALSE
sind.dists[junk.dists] <- FALSE

# Mark fata dists listed in nwfp as fata and also change province to fata

# For province-level indicators
for(i in 1:ncol(dists[, fata.dists])){
	try(pp1[rowSums(dists) == 0 & 
						rowSums(dd1[, fata.dists]) >= 1 & 
						rowSums(dds1[, fata.dists]) >= 1 & 
						dd1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(pp1[rowSums(dists) == 0 & 
						rowSums(dd1[, fata.dists]) >= 1 & 
						rowSums(dds1[, fata.dists]) == 0 & 
						dd1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(pp1[rowSums(dists) == 0 & 
						rowSums(dd1[, fata.dists]) >= 1 & 
						rowSums(dds1[, fata.dists]) >= 1 & 
						dd1[, fata.dists][, i] == T & 
						dds1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(pp1[rowSums(dists) == 0 & 
						rowSums(dd1[, fata.dists]) >= 1 & 
						dd1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(pp1[rowSums(dists) == 0 & 
						rowSums(dd1[, fata.dists]) == 0 & 
						rowSums(dds1[, fata.dists]) >= 1 & 
						dds1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(pp1[rowSums(dists) == 0 & 
						dd1[, fata.dists][, i] == T & 
						dds1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T, "fata"] <- TRUE, silent = T)
}

for(i in 1:ncol(dists[, fata.dists])){
	try(pp1[rowSums(dists) == 0 & 
						rowSums(dd1[, fata.dists]) >= 1 & 
						rowSums(dds1[, fata.dists]) >= 1 & 
						dd1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T & 
						pp1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(pp1[rowSums(dists) == 0 & 
						rowSums(dd1[, fata.dists]) >= 1 & 
						rowSums(dds1[, fata.dists]) == 0 & 
						dd1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T & 
						pp1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(pp1[rowSums(dists) == 0 & 
						rowSums(dd1[, fata.dists]) >= 1 & 
						rowSums(dds1[, fata.dists]) >= 1 & 
						dd1[, fata.dists][, i] == T & 
						dds1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T & pp1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(pp1[rowSums(dists) == 0 & 
						rowSums(dd1[, fata.dists]) >= 1 & 
						dd1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T & 
						pp1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(pp1[rowSums(dists) == 0 & 
						rowSums(dd1[, fata.dists]) >= 0 & 
						rowSums(dds1[, fata.dists]) >= 1 & 
						dds1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T & 
						pp1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(pp1[rowSums(dists) == 0 & 
						dd1[, fata.dists][, i] == T & 
						dds1[, fata.dists][, i] == T & 
						pp1[, "kp"] == T & 
						pp1[, "fata"] == T, "kp"] <- FALSE, silent = T)
}

# For district-level indicators of province
for(i in 1:ncol(dists[, fata.dists])){
	try(ppd1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(ppd1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) == 0 & 
					 	dd1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(ppd1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	dds1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(ppd1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(ppd1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) == 0 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dds1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(ppd1[rowSums(dists) == 0 & 
					 	dd1[, fata.dists][, i] == T & 
					 	dds1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T, "fata"] <- TRUE, silent = T)
}

for(i in 1:ncol(dists[, fata.dists])){
	try(ppd1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T & 
					 	ppd1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(ppd1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) == 0 & 
					 	dd1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T & 
					 	ppd1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(ppd1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	dds1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T & ppd1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(ppd1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T & 
					 	ppd1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(ppd1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 0 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dds1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T & 
					 	ppd1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(ppd1[rowSums(dists) == 0 & 
					 	dd1[, fata.dists][, i] == T & 
					 	dds1[, fata.dists][, i] == T & 
					 	ppd1[, "kp"] == T & 
					 	ppd1[, "fata"] == T, "kp"] <- FALSE, silent = T)
}

# For subdivision-level indicators of province
for(i in 1:ncol(dists[, fata.dists])){
	try(pps1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(pps1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) == 0 & 
					 	dd1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(pps1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	dds1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(pps1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(pps1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) == 0 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dds1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T, "fata"] <- TRUE, silent = T)
	try(pps1[rowSums(dists) == 0 & 
					 	dd1[, fata.dists][, i] == T & 
					 	dds1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T, "fata"] <- TRUE, silent = T)
}

for(i in 1:ncol(dists[, fata.dists])){
	try(pps1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T & 
					 	pps1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(pps1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) == 0 & 
					 	dd1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T & 
					 	pps1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(pps1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	dds1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T & pps1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(pps1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 1 & 
					 	dd1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T & 
					 	pps1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(pps1[rowSums(dists) == 0 & 
					 	rowSums(dd1[, fata.dists]) >= 0 & 
					 	rowSums(dds1[, fata.dists]) >= 1 & 
					 	dds1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T & 
					 	pps1[, "fata"] == T, "kp"] <- FALSE, silent = T)
	try(pps1[rowSums(dists) == 0 & 
					 	dd1[, fata.dists][, i] == T & 
					 	dds1[, fata.dists][, i] == T & 
					 	pps1[, "kp"] == T & 
					 	pps1[, "fata"] == T, "kp"] <- FALSE, silent = T)
}

# Create province indices
for(i in 1:ncol(provs)){
	provs[rowSums(pp1) == 1 & 
					rowSums(ppd1) == 1 & 
					rowSums(pps1) == 1 & 
					pp1[, i] == T & 
					ppd1[, i] == T & 
					pps1[, i] == T, i] <- TRUE
	provs[rowSums(provs) == 0 & 
					rowSums(pp1) == 1 & 
					rowSums(ppd1) == 1 & 
					rowSums(pps1) == 0 & 
					pp1[, i] == T & 
					ppd1[, i] == T, i] <- TRUE
	provs[rowSums(provs) == 0 & 
					rowSums(pp1) == 1 & rowSums(ppd1) == 0 & 
					rowSums(pps1) == 1 & 
					pp1[, i] == T & 
					pps1[, i] == T, i] <- TRUE
	provs[rowSums(provs) == 0 & 
					rowSums(pp1) == 1 & 
					rowSums(ppd1) == 0 & 
					rowSums(pps1) == 0 & 
					pp1[, i] == T, i] <- TRUE
	provs[rowSums(provs) == 0 & 
					rowSums(pp1) == 0 & 
					rowSums(ppd1) == 1 & 
					rowSums(pps1) == 0 & 
					ppd1[, i] == T, i] <- TRUE
	provs[rowSums(provs) == 0 & 
					rowSums(pp1) == 0 & 
					rowSums(ppd1) == 0 & 
					rowSums(pps1) == 1 & 
					pps1[, i] == T, i] <- TRUE
	provs[rowSums(provs) == 0 & 
					rowSums(pp1) == 1 & 
					pp1[, i] == T, i] <- TRUE
	provs[rowSums(provs) == 0 & 
					rowSums(pp1) == 0 & 
					rowSums(ppd1) == 1 & 
					ppd1[, i] == T, i] <- TRUE
	provs[rowSums(provs) == 0 & 
					pp1[, i] == T & 
					ppd1[, i] == T & 
					pps1[, i] == T, i] <- TRUE
}

provs[rowSums(provs)==0 &
				grepl("north waziristan", pak$record, ignore.case = T), "fata"] <- TRUE
provs[rowSums(provs)==0 &
				grepl("south waziristan", pak$record, ignore.case = T), "fata"] <- TRUE
provs[rowSums(provs)==0 &
				grepl("Lal Masjid", pak$record, ignore.case = T), "punjab"] <- TRUE
provs[rowSums(provs)==0 &
				grepl("Rawalpindi", pak$record, ignore.case = T), "punjab"] <- TRUE

dd1[provs[, "fata"] != 0, !fata.dists] <- FALSE
dd1[provs[, "balochistan"] != 0, !balo.dists] <- FALSE
dd1[provs[, "contestedareas"] != 0, !cont.dists] <- FALSE
dd1[provs[, "kp"] != 0, !nwfp.dists] <- FALSE
dd1[provs[, "punjab"] != 0, !punj.dists] <- FALSE
dd1[provs[, "sindh"] != 0, !sind.dists] <- FALSE

dds1[provs[, "fata"] != 0, !fata.dists] <- FALSE
dds1[provs[, "balochistan"] != 0, !balo.dists] <- FALSE
dds1[provs[, "contestedareas"] != 0, !cont.dists] <- FALSE
dds1[provs[, "kp"] != 0, !nwfp.dists] <- FALSE
dds1[provs[, "punjab"] != 0, !punj.dists] <- FALSE
dds1[provs[, "sindh"] != 0, !sind.dists] <- FALSE

for(i in 1:ncol(dists)){
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1) == 1 & 
							rowSums(dds1) == 1 & 
							dd1[, i] == T, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1) == 1 & 
							rowSums(dds1) == 0 & 
							dd1[, i] == T, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1) == 1 & 
							rowSums(dds1) == 1 & 
							dd1[, i] == T & 
							dds1[, i] == T, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1) == 1 & 
							dd1[, i] == T, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1) == 0 & 
							rowSums(dds1) == 1 & 
							dds1[, i] == T, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							dd1[, fata.dists][, i] == T & 
							dds1[, i] == T, i] <- TRUE, silent = T)
}

for(i in 1:ncol(dists[, fata.dists])){
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, fata.dists]) == 1 & 
							rowSums(dds1[, fata.dists]) == 1 & 
							dd1[, fata.dists][, i] == T & 
							provs[, "fata"] == T, fata.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, fata.dists]) == 1 & 
							rowSums(dds1[, fata.dists]) == 0 & 
							dd1[, fata.dists][, i] == T & 
							provs[, "fata"] == T, fata.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, fata.dists]) == 1 & 
							rowSums(dds1[, fata.dists]) == 1 & 
							dd1[, fata.dists][, i] == T & 
							dds1[, fata.dists][, i] == T & 
							provs[, "fata"] == T, fata.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, fata.dists]) == 1 & 
							dd1[, fata.dists][, i] == T & 
							provs[, "fata"] == T, fata.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, fata.dists]) == 0 & 
							rowSums(dds1[, fata.dists]) == 1 & 
							dds1[, fata.dists][, i] == T & 
							provs[, "fata"] == T, fata.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							dd1[, fata.dists][, i] == T & 
							dds1[, fata.dists][, i] == T & 
							provs[, "fata"] == T, fata.dists][, i] <- TRUE, silent = T)
}
for(i in 1:ncol(dists[, nwfp.dists])){
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, nwfp.dists]) == 1 & 
							rowSums(dds1[, nwfp.dists]) == 1 & 
							dd1[, nwfp.dists][, i] == T & 
							provs[, "kp"] == T, nwfp.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, nwfp.dists]) == 1 & 
							rowSums(dds1[, nwfp.dists]) == 0 & 
							dd1[, nwfp.dists][, i] == T & 
							provs[, "kp"] == T, nwfp.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, nwfp.dists]) == 1 & 
							rowSums(dds1[, nwfp.dists]) == 1 & 
							dd1[, nwfp.dists][, i] == T & 
							dds1[, nwfp.dists][, i] == T & 
							provs[, "kp"] == T, nwfp.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, nwfp.dists]) == 1 & 
							dd1[, nwfp.dists][, i] == T & 
							provs[, "kp"] == T, nwfp.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, nwfp.dists]) == 0 & 
							rowSums(dds1[, nwfp.dists]) == 1 & 
							dds1[, nwfp.dists][, i] == T & 
							provs[, "kp"] == T, nwfp.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							dd1[, nwfp.dists][, i] == T & 
							dds1[, nwfp.dists][, i] == T & 
							provs[, "kp"] == T, nwfp.dists][, i] <- TRUE, silent = T)
}
for(i in 1:ncol(dists[, balo.dists])){
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, balo.dists]) == 1 & 
							rowSums(dds1[, balo.dists]) == 1 & 
							dd1[, balo.dists][, i] == T & 
							provs[, "balochistan"] == T, balo.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, balo.dists]) == 1 & 
							rowSums(dds1[, balo.dists]) == 0 & 
							dd1[, balo.dists][, i] == T & 
							provs[, "balochistan"] == T, balo.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, balo.dists]) == 1 & 
							rowSums(dds1[, balo.dists]) == 1 & 
							dd1[, balo.dists][, i] == T & 
							dds1[, balo.dists][, i] == T & 
							provs[, "balochistan"] == T, balo.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, balo.dists]) == 1 & 
							dd1[, balo.dists][, i] == T & 
							provs[, "balochistan"] == T, balo.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, balo.dists]) == 0 & 
							rowSums(dds1[, balo.dists]) == 1 & 
							dds1[, balo.dists][, i] == T & 
							provs[, "balochistan"] == T, balo.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							dd1[, balo.dists][, i] == T & 
							dds1[, balo.dists][, i] == T & 
							provs[, "balochistan"] == T, balo.dists][, i] <- TRUE, silent = T)
}
for(i in 1:ncol(dists[, sind.dists])){
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, sind.dists]) == 1 & 
							rowSums(dds1[, sind.dists]) == 1 & 
							dd1[, sind.dists][, i] == T & 
							provs[, "sindh"] == T, sind.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, sind.dists]) == 1 & 
							rowSums(dds1[, sind.dists]) == 0 & 
							dd1[, sind.dists][, i] == T & 
							provs[, "sindh"] == T, sind.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, sind.dists]) == 1 & 
							rowSums(dds1[, sind.dists]) == 1 & 
							dd1[, sind.dists][, i] == T & 
							dds1[, sind.dists][, i] == T & 
							provs[, "sindh"] == T, sind.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, sind.dists]) == 1 & 
							dd1[, sind.dists][, i] == T & 
							provs[, "sindh"] == T, sind.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, sind.dists]) == 0 & 
							rowSums(dds1[, sind.dists]) == 1 & 
							dds1[, sind.dists][, i] == T & 
							provs[, "sindh"] == T, sind.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							dd1[, sind.dists][, i] == T & 
							dds1[, sind.dists][, i] == T & 
							provs[, "sindh"] == T, sind.dists][, i] <- TRUE, silent = T)
}
for(i in 1:ncol(dists[, punj.dists])){
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, punj.dists]) == 1 & 
							rowSums(dds1[, punj.dists]) == 1 & 
							dd1[, punj.dists][, i] == T & 
							provs[, "punjab"] == T, punj.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, punj.dists]) == 1 & 
							rowSums(dds1[, punj.dists]) == 0 & 
							dd1[, punj.dists][, i] == T & 
							provs[, "punjab"] == T, punj.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, punj.dists]) == 1 & 
							rowSums(dds1[, punj.dists]) == 1 & 
							dd1[, punj.dists][, i] == T & 
							dds1[, punj.dists][, i] == T & 
							provs[, "punjab"] == T, punj.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, punj.dists]) == 1 & 
							dd1[, punj.dists][, i] == T & 
							provs[, "punjab"] == T, punj.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, punj.dists]) == 0 & 
							rowSums(dds1[, punj.dists]) == 1 & 
							dds1[, punj.dists][, i] == T & 
							provs[, "punjab"] == T, punj.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							dd1[, punj.dists][, i] == T & 
							dds1[, punj.dists][, i] == T & 
							provs[, "punjab"] == T, punj.dists][, i] <- TRUE, silent = T)
}

for(i in 1:ncol(dists[, cont.dists])){
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, cont.dists]) == 1 & 
							rowSums(dds1[, cont.dists]) == 1 & 
							dd1[, cont.dists][, i] == T & 
							provs[, "contestedareas"] == T, cont.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, cont.dists]) == 1 & 
							rowSums(dds1[, cont.dists]) == 0 & 
							dd1[, cont.dists][, i] == T & 
							provs[, "contestedareas"] == T, cont.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, cont.dists]) == 1 & 
							rowSums(dds1[, cont.dists]) == 1 & 
							dd1[, cont.dists][, i] == T & 
							dds1[, cont.dists][, i] == T & 
							provs[, "contestedareas"] == T, cont.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, cont.dists]) == 1 & 
							dd1[, cont.dists][, i] == T & 
							provs[, "contestedareas"] == T, cont.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							rowSums(dd1[, cont.dists]) == 0 & 
							rowSums(dds1[, cont.dists]) == 1 & 
							dds1[, cont.dists][, i] == T & 
							provs[, "contestedareas"] == T, cont.dists][, i] <- TRUE, silent = T)
	try(dists[rowSums(dists) == 0 & 
							dd1[, cont.dists][, i] == T & 
							dds1[, cont.dists][, i] == T & 
							provs[, "contestedareas"] == T, cont.dists][, i] <- TRUE, silent = T)
}

dists[rowSums(dists)==0 &
				grepl("north waziristan", pak$record, ignore.case = T), 
			"north.?waziristan"] <- TRUE
dists[rowSums(dists)==0 &
				grepl("south waziristan", pak$record, ignore.case = T), 
			"south.?waziristan"] <- TRUE
dists[rowSums(dists)==0 &
				grepl("Lal Masjid", pak$record, ignore.case = T), 
			"islamabad"] <- TRUE
dists[rowSums(dists)==0 &
				grepl("Rawalpindi", pak$record, ignore.case = T), 
			"rawalpindi"] <- TRUE

# Identify districts that take precedence in the coding when they appear in 
# conjunction with other districts, then code the indices appropriately
dupes_to_swat <- grepl("Swat", pak$record) & 
	rowSums(dists)==0 &
	(grepl("Malakand", pak$record) | 
	 	grepl("Buner", pak$record) |
	 	grepl("Shangla", pak$record) | 
	 	grepl("Dir", pak$record) | 
	 	grepl("Bannu", pak$record))

dupes_to_orakzai <- grepl("Orakzai", pak$record) & 
	rowSums(dists)==0 &
	(grepl("Khyber", pak$record) | 
	 	grepl("Kurram", pak$record) |
	 	grepl("Mohmand", pak$record))

dupes_to_quetta <- grepl("Quetta", pak$record) & 
	rowSums(dists)==0 &
	(grepl("Mastung|Khuzdar", pak$record))

dupes_to_waziristan <- grepl("Waziristan", pak$record) & 
	rowSums(dists)==0 &
	rowSums(dd1+dds1)==0

dupes_to_kech <- grepl("Kech", pak$record) & 
	rowSums(dists)==0 &
	(grepl("Khuzdar", pak$record))

dupes_to_derabugti <- grepl("Dera.?Bugti", pak$record) &
	rowSums(dists)==0 &
	(grepl("Kohlu", pak$record))

dupes_to_bajaur <- grepl("Bajaur", pak$record) &
	rowSums(dists)==0 &
	(grepl("Mohmand", pak$record))

dupes_to_hangu <- grepl("Hangu", pak$record) &
	rowSums(dists)==0 &
	(grepl("Kohat", pak$record))


dists[grepl("Swat", pak$record) & 
				rowSums(dists)==0 &
				(grepl("Malakand", pak$record) | 
				 	grepl("Buner", pak$record) |
				 	grepl("Shangla", pak$record) | 
				 	grepl("Dir", pak$record) | 
				 	grepl("Bannu", pak$record)), "swat"] <- TRUE

dists[grepl("Orakzai", pak$record) & 
				rowSums(dists)==0 &
				(grepl("Khyber", pak$record) | 
				 	grepl("Kurram", pak$record) |
				 	grepl("Mohmand", pak$record) | 
				 	grepl("Bajaur", pak$record)), "orakzai"] <- TRUE

dists[grepl("Quetta", pak$record) & 
				rowSums(dists)==0 &
				(grepl("Mastung|Khuzdar", pak$record)), "quetta"] <- TRUE

provs[grepl("Waziristan", pak$record) & 
				rowSums(dists)==0 &
				rowSums(dd1+dds1)==0, "fata"] <- TRUE

dists[grepl("Waziristan", pak$record) & 
				rowSums(dists)==0 &
				rowSums(dd1+dds1)==0, "north.?waziristan"] <- TRUE

dists[grepl("Kech", pak$record) & 
				rowSums(dists)==0 &
				(grepl("Khuzdar", pak$record)), "kech"] <- TRUE

dists[grepl("Dera.?Bugti", pak$record) &
				rowSums(dists)==0 &
				(grepl("Kohlu", pak$record)), "dera.?bugti"] <- TRUE

dists[grepl("Bajaur", pak$record) &
				rowSums(dists)==0 &
				(grepl("Mohmand", pak$record)), "bajaur"] <- TRUE

dists[grepl("Hangu", pak$record) &
				rowSums(dists)==0 &
				(grepl("Kohat", pak$record)), "hangu"] <- TRUE

provs.missing <- rowSums(provs)==0
dists.missing <- rowSums(dists)==0 & provs[,"contestedareas"]==F

# Collapse binary indicator matrices into vectors of province/district names
provs$unknown <- rep(FALSE, nrow(provs))
provs$unknown[rowSums(provs)==0] <- TRUE
dists$unknown <- rep(FALSE, nrow(dists))
dists$unknown[rowSums(dists)==0] <- TRUE

pak$province <- colnames(provs)[as.matrix(provs) %*% 1:ncol(provs)]
colnames(dists) <- gsub("[[:punct:]]+|district|city|tribal|area", 
												" ", colnames(dists))
colnames(dists) <- gsub("\\s+", " ", colnames(dists))
colnames(dists) <- gsub("^\\s+|\\s+$", "", colnames(dists))
pak$district <- colnames(dists)[as.matrix(dists) %*% 1:ncol(dists)]

#------------------------------------------------------------------------------#
# Diagnostic tools for checking the coding

# prov.check <- pp1+ppd1+pps1
# dist.check <- (dd1+dds1)
# 
# pak$record[rowSums(provs)==0]
# lapply(1:sum(rowSums(provs)==0), function(i){
# 	colnames(provs[which(prov.check[rowSums(provs)==0, ][i, ]!=0)])
# })
# 
# lapply(1:sum(rowSums(provs)!=0 & 
# 	provs[, "contestedareas"] == F & 
# 	rowSums(dists)==0), function(i){
# 		colnames(dists[which(dist.check[rowSums(provs)!=0 & 
# 			provs[, "contestedareas"] == F & 
# 			rowSums(dists)==0, ][i, ]!=0)])
# 	})
# 
# pak$record[rowSums(provs)!=0 & 
# 	provs[, "contestedareas"] == F & 
# 	rowSums(dists)==0]
# colnames(dists[which(dist.check[rowSums(provs)!=0 & 
# 	provs[, "contestedareas"] == F & 
# 	rowSums(dists)==0, ][27, ]!=0)])
# locations[grepl("balo", locations$province),]
# locations[grepl("Zi", locations$district),]
# locations[grepl("Zi", locations$subdivision), ]
# unique(locations[grepl("Bannu", locations$district), "province"])
# 
# 
# 
# locs.remove <- paste(
# 	"\\b",
# 	unique(c(c("Balochistan", "contestedareas", "FATA", "NWFP", 
# 						 "Punjab", "Sindh"),
# 					 unlist(strsplit(gsub("[[:punct:]]+|\\b[[:lower:]]+\\b"," ", 
# 					 										 sort(locations$district)),"\\s+")),
# 					 unlist(strsplit(gsub("[[:punct:]]+|\\b[[:lower:]]+\\b"," ", 
# 					 										 sort(locations$subdivision)),"\\s+")),
# 					 c("Pakistan", "Afghanistan"))),
# 	"\\b", sep = "")
# 
# for(i in 1:length(remove.locs)){
# 	pak$record <- gsub(remove.locs[i], "", pak$record)
# }
# tst <- unique(do.call("c",str_extract_all(pak$record,"[[:upper:]]\\w+")))
# tst.spell <- aspell(as.factor(tolower(tst)),
#                     control = c("--master=en_US --sug-mode=ultra"))
# sort(tst[!(tolower(tst) %in% tst.spell$Original)])
