############
## Scraping each team's offensive and defensive game logs from each season, from pages like this:
## https://www.sports-reference.com/cfb/schools/air-force/2001/gamelog/
############

library(rvest)  # Should be installed along with 'xml2'
library(plotrix) # Also install it please.
library(stringi) # Needed for certain string operations.

###############
### This function allows to scrape the second table from a page like 
###   https://www.sports-reference.com/cfb/schools/air-force/2001/gamelog/.
###
### This second table can't be recognized by basic xml_find_all(read_html(url_link),"//table") command
### as it is (for some reason) commented out in the page source code.
#################

find_extra_table <- function(url_link){
  # Additional tables are within the comment tags -  between "<!--"  and  "-->"
  # Which is why your xpath is missing them.
  
  # Find all the commented nodes
  alt_tables <- xml_find_all(read_html(url_link),"//comment()")
  
  #Among commented nodes, find those containing '<table' reg. expression 
  #raw_parts <- as.character(alt_tables[grep("\\</?table", as.character(alt_tables))])
  raw_parts <- as.character(alt_tables[grep("<table", as.character(alt_tables))])
  
  # Remove the comment begin ("<!--") and end ("-->") tags
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  # Apply what we did for UNcommented table before
  strip_html <- read_html(strip_html)
  table_two <- xml_find_all(strip_html, "//table") 
  class(table_two)
  
  # Obtain the actual contents of the table, wrapped into a LIST of DATA FRAMES
  table_two <- html_table(table_two)
  return(table_two)
}


##########
##########
### For which year do we want the offensive & defensive logs of all teams?
##########
###########

year <- 2017


#######
## Creating folders that will contain the offensive and defensive logs, respectively, 
## for this particular year.
#######

dir.create("Game_Logs")
dir.create(paste("Game_Logs/",year, sep=''))
dir.create(paste("Game_Logs/",year,"/Offense", sep=''))
dir.create(paste("Game_Logs/",year,"/Defense", sep=''))

##########
# First order of business - getting a vector of all FBS team names playing that year
# (keep in mind that certain teams got added or left throughout that 2000-2017 period,
#  so we would have to do that for each year separately)
# We'll scrape it off of one 'Season Summary' table on sports-reference. 
# For example, for year 2001 we can do it from here:
# https://www.sports-reference.com/cfb/years/2001-team-defense.html
##########

## Using a webpage that contains a table with all team names, scraping that table.
url_link <- paste('https://www.sports-reference.com/cfb/years/', year, '-team-defense.html', sep='')
url <- read_html(url_link)
total_table <- html_table(url)[[1]]

# Prettying up the table by making 2nd row a header.
c.names <- total_table[1,]
total_table <- total_table[-1,]
colnames(total_table) <- c.names

# All we really need from this table is school names, hence the School column. 
total_table$School
# See an issue? Where are those occasional "" and "School" values coming from? Check the whole table:
total_table

# That can be easily cleaned for the whole table as follows:
good.ind <- !(total_table$School %in% c("","School"))
total_table_clean <- total_table[good.ind,]
total_table_clean$School
FBS_Team_names <- sort(total_table_clean$School)


## Next, recall the format of school names in the url links: 
#      - all lowercase,
#      - spaces are subbed by "-".
low.names <- tolower(FBS_Team_names)
url.names <- gsub(" ","-",low.names)

# One other aspect is that there shouldn't be any "(" ")" in the name like in miami(oh) or miami(fl),
# hence just replace both "(" and ")" with "". 
# When using either "(" or ")" in a regular expression, keep in mind that those are considered special characters,
# and need to be pre-fixed with escape sequence "\\":

url.names <- gsub("\\(","",url.names)
url.names <- gsub("\\)","",url.names)

print(url.names)

# Next, I will run my two-table scraping procedure for all those names, BUT
# accounting for the fact that there will still be some incorrect url names among the ones we've calculated.

# I will keep the indices of "bad" names in object "bad.ind". Those names will reveal themselves once you witness
# an empty scraped table (length(table_one) == 0), at which point we 
#      - record the index (bad.ind <- c(bad.ind, j)), and 
#      - proceed to the next inde (next).

bad.ind <- NULL

for (j in 1:length(low.names)){
  print(j)
  offense_data <- defense_data <- NULL
  
  ## If team had a "bad" name, the command below 
  url <- paste('https://www.sports-reference.com/cfb/schools/',url.names[j],'/',year,'/gamelog/', sep="")
  print(url)

  ### Scraping the first table on the page (offensive logs).
  
  table_one <- xml_find_all(read_html(url), "//table") %>% html_table
  
  ## If team had a "bad" name, there won't be any table scraped (length(table_one) = 0),
  ## and we can add it to "bad" indices.
  if (length(table_one) == 0) {bad.ind <- c(bad.ind,j); next}
  
  table_one <- table_one[[1]]

  tab.col.names <- table_one[1,]
  table_one <- table_one[-1,]
  colnames(table_one) <- tab.col.names
  
  table_one <- table_one[-nrow(table_one),]
  table_one[,-c(2:5)] <- lapply(table_one[,-c(2:5)],as.numeric)
  table_one[,1] <- url.names[j]
  colnames(table_one)[1] <- "Team"

  offense_data <- table_one
  colnames(offense_data)
  
  
  ### Scraping the second table on the page (defensive logs).
  
  table_two <- find_extra_table(url)[[1]]
  
  # That can be fixed via hard-coding:
  tab.col.names <- table_two[1,]
  table_two <- table_two[-1,]
  colnames(table_two) <- tab.col.names
  rownames(table_two) <- c(1:nrow(table_two))
  
  # Also, we don't quite need the LAST ROW, which is just totals for the year:
  # we can always calculate those on our own.
  table_two <- table_two[-nrow(table_two),]
  
  # Function as numeric comes to rescue:
  table_two[,-c(2:5)] <- lapply(table_two[,-c(2:5)],as.numeric)
  table_two[,1] <- url.names[j]
  colnames(table_two)[1] <- "Team"
  
  defense_data <- table_two
  
  ## Because of "Tot.1" (in offense) vs "TO" (in defense) for denoting turnovers,
  ## with "TO" preferred
  colnames(offense_data) <- colnames(defense_data)
  
  # Save the scraped defensive logs data.
  write.csv(offense_data, file=paste(getwd(),"/Game_Logs/",year,'/Offense/',FBS_Team_names[j],'.csv', sep=''))
  # Save the scraped defensive logs data.
  write.csv(defense_data, file=paste("Game_Logs/",year,'/Defense/',FBS_Team_names[j],'.csv', sep=''))

  offense_data <- read.csv(paste(getwd(),"/Game_Logs/",year,'/Offense/',FBS_Team_names[j],'.csv', sep=''))
  defense_data <- read.csv(paste("Game_Logs/",year,'/Defense/',FBS_Team_names[j],'.csv', sep=''))

  col.bad.names <- c("Att", "Yds", "TD", "Att.1","Yds.1", "Avg", "TD.1", "Yds.2", "Avg.1", "TD.2", "Avg.2", "Yds.3")
  col.good.names <- c("Pass.Att", "Pass.Yds", "Pass.TD", "Rush.Att", "Rush.Yds", "Rush.Avg", "Rush.TD", "Tot.Yds", "Tot.Avg", "Tot.TD", "Pass.Avg", "Pen.Yds")
  
  col.names <- colnames(offense_data)
  colnames(offense_data) <- colnames(defense_data) <- sapply(col.names, function(x) {if (x %in% col.bad.names) return(col.good.names[which(x == col.bad.names)]); return(x)})
  
  # Save the scraped defensive logs data.
  write.csv(offense_data, file=paste(getwd(),"/Game_Logs/",year,'/Offense/',FBS_Team_names[j],'.csv', sep=''))
  # Save the scraped defensive logs data.
  write.csv(defense_data, file=paste("Game_Logs/",year,'/Defense/',FBS_Team_names[j],'.csv', sep=''))
  
}




## Now, what were the bad names?
url.names[bad.ind]


## Bad names are the ones that result from lower-casing the initial team names, but
## that DON'T correspond to URL names.
## Good names are the actual URL names.
## Capital names are the best names for further official use in the modeling,
## here they will be used in the file names.

bad.good.capital.names <- matrix(c(
 "florida-a&m", "florida-am", "Florida A&M",
 "louisiana", "louisiana-lafayette", "Louisiana", 
 "lsu", "louisiana-state", "Louisiana State",
 "ole-miss", "mississippi", "Mississippi", 
 "pitt",  "pittsburgh", "Pittsburgh",
 "smu", "southern-methodist", "Southern Methodist", 
 "texas-a&m", "texas-am", "Texas A&M",
 "uab", "alabama-birmingham", "Alabama-Birmingham",
 "ucf", "central-florida", "Central Florida",
  "unlv", "nevada-las-vegas", "Nevada-Las Vegas",
  "usc", "southern-california", "Southern California", 
  "utep", "texas-el-paso", "Texas-El Paso",
  "utsa","texas-san-antonio", "Texas-San Antonio"
), ncol=3, byrow=T)

## Now, replace the initially bad names, with their correct counterparts (which can be used in url-names).
## That will allow you to scrape the offensive & defensive logs for those teams as well.
url.names[bad.ind] <- sapply(url.names[bad.ind], function(x) bad.good.capital.names[bad.good.capital.names[,1]==x,2])

## There's "Central Florida" in "Opponent" column, instead of "UCF"...
#  and a bunch of other examples...
#  So, instead, just decided to make it all according to the "Opponent" column format
good.capital.names <- sapply(url.names[bad.ind], function(x) bad.good.capital.names[bad.good.capital.names[,2]==x,3])



for (j in 1:length(bad.ind)){
  #for (j in good.names){
  print(j)
  offense_data <- defense_data <- NULL
  
  url <- paste('https://www.sports-reference.com/cfb/schools/',url.names[bad.ind[j]],'/',year,'/gamelog/', sep="")
  print(url)
  
  ### Scraping the first table on the page (offensive logs).
  
  table_one <- xml_find_all(read_html(url), "//table") %>% html_table
  
  ## Just making sure that there won't be any more "bad" names.
  if (length(table_one) == 0) {bad.ind <- c(bad.ind,j); next}
  
  table_one <- table_one[[1]]
  
  tab.col.names <- table_one[1,]
  table_one <- table_one[-1,]
  colnames(table_one) <- tab.col.names
  
  table_one <- table_one[-nrow(table_one),]
  table_one[,-c(2:5)] <- lapply(table_one[,-c(2:5)],as.numeric)
  table_one[,1] <- url.names[bad.ind[j]]
  colnames(table_one)[1] <- "Team"
  
  offense_data <- table_one
  
  ### Scraping the second table on the page (defensive logs).
  
  table_two <- find_extra_table(url)[[1]]
  
  # That can be fixed via hard-coding:
  tab.col.names <- table_two[1,]
  table_two <- table_two[-1,]
  colnames(table_two) <- tab.col.names
  rownames(table_two) <- c(1:nrow(table_two))
  
  # Also, we don't quite need the LAST ROW, which is just totals for the year:
  # we can always calculate those on our own.
  table_two <- table_two[-nrow(table_two),]
  
  # Function as numeric comes to rescue:
  table_two[,-c(2:5)] <- lapply(table_two[,-c(2:5)],as.numeric)
  table_two[,1] <- url.names[bad.ind[j]]
  colnames(table_two)[1] <- "Team"
  
  defense_data <- table_two
  
  ## Because of "Tot.1" (in offense) vs "TO" (in defense) for denoting turnovers,
  ## with "TO" preferred
  colnames(offense_data) <- colnames(defense_data)
  
  # Save the scraped defensive logs data.
  write.csv(offense_data, paste("Game_Logs/",year,'/Offense/',good.capital.names[j],'.csv', sep=''))
  
  # Save the scraped defensive logs data.
  write.csv(defense_data, paste("Game_Logs/",year,'/Defense/',good.capital.names[j],'.csv', sep=''))
  
  ## Re-read, and clean up the column names.
  offense_data <- read.csv(paste("Game_Logs/",year,'/Offense/',good.capital.names[j],'.csv', sep=''))
  defense_data <- read.csv(paste("Game_Logs/",year,'/Defense/',good.capital.names[j],'.csv', sep=''))
  
  col.bad.names <- c("Att", "Yds", "TD", "Att.1","Yds.1", "Avg", "TD.1", "Yds.2", "Avg.1", "TD.2", "Avg.2", "Yds.3")
  col.good.names <- c("Pass.Att", "Pass.Yds", "Pass.TD", "Rush.Att", "Rush.Yds", "Rush.Avg", "Rush.TD", "Tot.Yds", "Tot.Avg", "Tot.TD", "Pass.Avg", "Pen.Yds")
  
  col.names <- colnames(offense_data)
  colnames(offense_data) <- colnames(defense_data) <- sapply(col.names, function(x) {if (x %in% col.bad.names) return(col.good.names[which(x == col.bad.names)]); return(x)})
  
  # Save the scraped defensive logs data.
  write.csv(offense_data, file=paste("Game_Logs/",year,'/Offense/',good.capital.names[j],'.csv', sep=''))
  # Save the scraped defensive logs data.
  write.csv(defense_data, file=paste("Game_Logs/",year,'/Defense/',good.capital.names[j],'.csv', sep=''))
  
}


