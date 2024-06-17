###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed-data folder
#
# Note the ## ---- name ---- notation
# This is done so one can pull in the chunks of code into the Quarto document
# see here: https://bookdown.org/yihui/rmarkdown-cookbook/read-chunk.html
###############################


## ---- packages --------
#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing/cleaning
library(tidyr) #for data processing/cleaning
library(skimr) #for nice visualization of data 
library(here) #to set paths


## ---- loaddata --------
#path to data
#note the use of the here() package and not absolute paths
#data_location <- here::here("data","raw-data","exampledata.xlsx")
#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
#rawdata <- readxl::read_excel(data_location)
# We might also want to load the codebook to look at it
#codebook <- readxl::read_excel(data_location, sheet ="Codebook")
#Loading the Data Sets
#install.packages("RCurl")
#library(RCurl)

# Injury Player Data
#injuries <- read.csv(text = getURL("https://raw.githubusercontent.com/btj5z2/BrodyJohnson-P2-portfolio/main/position_injury_player_2019_2020_update.csv")) #also works but not needed
injuries=read.csv("data/raw-data/position_injury_player_2019_2020_update.csv")

# Cumulative Play data
#pbp <- read.csv(text = getURL("https://raw.githubusercontent.com/btj5z2/BrodyJohnson-P2-portfolio/main/pbp-2019.csv")) #short for play-by-play #also works but not needed
pbp = read.csv("data/raw-data/pbp-2019.csv")

#Player Demographic data
#players <- read.csv(text = getURL("https://raw.githubusercontent.com/btj5z2/BrodyJohnson-P2-portfolio/main/player_position_Rmd1.csv"))
players = read.csv("data/raw-data/players (1).csv")

## ---- exploredata --------
#take a look at the data
dplyr::glimpse(injuries)
  #Summarize "description" into "play_type"
  #Summarize "injury_area" as upper vs lower body into new parameter
  #Create new field calculating age from birthdate (mm/dd/yyyy)
dplyr::glimpse(pbp) 
  #Convert "gamedate" to date (from char), 
  #remove empty columns (X, X.1,X.2,X.3), 
  #Remove observations in "Description" indicating end of qtr/game, 2-minute warnings, and timeouts 
  #0 values for "Down" indicate kickoffs & PATs. Concert to NA
dplyr::glimpse(players) 
  #Convert height to inches and then numeric. 
  #Character parameters may also need to be converted to factor. 
  #Create new field calculating age from birthdate (yyyy-mm-dd)
  #Combine player positions to compare to injuries data set 

#another way to look at the data
summary(injuries) #meh way of looking at data
#yet another way to get an idea of the data
head(injuries) #meh way of looking at data
#this is a nice way to look at data
skimr::skim(injuries) #Neat way looking at data
skimr::skim(pbp)
skimr::skim(players)
#"Glimpse" into the data is favorite way of initially looking at data

#Creating function to capitalize 'description' variable 
capitalize_words = function(text) {
  mylist = list()
  words = strsplit(text, '\\s+')[[1]] #Split text into words 
  capitalized_words = toupper(words) #Capitalize each word
  for (i in capitalized_words) #Combine words back into list
    mylist = append(mylist, i)
  return(mylist)
}

## ---- cleaninjuriesdata1 --------
#Summarize "description" into "play_type"
d1 = injuries 
d1$play_type=NA
for (i in 1:nrow(d1)) {
  if('EXTRA' %in% capitalize_words(d1$desc[i])) {d1$play_type[i] = 'FG/PAT Attempt or Kickoff'}
  else if('KICK' %in% capitalize_words(d1$desc[i])) {d1$play_type[i] = 'FG/PAT Attempt or Kickoff'}
  else if('FUMBLE' %in% capitalize_words(d1$desc[i])) {d1$play_type[i] = 'Fumble'}
  else if('DEEP' %in% capitalize_words(d1$desc[i])) {d1$play_type[i] = 'Deep Pass'}
  else if('SHORT' %in% capitalize_words(d1$desc[i])) {d1$play_type[i] = 'Short Pass'}
  else if('PUNT' %in% capitalize_words(d1$desc[i])) {d1$play_type[i] = 'Punt'}
  else if('SACK' %in% capitalize_words(d1$desc[i])) {d1$play_type[i] = 'Sack'}
  else if('SCRAMBLES' %in% capitalize_words(d1$desc[i])) {d1$play_type[i] = 'Scramble'}
  else if('TWO-POINT' %in% capitalize_words(d1$desc[i])) {d1$play_type[i] = '2-Point Conversion'}
  else {d1$play_type[i] ='Run'}
} 


## ---- cleaninjuriesdata2 --------
#Summarize "injury_area" as upper vs lower body into new parameter

#First, cleaning raw data for misspelled words, etc.
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'lower bodt', 'lower body')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'lower', 'lower body')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'upper bodt', 'upper body')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'upper bdoy', 'lower body')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'knee, ankle', 'knee/ankle')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'ankle/ achilles', 'ankle')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'lower body/ achilles', 'ankle')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'wrist/hand', 'hand')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'finger', 'hand')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'lower body/knee', 'knee')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'upper body/shoulder', 'shoulder')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'upper body/ elbow', 'elbow')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'upper body/ shoulder', 'shoulder')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'pass rusher', NA)
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'upper body/back', 'back')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'lower back', 'back')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'not shown', NA)
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'head/ neck', 'head/neck')
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'unknown', NA)
d1$injury.area = replace(d1$injury.area, d1$injury.area == 'unkown', NA)

#Next, create a new variable summarizing injury_area by only lower body, upper body, head, and NAs
d1$injury.area.new=NA
headwords = c('head', 'head/neck')
upperbodywords = c('upper body', 'arm', 'hand', 'shoulder', 'back/neck', 'eye', 'back', 'elbow', 'face', 'neck', 'ribs')
lowerbodywords = c('lower body', 'knee', 'ankle', 'calf', 'foot', 'thigh', 'groin', 'knee/ankle', 'hamstring', 'hip')
upperlower = c('upper and lower body')
for (i in 1:nrow(d1)) {
  if(d1$injury.area[i] %in% headwords) {d1$injury.area.new[i] = 'Head'}
  else if(d1$injury.area[i] %in% upperbodywords) {d1$injury.area.new[i] = 'Upper Body'}
  else if(d1$injury.area[i] %in% lowerbodywords) {d1$injury.area.new[i] = 'Lower Body'}
  else if(d1$injury.area[i] %in% upperlower) {d1$injury.area.new[i] = 'Upper and lower body'}
  else {d1$injury.area.new[i] = NA}
} 

## ---- cleaninjuriesdata3 --------
#Create new field calculating age from birthdate (mm/dd/yyyy).
is.na(d1$birth_date) = NA #Some NA values were not recognized by R as NA or anything else 
d1$birth_date <- as.Date(d1$birth_date, format = "%m/%d/%Y", tryFormats = c("%m/%d/%Y")) 
d1$game_date <- d1$game_date+as.Date("1899-12-30", "%Y-%m-%d") #Convert game_date from integer of # days after 12/30/1899 to date
d1$age = NA
d1$age = round(as.numeric(difftime(d1$game_date, d1$birth_date, units = 'weeks'))/52, digits=1)

## ---- cleaninjuriessdata4 --------
#Character parameters may also need to be converted to factor. 
d1$position <- as.factor(d1$position)

## ---- cleanpbpdata1 --------
#Convert "gamedate" to date (from char), 
p1 = pbp 
p1$GameDate = as.Date(p1$GameDate, "%Y-%m-%d")

## ---- cleanpbpdata2 --------
#remove empty columns (X, X.1,X.2,X.3), 
p2 = select(p1, -c(X, X.1, X.2, X.3))

## ---- cleanpbpdata3 --------
#Remove observations in "Description" indicating end of qtr/game, 2-minute warnings, and timeouts 
p3 = p2 
p3 = p3[!grepl("TWO-MINUTE WARNING|END QUARTER|END OF QUARTER|TIMEOUT #|END GAME", p3$Description, ignore.case = T),] 

## ---- cleanpbpdata4 --------
#Summarize "description" into "play_type"
p3$play_type=NA
for (i in 1:nrow(p3)) {
  if('EXTRA' %in% capitalize_words(p3$Description[i])) {p3$play_type[i] = 'FG/PAT Attempt or Kickoff'}
  else if('KICK' %in% capitalize_words(p3$Description[i])) {p3$play_type[i] = 'FG/PAT Attempt or Kickoff'}
  else if('FUMBLE' %in% capitalize_words(p3$Description[i])) {p3$play_type[i] = 'Fumble'}
  else if('DEEP' %in% capitalize_words(p3$Description[i])) {p3$play_type[i] = 'Deep Pass'}
  else if('SHORT' %in% capitalize_words(p3$Description[i])) {p3$play_type[i] = 'Short Pass'}
  else if('PUNT' %in% capitalize_words(p3$Description[i])) {p3$play_type[i] = 'Punt'}
  else if('SACK' %in% capitalize_words(p3$Description[i])) {p3$play_type[i] = 'Sack'}
  else if('SCRAMBLES' %in% capitalize_words(p3$Description[i])) {p3$play_type[i] = 'Scramble'}
  else if('TWO-POINT' %in% capitalize_words(p3$Description[i])) {p3$play_type[i] = '2-Point Conversion'}
  else {p3$play_type[i] ='Run'}
} 

## ---- cleanpbpdata5 --------
#0 values for "Down" indicate kickoffs & PATs. Convert to NA
p3$Down[p3$Down == 0] = NA

## ---- cleanplayersdata1 --------
#Convert height to inches and then numeric. 
pl1 = players

hasdash = grepl("-", pl1$height) #Checks each element for a dash
for (i in seq_along(pl1$height)) {
  if (hasdash[i]) {
    #If dash present, split at dash and convert to numeric
    parts = as.numeric(unlist(strsplit(pl1$height[i], "-")))
    #Convert two numbers (ft, in) into inches 
    pl1$height[i] = parts[1]*12+parts[2]
  }
  else {
    #If no dash, record height as inches 
    pl1$height[i] = as.numeric(pl1$height[i])
  }
}


## ---- cleanplayersdata2 --------
#Character parameters may also need to be converted to factor. 
pl1$position <- as.factor(pl1$position)  

## ---- cleanplayersdata3 --------
#Create new field calculating age from birthdate (yyyy-mm-dd)
pl1$birthDate <- as.Date(pl1$birthDate, tryFormats = c("%m/%d/%Y", "%Y-%m-%d", "%d/%m/%Y")) #Not converting the observations with "%m/%d/%Y" format. Returning NA instead
pl1$age = NA
pl1$age = round(as.numeric(difftime(as.Date("2019-09-05"), pl1$birthDate, units = 'weeks'))/52, digits=1)


## ---- savedata --------
# all done, data is clean now. 
# Let's assign at the end to some final variable
# makes it easier to add steps above
injuriesprocesseddata = d1
pbpprocesseddata = p3
playersprocesseddata = pl1
# location to save file
save_data_location <- here::here("data","processed-data","injuriesprocesseddata.rds")
saveRDS(injuriesprocesseddata, file = save_data_location)
save_data_location <- here::here("data","processed-data","pbpprocesseddata.rds")
saveRDS(pbpprocesseddata, file = save_data_location)
save_data_location <- here::here("data","processed-data","playersprocesseddata.rds")
saveRDS(playersprocesseddata, file = save_data_location)


## ---- notes --------
# anything you don't want loaded into the Quarto file but 
# keep in the R file, just give it its own label and then don't include that label
# in the Quarto file

# Dealing with NA or "bad" data:
# removing anyone who had "faulty" or missing data is one approach.
# it's often not the best. based on your question and your analysis approach,
# you might want to do cleaning differently (e.g. keep individuals with some missing information)

# Saving data as RDS:
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# However, CSV is better for sharing with others since it's plain text. 
# If you do CSV, you might want to write down somewhere what each variable is.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata
