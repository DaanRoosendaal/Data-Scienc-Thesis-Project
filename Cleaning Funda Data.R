### Title:    Cleaning Funda Scrape Data 
### Author:   Daan F. D. Roosendaal
### ANR:      u599939
### Created:  2021-04-17
### Modified: 2021-05-25

###--------------------------------------------------------------------------###

### Preliminaries ###

## load Required Packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(caret)

## Set working directory 
setwd("~Your Working Directory Here")

## Set Directories
DataDir   <- "../Data/"
outputDir <- "../Output/"

## Dataframe names 
FileName1   <- "All_houses.csv"   ## your scraped datasets 
FileName2   <- "All_houses2.csv"  ## second DF if you made multiple scrapes

## Load Dataframes to Environment
dat1  <- read.csv(paste0(DataDir, FileName1), row.names = 1)
dat2  <- read.csv(paste0(DataDir, FileName2), row.names = 1)


###--------------------------------------------------------------------------###

### Binding All Dataframes ###

## dropping too dirty and irrelevant columns
ColDrop = c("opp", "HotWater", "RoofKind", "Levels", "bathroomFac", "homeKind")

## Drop
dat1 = dat1[, !names(dat1) %in% ColDrop]
dat2 = dat2[, !names(dat2) %in% ColDrop]

## Row Bind into one Dataframe 
datTot = rbind(dat1, dat2, stringsAsFactors = FALSE)

# Backup Total Dataframe
BackUp <- datTot

## Remove partial dataframes from environment
rm(dat1, dat2)


###--------------------------------------------------------------------------###

### DATA CLEANING ### 

## funtion to remove scraping noise out of all columns
remove_spaces =  function (col) {
  col = str_replace_all(col, "\\n", "")
  col = str_replace_all(col, "  ", "")
  return(col)
}
## remove scraping noise from df
datTot = sapply(datTot, FUN = remove_spaces, USE.NAMES = TRUE )
datTot <- as.data.frame(datTot)

## make backup
BackUp <- datTot

## cleaning PublishedDate ##
# set tot datetime format
datTot <- datTot %>%
  mutate(PublishedDate = as.Date(datTot$PublishedDate, format = "%d-%m-%Y"))
# sometimes R turns the format around when loading the data. Check if
# transformation is done right, if needed change format accordingly 
summary(datTot$PublishedDate)

## Cleaning Zipcode Column ##
# splitting zipcode column in city and zipcode  
datTot <- datTot %>%
  separate(zipcode, into = c('zipcode', 'Letter', 'City', 'City2'), sep = "\\ ") %>%
  mutate(City = paste(City, City2)) %>%
  mutate(zipcode = paste(zipcode, Letter))

datTot <- datTot %>%
  mutate(City = gsub(' NA' , '', datTot$City)) %>%
  select(-c(City2, Letter))


## Cleaning Volume column ##
datTot <- datTot %>%
  separate(Volume, into = c("Vol1", "Volume"), sep = ",", fill = "left") %>%
  select(-c("Vol1")) 


## Cleaning Garden Area ## 
# separate Area in Depth and Width
datTot <- datTot %>%
  separate(GardenArea, into = c('GardenArea', 'Other'), sep = "\\(") %>%
  separate(Other, into = c('Depth', 'Width'), sep = "en")

# Now remove all strings from numbers 
datTot <- datTot %>%
  mutate(Depth = gsub(" meter diep ", "", datTot$Depth) %>%
  # remove wrongly scraped values 
                  gsub("€ 100,00 per maand) ", NA, .) %>%
                  gsub("€ 121,50 per maand) ", NA, .) %>%
                  gsub("€ 123,61 per maand) ", NA, .) %>%
                  gsub("€ 134,00 per maand) ", NA, .) %>%
                  gsub("€ 150,00 per maand) ", NA, .) %>%
                  gsub("€ 159,01 per maand) ", NA, .) %>%
                  gsub("€ 160,00 per maand) ", NA, .) %>%
                  gsub("€ 201,37 per maand) ", NA, .) %>%
                  gsub("€ 50,00 per maand) ", NA, .) %>%
                  gsub("€ 540,00 per maand) ", NA, .) %>%
                  gsub("€ 202,14 per maand) ", NA, .) %>%
                  gsub("€ 205,00 per maand) ", NA, .) ) %>%
  # remove string from Width
  mutate(Width = gsub(" meter breed)", "", datTot$Width))

# set values to numeric
datTot <- datTot %>%
  mutate(Depth = gsub(",", ".", datTot$Depth) %>%
                 as.numeric(.) %>%
                 replace_na(., 0)) %>%
  mutate(Width = gsub(",", ".", datTot$Width) %>%
                 as.numeric(.) %>%
                 replace_na(., 0))


## further removal of noise ##
# Function to remove all non-alphanumeric characters
rm_NonAlpha = function(col_x) {
  col_x = str_replace_all(col_x, "[^[:alnum:]]", "")
  return(col_x)
}
# cols to do alphanumeric deletion on
NonAlpha_cols = c("price", "LivingArea", "ExStorage", "LotArea", 
                  "Volume", "GardenArea")
# apply non-aplphanumeric deletion function
datTot[NonAlpha_cols] = sapply(datTot[NonAlpha_cols], FUN = rm_NonAlpha)


## Cleaning ExStorage ##
datTot <- datTot %>%
  separate(ExStorage, into = c("ExS1", "ExS2", "ExS3"), sep="m", remove = F)

datTot <- datTot %>%
  mutate(ExS1 = as.numeric(datTot$ExS1)) %>%
  mutate(ExS2 = as.numeric(datTot$ExS2)) %>%
  mutate(ExS3 = as.numeric(datTot$ExS3))

datTot <- datTot %>%
  mutate(ExS1 = replace_na(datTot$ExS1, 0)) %>%
  mutate(ExS2 = replace_na(datTot$ExS2, 0)) %>%
  mutate(ExS3 = replace_na(datTot$ExS3, 0))

datTot <- datTot %>%
  mutate(ExStor_TOT = (ExS1 + ExS2 + ExS3))

## delete older 4 columns


## Function to remove remaining "m" in multiple columns
m_remover =  function (col_x) {
  # remove "m"
  col_x = gsub("m", "", col_x)
  # make col numeric
  col_x = as.numeric(col_x)
  # replace NA with 0
  col_x = replace_na(col_x, 0)
  return(col_x)
}
# columns containing "m"
m_cols = c("LivingArea", "GardenArea", "LotArea", "Volume")
# apply function to remove "m"
datTot[m_cols] = sapply(datTot[m_cols] ,FUN =  m_remover)


## Cleaning Price Column ##
# remove "kostenkoper" and make col numeric
datTot <- datTot %>%
  mutate(price =  gsub('kostenkoper', '', datTot$price) %>%
                  as.numeric(.))
## wat te doen met NA's? MI uitvoeren?

## Separate rooms and bedrooms ## (before alphanumeric deletion)
datTot <- datTot %>%
  separate(Rooms, into = c('Rooms', 'Bedrooms'), sep = "\\(")
datTot <- datTot %>%
  mutate(Rooms = gsub(" kamers ", "", datTot$Rooms) %>%
                 gsub("kamer", "", .)  %>%
                 as.numeric(.)  %>%
                 replace_na(., 1) ) %>%
  mutate(Bedrooms = gsub(" slaapkamers)", "", datTot$Bedrooms) %>%
                    gsub(" slaapkamer)", "", .) %>%
                    as.numeric(.) %>%
                    replace_na(., 1) )

## Cleaning BuildKind column
# only replace NA with UnKnown
datTot <- datTot %>%
  mutate(buildKind = replace_na(datTot$buildKind, "UnKnown"))


###--------------------------------------------------------------------------###

### Cleaning text columns using regular expressions ###

## Cleaning storage ##
## Devide column into 3 classes:
# outdoor     = berging, box, carport, garage, parkeer, parkeren, terrein
# indoor      = inpandig, ja
# no storage  = nee, NA 

## set up specific patterns
# pattern to extract info from text data
Pattern1 = "(garage|carport|berging|inpandig|terrein|parkeer|parkeren|nee|ja|box|inpandig)"
# pattern for outdoor storage
outdoor_pat = c("berging|box|carport|garage|parkeer|parkeren|terrein")
# pattern for indoor storage 
indoor_pat = c("inpandig|ja")

## devide column into 3 groups
datTot <- datTot %>%
  mutate(Storage = str_extract(tolower(datTot$Storage), Pattern1) %>%
           str_replace_all(. , outdoor_pat , "outdoor") %>%
           str_replace_all(. , indoor_pat , "indoor") %>%
           # replace nee and NA to NoStorage class
           str_replace_all(. , "nee", "NoStorage") %>%
           replace_na(. , "NoStorage") )

## Cleaning energy label
# only keep letters
Pattern <- "([A-G]|Niet beschikbaar|Niet verplicht)"
datTot$EnergyLable = str_extract(datTot$EnergyLable, Pattern) 
# Set other values to "Unknown"
datTot <- datTot %>%
  mutate(EnergyLable = gsub("Niet beschikbaar", "Unknown", datTot$EnergyLable) %>%
           gsub("Niet verplicht", "Unknown", .) %>%
           replace_na(., "Unknown"))


## Cleaning GardenAim ##
# Divide into groups:
# other       = balkon, ja, dakterras
# south       = zuiden, zuidoosten, zuidwesten
# north       = noorden, noordoosten, noordwesten
# west        = westen
# east        = oosten
# NoGarden    = nee, NA

# North en South are the most important regarding the influence of the sun, 
#     that's why northeast/northwest etc. are added to south or north instead 
#     of lose variables. 

# Set up different class patterns
north_pattern = "(noorden|noordoosten|noordwesten)"
south_pattern = "(zuiden|zuidoosten|zuidwesten)"
Alt_pattern = "(balkon|ja|dakterras)"
# extraction pattern
GA_pattern = "(North|East|South|West|Other|NoGarden)"

## transfrom GardenAim Column
datTot <- datTot %>%
  mutate(GardenAim =  str_replace_all(tolower(datTot$GardenAim), north_pattern, 
                                      "North") %>%
           str_replace_all(., south_pattern, "South") %>%
           str_replace_all(., "westen", "West") %>%
           str_replace_all(., "oosten", "East") %>%
           str_replace_all(., Alt_pattern, "Other") %>%
           # replace "nee" and "NA" with NoGarden
           str_replace_all(., "nee", "NoGarden") %>%
           replace_na(., "NoGarden") %>%
           # extract classes
           str_extract(., GA_pattern) )

## cleaning garden ## 
## set REGEX pattern to extract info from text data
Pattern2 = "(terras|tuin|nee|ja|plaats|patio|atrium|balkon)"
## set patterns for different classes 
Patio_pat = c("plaats|patio|atrium")
Garden_pat = c("tuin|ja")
Terras_pat = c("balkon|terras")

# create different classes 
datTot <- datTot %>%
  mutate(Garden = str_extract(tolower(datTot$Garden), Pattern2) %>%
           str_replace_all(., Patio_pat, "patio") %>%
           str_replace_all(., Garden_pat, "garden") %>%
           str_replace_all(., Terras_pat, "terras/balkon") %>%
           # set "nee" and "NA" to NoGarden class
           str_replace_all(., "nee", "NoGarden") %>%
           replace_na(., "NoGarden"))


## Cleaning Bathrooms ##
# setup REGEX pattern
BathRooms_pattern = "\\d{1,2} (badkamer)"
# mutate Bathrooms
datTot = datTot %>%
  mutate(Bathrooms = str_extract(tolower(datTot$Bathrooms), BathRooms_pattern) %>%
           gsub(" badkamer", "", .) %>%
           as.numeric(.) %>%
           replace_na(. , 1) )


## Clean City Column ##
# make groups of 
denBosch   = c("Nuland", "Rosmalen", "Vinkel (Gem.") 
middelburg = c("Arnemuiden") 
tilburg    = c("Berkel-Enschot", "Udenhout") 
Utrecht    = c("De Meern", "Haarzuilens", "Vleuten") 
Groning    = c("Garmerwolde", "Glimmen", "Haren (GR)", "(GR)", "Meerstad", 
            "Noordlaren", "Onnen", "Ten Boer", "Ten Post", "Winneweer")   
DenHaag    = c("Haag") 
Rotterdam  = c("Hoek van", "Hoogvliet Rotterdam", "Pernis Rotterdam",
              "Rozenburg (ZH)")
Amersfoort = c("Hoogland", "Hooglanderveen") 
Amsterdam  = c("Nieuw- en")
Haarlem    = c("Spaarndam gem.", "Ten Boer")

# function to simplify City column
simplify_City = function(city_col) {
  city_col[city_col %in% denBosch] <- "Den Bosch"
  city_col[city_col %in% middelburg] <- "Middelburg"
  city_col[city_col %in% tilburg] <- "Tilburg"
  city_col[city_col %in% Utrecht] <- "Utrecht"
  city_col[city_col %in% Groning] <- "Groningen"
  city_col[city_col %in% DenHaag] <- "Den Haag"
  city_col[city_col %in% Rotterdam] <- "Rotterdam"
  city_col[city_col %in% Amersfoort] <- "Amersfoort"
  city_col[city_col %in% Amsterdam] <- "Amsterdam"
  city_col[city_col %in% Haarlem] <- "Haarlem"
  city_col
}
# apply function
datTot <- datTot %>%
  mutate(City = simplify_City(City))


## dropping columns that have been replaced (Cleanup)
ColDrop2 = c("ExStorage", "ExS1", "ExS2", "ExS3", "Heating", "Location", "Insulation")
# drop these columns 
datTot = datTot[, !names(datTot) %in% ColDrop2]



###--------------------------------------------------------------------------###

## Adding Additional Columns ##

## Calculate price per square meter of living space
datTot <- datTot %>%
  mutate(PricePerM2 = round((datTot$price / datTot$LivingArea),1))

## update backup
BackUp <- datTot


## making dummies on character data ##

# met paste0() een functie maken voor meerdere columns?
dmy <- dummyVars("~ City", data = datTot)
datTot <- cbind(datTot, data.frame(predict(dmy, newdata=datTot)))

dmy <-dummyVars("~ buildKind", data = datTot)
datTot <- cbind(datTot, data.frame(predict(dmy, newdata=datTot)))

dmy <- dummyVars("~ EnergyLable", data = datTot)
datTot <- cbind(datTot, data.frame(predict(dmy, newdata=datTot)))

dmy <- dummyVars("~ Garden", data = datTot)
datTot <- cbind(datTot, data.frame(predict(dmy, newdata=datTot)))

dmy <- dummyVars("~ GardenAim", data = datTot)
datTot <- cbind(datTot, data.frame(predict(dmy, newdata=datTot)))

dmy <- dummyVars("~ Storage", data = datTot)
datTot <- cbind(datTot, data.frame(predict(dmy, newdata=datTot)))

## dropping some dummy columns to avoid Multi-colinearity
DummyDrop <- c("buildKindUnKnown", "EnergyLableUnknown", 
               "GardenNoGarden", "GardenAimNoGarden", "StorageNoStorage")
datTot = datTot[, !names(datTot) %in% DummyDrop]

## write dataframes to output folder
write.csv(BackUp, paste0(outputDir, "BackUp.csv"))
write.csv(datTot, paste0(outputDir, "MLR_data.csv"))


###--------------------------------------------------------------------------###












