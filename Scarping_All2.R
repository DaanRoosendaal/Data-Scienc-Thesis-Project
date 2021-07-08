### Title:    Scraping Funda Data 
### Author:   Daan F. D. Roosendaal
### ANR:      u599939
### Created:  2021-04-23
### Modified: 2021-05-25
## scrapping multiple pages 

## preliminaries
library(rvest)
library(dplyr)

# set working Directory
setwd("~/Your Directory Here")

## main page link
#link = "https://www.funda.nl/koop/amsterdam/"


### functions for nested links data scrapes 

## Normal Information
# Price (because of error)
get_price = function(hous_link) {
  house_page = read_html(hous_link)
  house_price = house_page %>% html_nodes(".object-kenmerken-list:nth-child(2) .fd-align-items-center:nth-child(2)") %>%
    html_text()
  return(house_price)
}

# date of posting on page
get_postDate = function(hous_link) {
  house_page = read_html(hous_link)
  house_postDate = house_page %>% html_nodes(".fd-flex~ .fd-align-items-center:nth-child(6)") %>% 
    html_text()
  return(house_postDate)
}


## Build details:
# kind of home
get_homeKind = function(hous_link) {
  house_page = read_html(hous_link)
  house_homeKind = house_page %>% html_nodes(".object-kenmerken-list:nth-child(5) .fd-align-items-center:nth-child(2)") %>% 
    html_text()
  return(house_homeKind)
}

# kind of build
get_buildKind = function(hous_link) {
  house_page = read_html(hous_link)
  house_buildKind = house_page %>% html_nodes(".object-kenmerken-list:nth-child(5) .fd-align-items-center:nth-child(4)") %>% 
    html_text()
  return(house_buildKind)
} 

# Year of construction
get_ConstuctYear = function(hous_link) {
  house_page = read_html(hous_link)
  house_ConstructYear = house_page %>% html_nodes(".object-kenmerken-list:nth-child(5) .fd-align-items-center:nth-child(6)") %>% 
    html_text()
  return(house_ConstructYear)
} 


## Area & Volume details:
# Living Area (m2):
get_LivingArea = function(hous_link) {
  house_page = read_html(hous_link)
  house_livingArea = house_page %>% html_nodes(".object-kenmerken-list:nth-child(8) .fd-align-items-center:nth-child(2)") %>%
    html_text()
  return(house_livingArea)
}

# External Storage area (m2):
get_ExStorage = function(hous_link) {
  house_page = read_html(hous_link)
  house_ExStorage = house_page %>% html_nodes(".object-kenmerken-list:nth-child(8) .object-kenmerken-group-list .fd-align-items-center~ .fd-align-items-center") %>%
    html_text() %>% paste(collapse = ",")
  return(house_ExStorage)
}

# Lot Area:
get_Lot = function(hous_link) {
  house_page = read_html(hous_link)
  house_Lot = house_page %>% html_nodes(".fd-align-items-center:nth-child(5)") %>%
    html_text()
  return(house_Lot)
}

# Volume (m3):
get_Volume = function(hous_link) {
  house_page = read_html(hous_link)
  house_Volume = house_page %>% html_nodes(".object-kenmerken-group-list~ .fd-align-items-center") %>%
    html_text() %>% paste(collapse = ",")
  return(house_Volume)
}


## Layout details:
# Amount of rooms in property
get_Rooms = function(hous_link) {
  house_page = read_html(hous_link)
  house_Rooms = house_page %>% html_nodes(".object-kenmerken-list:nth-child(11) .fd-align-items-center:nth-child(2)") %>%
    html_text() %>% paste(collapse = ",")
  return(house_Rooms)
}

# Amount of Bathrooms in property
get_Bathrooms = function(hous_link) {
  house_page = read_html(hous_link)
  house_bathrooms = house_page %>% html_nodes(".object-kenmerken-list:nth-child(11) .fd-align-items-center:nth-child(4)") %>%
    html_text() %>% paste(collapse = ",")
  return(house_bathrooms)
}

# Bathroom Facilities 
get_BathFac = function(hous_link) {
  house_page = read_html(hous_link)
  house_bathFac = house_page %>% html_nodes(".object-kenmerken-list:nth-child(11) .fd-align-items-center:nth-child(6)") %>%
    html_text()
  return(house_bathFac)
}


# Amount of levels 
get_Levels = function(hous_link) {
  house_page = read_html(hous_link)
  house_levels = house_page %>% html_nodes(".object-kenmerken-list:nth-child(11) .fd-align-items-center:nth-child(8)") %>%
    html_text()
  return(house_levels)
}


## Energy Information:
# Energy lable
get_EnergyLable = function(hous_link) {
  house_page = read_html(hous_link)
  house_label = house_page %>% html_nodes(".object-kenmerken-list:nth-child(14) dd:nth-child(2)") %>%
    html_text()
  return(house_label)
}

# Insulation 
get_Insulation = function(hous_link) {
  house_page = read_html(hous_link)
  house_Insulation = house_page %>% html_nodes(".object-kenmerken-list:nth-child(14) .fd-align-items-center:nth-child(4)") %>%
    html_text()
  return(house_Insulation)
}

# Heating
get_Heating = function(hous_link) {
  house_page = read_html(hous_link)
  house_heating = house_page %>% html_nodes(".object-kenmerken-list:nth-child(14) .fd-align-items-center:nth-child(6)") %>%
    html_text()
  return(house_heating)
}


## Outside information:
# Location of house
get_Location = function(hous_link) {
  house_page = read_html(hous_link)
  house_Location = house_page %>% html_nodes(".object-kenmerken-list:nth-child(19) .fd-align-items-center:nth-child(2)") %>%
    html_text()
  return(house_Location)
}

# Garden info
get_Garden = function(hous_link) {
  house_page = read_html(hous_link)
  house_Garden = house_page %>% html_nodes(".object-kenmerken-list:nth-child(19) .fd-align-items-center:nth-child(4)") %>%
    html_text()
  return(house_Garden)
}

# Garden area (m2)
get_GardenArea = function(hous_link) {
  house_page = read_html(hous_link)
  house_GardenArea = house_page %>% html_nodes(".object-kenmerken-list:nth-child(19) .fd-align-items-center:nth-child(6)") %>%
    html_text()
  return(house_GardenArea)
}

# Garden Aim (Noth/East/South/West)
get_GardenAim = function(hous_link) {
  house_page = read_html(hous_link)
  house_GardenAim = house_page %>% html_nodes(".object-kenmerken-list:nth-child(19) .fd-align-items-center:nth-child(8)") %>%
    html_text()
  return(house_GardenAim)
}

# Storage (Shed/garage)
get_Storage = function(hous_link) {
  house_page = read_html(hous_link)
  house_Storage = house_page %>% html_nodes(".object-kenmerken-list:nth-child(22) .fd-align-items-center:nth-child(2)") %>%
    html_text()
  return(house_Storage)
}

# Parking Facilities
get_Parking = function(hous_link) {
  house_page = read_html(hous_link)
  house_Parking = house_page %>% html_nodes(".object-kenmerken-list:nth-child(26) .fd-align-items-center") %>%
    html_text()
  return(house_Parking)
}


## gettting publish date 
get_published_date = function(hous_link) {
  house_page = read_html(hous_link)
  house_Parking = house_page %>% html_nodes("app-object-statistics") %>%
    html_attr("published-date")
  return(house_Parking)
}


### Scraping all data form multiple pages + nested ###
## create empty data frame
All_houses2 = data.frame()

## function to scrape all data
for(page_result in seq(from = 126, to = 153, by = 1)) {
  link = paste0("PASTE FUNDA LINK HERE", page_result,"/")
  page = read_html(link)
  
  ## give algorithm a break so servers aren't stressed too much
  if((page_result %% 5) == 0){
    message("taking a break")
    Sys.sleep(10)
  }
  
  ## Start iteration timer:
  Start = Sys.time()
  
  ## main page data scrape
  street = page %>% html_nodes(".search-result__header-title-col a:nth-child(1)") %>% html_text()
  homes_links = page %>% html_nodes(".search-result__header-title-col a:nth-child(1)") %>% 
    html_attr("href") %>% paste("https://www.funda.nl", ., sep = "")
  zipcode = page %>% html_nodes(".search-result__header-subtitle") %>% html_text()
  
  ## Nested data scrape 
  price = sapply(homes_links, FUN = get_price, USE.NAMES = FALSE)
  price[lengths(price) == 0] <- NA_character_
  price = unlist(price, use.names = FALSE)
  
  PublishedDate = sapply(homes_links, FUN = get_published_date, USE.NAMES = FALSE)
  PublishedDate[length(PublishedDate) == 0] <- NA_character_
  PublishedDate = unlist(PublishedDate, use.names = FALSE)
  
  buildKind = sapply(homes_links, FUN = get_buildKind, USE.NAMES = FALSE)
  buildKind[lengths(buildKind) == 0] <- NA_character_
  buildKind = unlist(buildKind, use.names = FALSE)
  
  ConstructionYear = sapply(homes_links, FUN = get_ConstuctYear, USE.NAMES = FALSE)
  ConstructionYear[lengths(ConstructionYear) == 0] <- NA_character_
  ConstructionYear = unlist(ConstructionYear, use.names = FALSE)
  
  LivingArea = sapply(homes_links, FUN = get_LivingArea, USE.NAMES = FALSE)
  LivingArea[lengths(LivingArea) == 0] <- NA_character_
  LivingArea = unlist(LivingArea, use.names = FALSE)
  
  ExStorage = sapply(homes_links, FUN = get_ExStorage, USE.NAMES = FALSE)
  ExStorage[lengths(ExStorage) == 0] <- NA_character_
  ExStorage = unlist(ExStorage, use.names = FALSE)
  
  LotArea = sapply(homes_links, FUN = get_Lot, USE.NAMES = FALSE)
  LotArea[lengths(LotArea) == 0] <- NA_character_
  LotArea = unlist(LotArea, use.names = FALSE)
  
  Volume = sapply(homes_links, FUN = get_Volume, USE.NAMES = FALSE)
  Volume[lengths(Volume) == 0] <- NA_character_
  Volume = unlist(Volume, use.names = FALSE)
  
  Rooms = sapply(homes_links, FUN = get_Rooms, USE.NAMES = FALSE)
  Rooms[lengths(Rooms) == 0] <- NA_character_
  Rooms = unlist(Rooms, use.names = FALSE)
  
  Bathrooms = sapply(homes_links, FUN = get_Bathrooms, USE.NAMES = FALSE)
  Bathrooms[lengths(Bathrooms) == 0] <- NA_character_
  Bathrooms = unlist(Bathrooms, use.names = FALSE)
  
  # BathroomFac = sapply(homes_links, FUN = get_BathFac, USE.NAMES = FALSE)
  # BathroomFac[lengths(BathroomFac) == 0] <- NA_character_
  # BathroomFac = unlist(BathroomFac, use.names = FALSE)
  
  Levels = sapply(homes_links, FUN = get_Levels, USE.NAMES = FALSE)
  Levels[lengths(Levels) == 0] <- NA_character_
  Levels = unlist(Levels, use.names = FALSE)
  
  EnergyLable = sapply(homes_links, FUN = get_EnergyLable, USE.NAMES = FALSE)
  EnergyLable[lengths(EnergyLable) == 0] <- NA_character_
  EnergyLable = unlist(EnergyLable, use.names = FALSE)
  
  Insulation = sapply(homes_links, FUN = get_Insulation, USE.NAMES = FALSE)
  Insulation[lengths(Insulation) == 0] <- NA_character_
  Insulation = unlist(Insulation, use.names = FALSE)
  
  Heating = sapply(homes_links, FUN = get_Heating, USE.NAMES = FALSE)
  Heating[lengths(Heating) == 0] <- NA_character_
  Heating = unlist(Heating, use.names = FALSE)
  
  Location = sapply(homes_links, FUN = get_Location, USE.NAMES = FALSE)
  Location[lengths(Location) == 0] <- NA_character_
  Location = unlist(Location, use.names = FALSE)
  
  Garden = sapply(homes_links, FUN = get_Garden, USE.NAMES = FALSE)
  Garden[lengths(Garden) == 0] <- NA_character_
  Garden = unlist(Garden, use.names = FALSE)
  
  GardenArea = sapply(homes_links, FUN = get_GardenArea, USE.NAMES = FALSE)
  GardenArea[lengths(GardenArea) == 0] <- NA_character_
  GardenArea = unlist(GardenArea, use.names = FALSE)
  
  GardenAim = sapply(homes_links, FUN = get_GardenAim, USE.NAMES = FALSE)
  GardenAim[lengths(GardenAim) == 0] <- NA_character_
  GardenAim = unlist(GardenAim, use.names = FALSE)
  
  Storage = sapply(homes_links, FUN = get_Storage, USE.NAMES = FALSE)
  Storage[lengths(Storage) == 0] <- NA_character_
  Storage = unlist(Storage, use.names = FALSE)
  
  ## update data frame
  All_houses2 = rbind(All_houses2, data.frame(street, 
                                            zipcode, 
                                            price,
                                            PublishedDate,
                                            buildKind, ConstructionYear,
                                            LivingArea,
                                            ExStorage,
                                            LotArea, Volume, Rooms,
                                            Bathrooms,
                                            Levels, EnergyLable,
                                            Insulation, Heating, 
                                            Location,
                                            Garden,
                                            GardenArea,
                                            GardenAim, 
                                            Storage, 
                                            stringsAsFactors = FALSE))    
  
  ## End iteration timer
  End = Sys.time()
  ## Calculate time of iteration step
  Time = End - Start
  
  ## show progress and time iteration
  print(paste("Page:", page_result, "/ Time:", round(Time, 2) ))
}



## view data frame 
View(houses_test1)
## store dataframe as csv file
write.csv(All_houses2, "All_houses2.csv")


