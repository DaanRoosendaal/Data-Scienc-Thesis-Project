library(dplyr)

setwd("~/Desktop/UT/Master/Thesis/Code/")

outputDir <- "../Output/"

# COVID data
COVID <- read.csv2("~/Downloads/COVID-19_aantallen_gemeente_per_dag.csv")
MLR <- read.csv(paste0(outputDir, "MLR_data.csv"), row.names = 1)

COVID <- COVID %>%
  select(Date_of_publication, Municipality_name, Total_reported, Hospital_admission, Deceased)
COVID <- COVID %>%
  mutate(Date_of_publication = as.Date(COVID$Date_of_publication, format = "%Y-%m-%d"))
  
## delete troubeling values
MLR <- MLR  %>%
  filter(price > 0 & 
           (PricePerM2 < Inf | PricePerM2 > -Inf) &
           LotArea > 0 &
           LivingArea >0 &
           Volume > 0)

## rename columns
MLR <- MLR %>%
  rename(Date_of_publication=PublishedDate, Municipality_name=City)
MLR <- MLR %>%
  mutate(Date_of_publication = as.Date(MLR$Date_of_publication, format = "%Y-%m-%d"))
  
MLR <- MLR %>%
  mutate(PublishedDate = as.Date(MLR$PublishedDate, format = "%Y-%m-%d"))

## merge dataframes
COVID_add3 <- inner_join(MLR, COVID, by = c('PublishedDate'='Date_of_publication', 'City' = 'Municipality_name'))

COVID_add3 <- COVID_add3[,-1]

write.csv(COVID_add3, paste0(outputDir, "MLR_COVID.csv"))













