##=============================================================================
## 00. preliminaries ==========================================================
## 01. data import ============================================================
## 02. data subset  ===========================================================
## 03. save output ============================================================
##=============================================================================

## 00. preliminaries ==========================================================
library(readr)
library(dplyr)

## 01. data import  ===========================================================
# manual data frame of countries used in the analysis
cntryz <- c("Algeria", "Bahrain", "Egypt", 
                                 "Iran (Islamic Republic of)", 
                                 "Iraq", "Israel", "Jordan", "Kuwait", 
                                 "Lebanon", "Libya", "Morocco", 
                                 "Oman", "State of Palestine", "Qatar", 
                                 "Saudi Arabia", "Syrian Arab Republic", 
                                 "Tunisia", "Turkey", "United Arab Emirates", 
                                 "Yemen")
# import population counts
pop <- read_csv(here::here("data/raw/WPP2017_PBSAS.csv"))

# import prospective age data
propsective_age <- read_csv(here::here("data/raw/final.data.csv"))

## 02. data subset  ===========================================================
# select only cases and variables needed
pop %>% 
  filter(Location %in% cntryz) %>% 
  select(Location, Time, AgeGrp, starts_with("Pop")) -> mena_pop

propsective_age %>% 
  filter(Location %in% cntryz) -> mena_prosp_age


## 03. save output ============================================================
# save population data for mena countires
saveRDS(mena_pop, here::here("data/interim/mena.pop.rds"))
saveRDS(mena_prosp_age, here::here("data/interim/mena.prosp.age.rds"))


