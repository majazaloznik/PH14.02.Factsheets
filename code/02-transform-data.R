##=============================================================================
## 00. preliminaries ==========================================================
## 01. data import ============================================================
## 02. data interpolation =====================================================
## 03. data transformation ====================================================
##=============================================================================

## 00. preliminaries ==========================================================
library(readr)
library(dplyr)
library(tidyr)

## 01. data import  =========================================================
# import population data 
pop <- readRDS(here::here("data/interim/mena.pop.rds"))

# import prospective age data 
prospective_age <- readRDS(here::here("data/interim/mena.prosp.age.rds"))

## 02. data transformation===================================================
# turn into percent instead of proportions
pop %>% 
  rename(location = Location,
         time = Time) %>% 
  group_by(location, time) %>% 
  mutate(prop_male = 100*PopMale/sum(PopTotal),
         prop_female = 100*PopFemale/sum(PopTotal)) %>% 
  select(-starts_with("Pop")) -> pop

# get threshold data ready for plotting 
prospective_age %>% 
  select(location, time, female, male, total) -> thresholds

# get proportiondata ready for plotting
prospective_age %>% 
  select(location, time, prop.over.65.total, prop.over.t.total) %>% 
  rename(prop.over.65 = prop.over.65.total,
         prop.over.t = prop.over.t.total) -> prop.over


## 03. save data for plotting  ================================================
saveRDS(pop, here::here("data/processed/mena.pop.rds"))
saveRDS(prop.over, here::here("data/processed/prop.over.rds"))
saveRDS(thresholds, here::here("data/processed/threshold.1y.rds"))


