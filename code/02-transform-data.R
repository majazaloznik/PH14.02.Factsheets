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
source(here::here("code/FunSpline.R"))

## 01. data import  =========================================================
# import population data 
pop <- readRDS(here::here("data/interim/mena.pop.rds"))

# import prospective age data 
prospective_age <- readRDS(here::here("data/interim/mena.prosp.age.rds"))

## 02. data transformation===================================================
# turn into percent instead of proportions
pop %>% 
  group_by(Location, Time) %>% 
  mutate(PropMale = 100*PopMale/sum(PopTotal),
         PropFemale = 100*PopFemale/sum(PopTotal)) -> pop

# get threshold data ready for plotting 
prospective_age %>% 
  select(Location, Time, Female, Male, Total) -> thresholds

# get proportiondata ready for plotting
prospective_age %>% 
  select(Location, Time, prop.over.65.total, prop.over.t.total) %>% 
  rename(prop.over.65 = prop.over.65.total,
         prop.over.t = prop.over.t.total) -> prop.over


## 03. save data for plotting  ================================================
saveRDS(pop, here::here("data/processed/mena.pop.rds"))
saveRDS(prop.over, here::here("data/processed/prop.over.rds"))
saveRDS(thresholds, here::here("data/processed/threshold.1y.rds"))


