##=============================================================================
## 00. preliminaries ==========================================================
## 01. data import ============================================================
## 02. data culling  ===========================================================
## 03. data interpolation =====================================================
##=============================================================================

## 00. preliminaries ==========================================================
library(readr)
library(dplyr)
library(tidyr)
source(here::here("code/FunSpline.R"))

## 01. data import  =========================================================

# import population data 
mena.pop <- readRDS("data/processed/mena.pop.rds")

# import life expectacy data 
mena.lt <- readRDS("data/processed/mena.lt.rds")

## 02. data culling  ==========================================================

# get proportion ove 65 in each country/year combination
mena.pop %>% 
  select(Location, Time, PopTotal, AgeGrp) %>% 
  mutate(over.65 = ifelse(AgeGrp < 65, "under", "over")) %>% 
  group_by(Location, Time, over.65) %>% 
  summarise(PopTotal = sum(PopTotal)) %>% 
  spread(over.65, PopTotal) %>% 
  mutate(prop.over.65 = over/(over+under))  -> mena.over.65
  
## 03. data interpolation =====================================================

# use splines to get old age threshold (15 years remaining life expectancy)
# for each year/country combination
mena.lt %>% 
  group_by(Location, MidPeriod, Sex) %>% 
  summarise(old.age=FunSpline(ex,AgeGrpStart))  %>% 
  spread(key = Sex, value = old.age)  -> mena.old.age

# use splines to get the age thresholds for each year

# first expand to get all years needed
interpolating.years <- expand.grid(MidPeriod = seq(from=min(mena.old.age$MidPeriod),
                                                   to = max(mena.old.age$MidPeriod), by = 1),
                                   Location = unique(mena.old.age$Location))

# interpolate the threshold for each single year
mena.old.age %>% 
  right_join(interpolating.years) %>% 
  select(-Female, -Male) %>% 
  group_by(Location) %>% 
  mutate(threshold.interpolated = spline(MidPeriod, Total, xout = MidPeriod)$y) -> mena.old.age.single.year
  

# interpolate population over threshold age. 
mena.pop %>% 
  right_join(mena.old.age.single.year, by = c("Time" = "MidPeriod", "Location" = "Location")) %>% 
  select(-PopMale, -PopFemale, -MidPeriod, -Variant,  -Total) %>% 
  mutate(Time = as.integer(Time),
         VarID = VarID -1) %>% 
  group_by(Location, Time) %>%  
  summarise(PopTotal=FunSpline(AgeGrp,PopTotal, unique(threshold.interpolated)),
            AgeGrp = unique(threshold.interpolated), 
            threshold = unique(threshold.interpolated),
            VarID = first(VarID )) -> mena.interpolated.old.age.pop

# now merge that back with the full population table, sliding the extra rows in
mena.pop %>% 
  filter(Time >= 1953) %>% 
  select(Location, Time, AgeGrp, VarID, PopTotal) %>% 
  bind_rows(mena.interpolated.old.age.pop) %>% 
  arrange(Location, Time, VarID, AgeGrp) %>% 
  group_by(Location, Time) %>% 
  fill(threshold) %>% 
  arrange(Location, Time, AgeGrp)-> x


