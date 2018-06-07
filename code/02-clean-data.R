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
  summarise(old.age=FunSpline(ex,AgeGrpStart, 15))  %>% 
  spread(key = Sex, value = old.age)  -> mena.old.age

# use splines to get the age thresholds for each year

# first expand to get all years needed
interpolating.years <- expand.grid(MidPeriod = seq(from=min(mena.old.age$MidPeriod),
                                                   to = max(mena.old.age$MidPeriod), by = 1),
                                   Location = unique(mena.old.age$Location))

# interpolate the threshold for each single year, for all three Sex groups
mena.old.age %>% 
  right_join(interpolating.years) %>% 
  group_by(Location) %>% 
  mutate(Female.old.age.i = spline(MidPeriod, Female, xout = MidPeriod)$y,
         Male.old.age.i = spline(MidPeriod, Male, xout = MidPeriod)$y,
         Total.old.age.i = spline(MidPeriod, Total, xout = MidPeriod)$y) -> 
  mena.old.age.single.year
  
# now merge that back with the full population table, sliding them as extra rows in
mena.old.age.single.year %>% 
  select(Location, MidPeriod, Total.old.age.i) %>% 
  rename(AgeGrp = Total.old.age.i, Time = MidPeriod) %>% 
  mutate(threshold = AgeGrp) %>% 
  bind_rows(mena.pop) %>% 
  select(c(1:4, 11)) %>% 
  arrange(Location, Time,AgeGrp) %>% 
  group_by(Location, Time) %>% 
  mutate(PopTotal = spline(AgeGrp, PopTotal, xout = AgeGrp)$y) %>% 
  mutate(temp = ifelse(!is.na(threshold), lag(PopTotal), NA),
         PopTotal = ifelse(!is.na(lead(threshold)), lead(PopTotal), PopTotal),
         PopTotal = ifelse(!is.na(threshold), temp-PopTotal, PopTotal)) %>% 
  select(-temp) %>% 
  arrange(Location, Time, threshold) %>% 
  fill(threshold) %>% 
  arrange(Location, Time,AgeGrp)  %>% 
  mutate(over.threshold = ifelse(AgeGrp < threshold, "under", "over")) %>% 
  group_by(Location, Time, over.threshold) %>% 
  summarise(PopTotal = sum(PopTotal)) %>% 
  spread(over.threshold, PopTotal) %>% 
  mutate(over.threshold = over/(over+under))  -> mena.over.threshold
# 
# 
# left_join(mena.over.65, mena.over.threshold, by = c("Location" = "Location", "Time" = "Time")) -> x
# 
# x %>% 
#   filter(Location == "Oman") -> a
#   
# plot(a$Time, a$prop.over.65, type = "l", xlim = c(1980, 2050), lwd = 2, ylim = c(0, 0.2)) 
# lines(a$Time, a$over.threshold, lwd = 2, lty = 2, col = "red") 
#           