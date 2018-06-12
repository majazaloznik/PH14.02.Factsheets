##=============================================================================
## 00. preliminaries ==========================================================
## 01. data import ============================================================
## 02. data export  ===========================================================
## 03. data interpolation =====================================================
##=============================================================================

## 00. preliminaries ==========================================================
library(readr)
library(dplyr)
library(tidyr)
source(here::here("code/FunSpline.R"))

## 01. data import  =========================================================

# import population data 
pop <- readRDS("data/processed/mena.pop.rds")

# import life expectacy data 
lt <- readRDS("data/processed/mena.lt.rds")

## 02. data export  ===========================================================
lt %>% 
  filter(Location == "Algeria", Sex == "Total") -> demo
saveRDS(demo, "data/processed/demo.rds")
pop %>% 
  filter(Location == "Algeria", Time == 1988) %>% 
  select(AgeGrp, PopTotal) -> demo.pop
saveRDS(demo.pop, "data/processed/demo.pop.rds")


## 03. data interpolation =====================================================

# use splines to get old age threshold (15 years remaining life expectancy)
# for each year/country combination
# NB: x and y are swapped here on the assumption that there is only 
# one age where ex is 15. 
lt %>% 
  group_by(Location, MidPeriod, Sex) %>% 
  summarise(old.age=FunSpline(ex,AgeGrpStart, 15))  %>% 
  spread(key = Sex, value = old.age)  -> old.age.threshold.5y

# use splines to get the age thresholds for each year
# first expand to get all years needed
interpolating.years <- expand.grid(MidPeriod = seq(from=min(old.age.threshold.5y$MidPeriod),
                                                   to = max(old.age.threshold.5y$MidPeriod), by = 1),
                                   Location = unique(old.age.threshold.5y$Location))

# interpolate the threshold for each single year, for all three Sex groups
old.age.threshold.5y %>% 
  right_join(interpolating.years) %>% 
  group_by(Location) %>% 
  mutate(Female.old.age.i = spline(MidPeriod, Female, xout = MidPeriod)$y,
         Male.old.age.i = spline(MidPeriod, Male, xout = MidPeriod)$y,
         Total.old.age.i = spline(MidPeriod, Total, xout = MidPeriod)$y) %>% 
  select(Location, MidPeriod, Total.old.age.i) %>% 
  rename(AgeGrp = Total.old.age.i, Time = MidPeriod) %>% 
  mutate(threshold = AgeGrp) ->   old.age.threshold.1y
rm(interpolating.years, old.age.threshold.5y)

# now merge that back with the full population table, sliding them as extra rows in
pop %>% 
  group_by(Location, Time) %>% 
  mutate(CumPop = cumsum(PopTotal)) %>% 
  select(-PopTotal) %>% 
  bind_rows(old.age.threshold.1y) %>% 
  arrange(Location, Time, threshold) %>% 
  fill(threshold) %>% 
  arrange(Location, Time, AgeGrp) %>% 
  select(c(2,5,7,10, 11)) %>% 
  arrange(Location, Time,AgeGrp) %>% 
  group_by(Location, Time) %>% 
  mutate(CumPop = spline(AgeGrp, CumPop, xout = AgeGrp)$y) ->  pop.old.age.threshold.1y


# now summarise proportions over threshold and over 65
pop.old.age.threshold.1y %>% 
  filter(Time >=1953, Time < 2098) %>% 
  mutate(over.65 = ifelse(AgeGrp < 65, "under", "over"),
         over.threshold = ifelse(AgeGrp < threshold, "under", "over")) %>% 
  group_by(Location, Time, over.65) %>% 
  mutate(Pop.65 = max(CumPop)) %>% 
  group_by(Location, Time, over.threshold) %>% 
  mutate(Pop.threshold = max(CumPop)) %>% 
  group_by(Location, Time) %>% 
  summarise(under.t = first(Pop.threshold),
            total = last(Pop.threshold),
            under.65 = first(Pop.65)) %>% 
  mutate(prop.over.65 = (total-under.65)/total,
         prop.over.t = (total - under.t)/total)  -> prop.over


saveRDS(prop.over, "data/processed/prop.over.rds")
saveRDS(old.age.threshold.1y, "data/processed/threshold.1y.rds")

