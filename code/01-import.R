##=============================================================================
## 00. preliminaries ==========================================================
## 01. data import ============================================================
## 02. save output ============================================================
##=============================================================================

## 00. preliminaries ==========================================================
library(readr)
library(dplyr)

## 01. data import  ===========================================================

# import country list
cntryz <- read_csv("data/raw/cntry.list.csv", col_names = "country")

# import population counts
pop.df <- read_csv("data/raw/WPP2017_PBSAS.csv")
#, col_types = "iciciniiinnn")

# select only cases and variables needed
pop.df %>% 
  filter(Location %in% pull(cntryz)) %>% 
  select(-AgeGrpStart, -AgeGrpSpan) %>% 
  mutate(AgeGrp = as.numeric(ifelse(AgeGrp == "80+", "80", AgeGrp)))-> mena.pop

# import life table data
lt.df <- read_csv("data/raw/WPP2017_LifeTable.csv")

# select only cases and variables needed
lt.df %>% 
  filter(Location %in% pull(cntryz))-> mena.lt

## 02. save output ============================================================

# save population data for mena countires
saveRDS(mena.pop, "data/processed/mena.pop.rds")

# save life expectancy data for mena countires
saveRDS(mena.lt, "data/processed/mena.lt.rds")

