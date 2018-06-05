##=============================================================================
## 00. preliminaries ==========================================================
## 01. data import ============================================================
##=============================================================================

## 00. preliminaries ==========================================================
library(readr)
library(dplyr)

## 01. data import  =========================================================

# import country list
cntryz <- read_csv("data/raw/cntry.list.csv", col_names = "country")

# import population counts
pop.df <- read_csv("data/raw/WPP2017_PBSAS.csv")

# select only cases and variables needed
pop.df %>% 
  filter(Location %in% pull(cntryz)) %>% 
  select(-AgeGrpStart, -AgeGrpSpan) -> mena.pop

# import life table data
lt.df <- read_csv("data/raw/WPP2017_LifeTable.csv")

# select only cases and variables needed
lt.df %>% 
  filter(Location %in% pull(cntryz))-> mena.lt





saveRDS(mena.pop, "data/processed/mena.pop.rds")
