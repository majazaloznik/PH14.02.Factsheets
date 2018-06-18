##=============================================================================
## 00. preliminaries ==========================================================
## 01. plotting thresholds ====================================================
## 02. plotting proportions over 65 ===========================================
##=============================================================================


## 00. preliminaries ==========================================================
library(dplyr)

threshold.1y <-readRDS("data/processed/threshold.1y.rds")
source("code/FunPlotThreshold.R")

prop.over <- readRDS("data/processed/prop.over.rds")

pop <- readRDS("data/processed/mena.pop.rds")

## 01. plotting thresholds ====================================================

lapply(unique(threshold.1y$Location), FUN = FunPlotThreshold)


FunPlotThreshold("Iraq", write = FALSE,  col.bg = "lightcyan2")


## 02. plotting proportions over 65 ===========================================


FunPlotProportions("Bahrain", 
                   col.bg = "lightcyan2",
                   write = FALSE)

lapply(unique(threshold.1y$Location), function(x) FunPlotProportions(x, write = FALSE))


