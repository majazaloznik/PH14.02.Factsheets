##=============================================================================
## 00. preliminaries ==========================================================
## 01. plotting thresholds ====================================================
## 02. plotting proportions over 65 ===========================================
##=============================================================================


## 00. preliminaries ==========================================================
library(dplyr)
source("code/FunPlotThreshold.R")

# threshold for each country/age/gender combination
threshold.1y <-readRDS("data/processed/threshold.1y.rds")
# prop over threshold and over 65 for each country/age/gender combination
prop.over <- readRDS("data/processed/prop.over.rds")
# single age populations for each each country/age/gender combination
pop <- readRDS("data/processed/mena.pop.rds")



## 01. plotting thresholds ====================================================

lapply(unique(threshold.1y$Location),
       function(x)  FunPlotThreshold(x,
                                     height = 4.5, width = 5,
                                     col.bg = background))

FunPlotThreshold("Iraq", write = FALSE,  col.bg = "lightcyan2",
                 height = 5, width = 5)


## 02. plotting proportions over 65 ===========================================


FunPlotProportions("Oman", 
                   write = FALSE)

lapply(unique(threshold.1y$Location), 
       function(x) FunPlotProportions(x, 
                                      write = TRUE, 
                                      height = 4.5,
                                      width = 5,
                                      col.bg = background))




##  03. plotting the pyramid




background <- NA


lapply(unique(threshold.1y$Location)[10:14], 
       function(x) FunPyramidPlotNoAxes(
         country = x, lwd = 3,
         height = 5.5, width = 10,
         gap = 0, xlim = c(3,1.5),
         col.bg = background))


FunPyramidPlotNoAxes(country = "Algeria", gap = 0,xlim = c(3,1.5),
                     height = 5.5, width = 10, write = FALSE, background = )


