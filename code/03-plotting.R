##=============================================================================
## 00. preliminaries ==========================================================
## 01. plotting thresholds ====================================================
## 02. plotting proportions over 65 ===========================================
##=============================================================================


## 00. preliminaries ==========================================================
library(dplyr)
library(magrittr)
source("code/FunPlotThreshold.R")

# threshold for each country/age/gender combination
threshold.1y <-readRDS("data/processed/threshold.1y.rds")
# prop over threshold and over 65 for each country/age/gender combination
prop.over <- readRDS("data/processed/prop.over.rds")
# single age populations for each each country/age/gender combination
pop <- readRDS("data/processed/mena.pop.rds")

col.total <- "darkgoldenrod2"
col.male <- "darkgoldenrod1"
col.female <- "darkgoldenrod1"
background <- NA

height.1 <- 3.341603
height.2 <- 4.1

## 01. plotting thresholds ====================================================

lapply(unique(threshold.1y$Location),
       function(x)  FunPlotThreshold(x,
                                     height = height.1, width = 5,
                                     col.bg = background,
                                     col.total = col.total,
                                     col.male = col.male,
                                     col.female = col.female))

# FunPlotThreshold("Iraq", write = FALSE,  col.bg = "lightcyan2",
#                 height = 5, width = 5)


## 02. plotting proportions over 65 ===========================================



lapply(unique(threshold.1y$Location), 
       function(x) FunPlotProportions(x, 
                                      write = TRUE, 
                                      height = height.1,
                                      width = 5,
                                      col.bg = background))

#FunPlotProportions("Oman", write = FALSE)



##  03. plotting the pyramid





lapply(unique(threshold.1y$Location), 
       function(x) FunPyramidPlotNoAxes(
         country = x, lwd = 3,
         height = height.2, width = 10,
         gap = 0, xlim = c(3,1.5),
         col.bg = background))


FunPyramidPlotNoAxes(country = "Algeria", gap = 0,xlim = c(3,1.5),
                     height = 5.5, width = 10, write = FALSE, background = )


