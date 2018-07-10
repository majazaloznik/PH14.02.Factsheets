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

lapply(unique(threshold.1y$Location), FUN = FunPlotThreshold)

FunPlotThreshold("Iraq", write = FALSE,  col.bg = "lightcyan2")


## 02. plotting proportions over 65 ===========================================


FunPlotProportions("Tunisia", 
                   col.bg = "lightcyan2",
                   write = FALSE)

lapply(unique(threshold.1y$Location), function(x) FunPlotProportions(x, write = FALSE))




##  03. plotting the pyramid




FunPyramidPlotNoAxes(country = "Iraq",
                     lxcol = "lightcyan3", rxcol = "lightcyan3", border = "lightcyan3",
                     do.first = grid(ny = NA, col = "lightcyan3", lty = 1, lwd = 1),
                     axes = FALSE, gap = 0, show.values = TRUE)



lapply(unique(threshold.1y$Location)[10:14], function(x) FunPyramidPlotNoAxes(country = x, 
                                                                       gap = 0, xlim = c(3,1.5)))


FunPyramidPlotNoAxes(country = "Tunisia", gap = 0, xlim = c(3,3))
axis(1)

max(pop$PropMale)
