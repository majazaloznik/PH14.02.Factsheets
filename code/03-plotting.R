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




country <- "Iraq"
pop %>% 
  filter(Location == country,
         Time == 2015) -> pyramid
pop %>% 
  filter(Location == country,
         Time == 2050) -> pyramid.50

FunPyramidPlotNoAxes(pyramid$PropMale,
                     pyramid$PropFemale,
                     lx50 = pyramid.50$PropMale,
                     rx50 = pyramid.50$PropFemale,
                     lxcol = "lightcyan3", rxcol = "lightcyan3", border = "lightcyan3",
                     do.first = grid(ny = NA, col = "white", lty = 1, lwd = 1),
                     laxlab = 0:2, raxlab = 0:2, axes = FALSE, gap = 0, show.values = TRUE)


country <- "Iraq"
pop %>% 
  filter(Location == country,
         Time == 2015) -> pyramid
pop %>% 
  filter(Location == country,
         Time == 2050) -> pyramid.50

FunPyramidPlotNoAxes(pyramid$PropMale,
                     pyramid$PropFemale,
                     lx50 = pyramid.50$PropMale,
                     rx50 = pyramid.50$PropFemale,
                     labels = 1:length(pyramid$PopMale), gap = 0, add = TRUE)
axis(1)
