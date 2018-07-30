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

# rename country names with spaces - includegraphics cannot handle them
threshold.1y$Location[threshold.1y$Location == 
                        "Iran (Islamic Republic of)"] <- "Iran"
prop.over$Location[prop.over$Location == 
                        "Iran (Islamic Republic of)"] <- "Iran"
pop$Location[pop$Location == 
                     "Iran (Islamic Republic of)"] <- "Iran"

threshold.1y$Location[threshold.1y$Location == 
                        "State of Palestine"] <- "Palestine"
prop.over$Location[prop.over$Location == 
                     "State of Palestine"] <- "Palestine"
pop$Location[pop$Location == 
               "State of Palestine"] <- "Palestine"

threshold.1y$Location[threshold.1y$Location == 
                        "Saudi Arabia"] <- "SaudiArabia"
prop.over$Location[prop.over$Location == 
                     "Saudi Arabia"] <- "SaudiArabia"
pop$Location[pop$Location == 
               "Saudi Arabia"] <- "SaudiArabia"


threshold.1y$Location[threshold.1y$Location == 
                        "Syrian Arab Republic"] <- "Syria"
prop.over$Location[prop.over$Location == 
                     "Syrian Arab Republic"] <- "Syria"
pop$Location[pop$Location == 
               "Syrian Arab Republic"] <- "Syria"


threshold.1y$Location[threshold.1y$Location == 
                        "United Arab Emirates"] <- "UAE"
prop.over$Location[prop.over$Location == 
                     "United Arab Emirates"] <- "UAE"
pop$Location[pop$Location == 
               "United Arab Emirates"] <- "UAE"


col.total <- "slateblue1"
col.male <- "slateblue1"
col.female <- "slateblue1"
col.threshold <- "slateblue1"
col.65 <- "darkgoldenrod1"
background <- NA

width <- 10 # 5.74
height.1 <- .45* width * 6.218 / 5.74
height.2 <- .50 * width * 6.218 / 5.74

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
                                      col.threshold = col.threshold,
                                      col.65 = col.65,
                                      col.bg = background))

#FunPlotProportions("Oman", write = FALSE)



##  03. plotting the pyramid





lapply(unique(threshold.1y$Location), 
       function(x) FunPyramidPlotNoAxes(
         country = x, lwd = 3,
         height = height.2, width = 10,
         gap = 0, xlim = c(3,1.5),
         col.bg = background))


# FunPyramidPlotNoAxes(country = "Algeria", gap = 0,xlim = c(3,1.5),
 #                    height = 5.5, width = 10, write = FALSE )

