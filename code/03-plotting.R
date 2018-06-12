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

## 01. plotting thresholds ====================================================

lapply(unique(threshold.1y$Location), FUN = FunPlotThreshold)



FunPlotThreshold("Egypt")
FunPlotThreshold("Algeria")
FunPlotThreshold("Tunisia")
FunPlotThreshold("Turkey")

## 02. plotting proportions over 65 ===========================================





country = "Egypt"
par(mar = c(3,3,1,0))
plot(2000, 0, 
     xlim = c(1980, 2050), 
     ylim = c(0, .2),
     bty = "n",
     type = "n",
     ylab = "", xlab = "",
     axes = FALSE)
prop.over%>% 
  filter(Location== country) %>% 
  lines(prop.over.65 ~ Time, . ,
        lwd = 3) 
prop.over%>% 
  filter(Location== country) %>% 
  lines(prop.over.t ~ Time, . ,
        lwd = 3, col = "red") 
axis(2, pos = 1980, las = 2)
axis(1)
  