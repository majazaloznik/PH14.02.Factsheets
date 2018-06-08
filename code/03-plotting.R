##=============================================================================
## 00. preliminaries ==========================================================
## 01. plotting thresholds ====================================================
## 02. plotting proportions over 65 ===========================================
##=============================================================================



## 00. preliminaries ==========================================================
prop.over <- readRDS("data/processed/prop.over.rds")
threshold.1y <-readRDS("data/processed/threshold.1y.rds")
library(dplyr)


prop.over


FunPlotOldAge <- function(country = "Algeria", 
                          col.bg = "white",
                          col.20 = "gray85",
                          col.total = "palevioletred4",
                          col.male = "palevioletred3",
                          col.female = "palevioletred3"){
  threshold.1y %>% 
    filter(MidPeriod > 1980, MidPeriod < 2050) -> threshold.1y
  par(bg = col.bg)
  par(mar = c(3,3,1,0))
  plot(2000, 50, 
       xlim = c(1980, 2050), 
       ylim = c(50, 80),
       bty = "n",
       type = "n",
       ylab = "", xlab = "",
       axes = FALSE)
  threshold.1y %>% 
    group_by(Location) %>% 
    lines(Total.old.age.i ~ MidPeriod, . ,
          lwd = 3, col = col.20)
  for (i in seq(50, 80, 5)){
    lines(c(1980, 2050), c(i,i), col = "gray50", lty = 3)
  }
  
  threshold.1y %>% 
    filter(Location == country) %>% 
    lines(Total.old.age.i ~ MidPeriod, . ,
          lwd = 3, col = col.total )
  
  threshold.1y %>% 
    filter(Location == country) %>% 
    lines(Male.old.age.i ~ MidPeriod, . ,
          lwd = 3, col = col.male, lty = 1)
  
  threshold.1y %>% 
    filter(Location == country) %>% 
    lines(Female.old.age.i ~ MidPeriod, . ,
          lwd = 3, col = col.female, lty = 1)
  
  
  abline(v = 2015, lty = 2, lwd = 2, col = "gray50")
  axis(2, pos = 1980, las = 2)
  axis(1)
  
  lines(c(1980, 2050), c(50,50), col = "gray50", lty = 3)
}



par(mfrow = c(2,2))
FunPlotOldAge("Egypt")
FunPlotOldAge("Algeria")
FunPlotOldAge("Tunisia")
FunPlotOldAge("Turkey")

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
  