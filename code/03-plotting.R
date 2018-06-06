


FunPlotOldAge <- function(country = "Algeria", 
                          col.bg = "white",
                          col.20 = "gray85",
                          col.total = "palevioletred4",
                          col.male = "palevioletred3",
                          col.female = "palevioletred3"){
  mena.old.age %>% 
    filter(MidPeriod > 1980, MidPeriod < 2050) -> mena.old.age
  par(bg = col.bg)
  par(mar = c(3,3,1,0))
  plot(2000, 50, 
       xlim = c(1980, 2050), 
       ylim = c(50, 80),
       bty = "n",
       type = "n",
       ylab = "", xlab = "",
       axes = FALSE)
  mena.old.age %>% 
    group_by(Location) %>% 
    lines(Total ~ MidPeriod, . ,
          lwd = 3, col = col.20)
  for (i in seq(50, 80, 5)){
    lines(c(1980, 2050), c(i,i), col = "gray50", lty = 3)
  }
  
  mena.old.age %>% 
    filter(Location == country) %>% 
    lines(Total ~ MidPeriod, . ,
          lwd = 3, col = col.total )
  
  mena.old.age %>% 
    filter(Location == country) %>% 
    lines(Male ~ MidPeriod, . ,
          lwd = 3, col = col.male, lty = 1)
  
  mena.old.age %>% 
    filter(Location == country) %>% 
    lines(Female ~ MidPeriod, . ,
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
