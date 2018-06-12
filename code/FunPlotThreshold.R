

FunPlotThreshold <- function(country = "Algeria", 
                          col.bg = "white",
                          col.20 = "gray85",
                          col.total = "palevioletred4",
                          col.male = "palevioletred3",
                          col.female = "palevioletred3"){
  postscript(file=paste0("figures/","threshold-",country,".eps"),
             horiz=FALSE,onefile=FALSE,width=6,height=4,paper="special") 
  threshold.1y %>% 
    filter(Time > 1980, Time < 2050) -> threshold.1y
  par(bg = col.bg)
  par(mar = c(3,3,1,0))
  plot(2000, 50, 
       xlim = c(1980, 2050), 
       ylim = c(50, 80),
       bty = "n",
       type = "n",
       ylab = "", xlab = "",
       axes = FALSE,
       main = country)
  threshold.1y %>% 
    group_by(Location) %>% 
    lines(AgeGrp ~ Time, . ,
          lwd = 3, col = col.20)
  for (i in seq(50, 80, 5)){
    lines(c(1980, 2050), c(i,i), col = "gray50", lty = 3)
  }
  lines(c(1980, 2050), c(65,65), col = "gray50", lty = 3, lwd = 2)
  
  
  threshold.1y %>% 
    filter(Location == country) %>% 
    lines(AgeGrp ~ Time, . ,
          lwd = 3, col = col.total )
  
  threshold.1y %>% 
    filter(Location == country) %>% 
    lines(Male ~ Time, . ,
          lwd = 3, col = col.male, lty = 1)
  
  threshold.1y %>% 
    filter(Location == country) %>% 
    lines(Female ~ Time, . ,
          lwd = 3, col = col.female, lty = 1)
  
  
  abline(v = 2015, lty = 2, lwd = 2, col = "gray50")
  
  axis(2, pos = 1980, las = 2)
  axis(1)
  
  lines(c(1980, 2050), c(50,50), col = "gray50", lty = 3)
  dev.off()
}
