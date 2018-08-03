## funciton for threshold legend

FunPlotThresholdLedge <- function() {
  
  postscript(file=here::here(paste0("figures/","threshold-legend.eps")),
             horiz=FALSE,onefile=FALSE,
             width=4,
             height=4,
             paper="special") 
  
  threshold.1y %>% 
    filter(Time > 1980, Time < 2050) -> threshold.1y
  par(mar = c(2,2,2.5,0))
  plot(2000, 50, 
       xlim = c(1990, 2040), 
       ylim = c(60, 75),
       bty = "n",
       type = "n",
       ylab = "", xlab = "",
       axes = FALSE)
  mtext("m", side = 3, line = .5)
  
  for (i in seq(55, 80, 5)){
    lines(c(2000, 2030), c(i,i), col = "lightcyan3", lty = 1,
          lwd = 1)
  }
  
  # background lines
  threshold.1y %>% 
    group_by(Location) %>% 
    filter(Time > 2005, Time < 2025) %>% 
    lines(AgeGrp ~ Time, . ,
          lwd = 3, col = col.20)
  lines(c(2000, 2030), c(65,65), col = "gray50", lty = 3, lwd = 2)
  
  # total
  threshold.1y %>% 
    filter(Location == "Bahrain") %>% 
    filter(Time > 2005, Time < 2025) %>% 
    lines(AgeGrp ~ Time, . ,
          lwd = 3, col = col.total )
  
  # men
  threshold.1y %>% 
    filter(Location == "Bahrain") %>% 
    filter(Time > 2005, Time < 2025) %>% 
    lines(Male ~ Time, . ,
          lwd = 3, col = col.male, lty = "63")
  
  #women
  threshold.1y %>% 
    filter(Location == "Bahrain") %>% 
    filter(Time > 2005, Time < 2025) %>% 
    lines(Female ~ Time, . ,
          lwd = 3, col = col.female, lty = "63")
  
  
  text(2029, 67, "a")
  text(2029, 73, "g")
  text(2029, 61, "b")
  
  dev.off()
}


# proporiton lenend
FunPlotProportionLedge <- function() {
  
  
postscript(file=here::here(paste0("figures/","proportion-legend.eps")),
          horiz=FALSE,onefile=FALSE,
          width=4,
          height=4,
          paper="special") 

par(bg = col.bg )
prop.over %>% 
  filter(Time > 1980, Time < 2050) -> prop.over
par(bg = col.bg)
par(mar = c(3,2,2.5,0))
plot(2000, 50, 
     xlim = c(2010, 2080), 
     ylim = c(0, .2),
     bty = "n",
     type = "n",
     ylab = "", xlab = "",
     axes = FALSE)
mtext("n", side = 3, line = .5)
for (i in seq(0, .2, .05)){
  lines(c(2020, 2050), c(i,i), col = col.grid, lty = lty.grid,
        lwd = lwd.grid)
}


prop.over%>% 
  filter(Location== "Syrian Arab Republic") %>% 
  filter(Time > 2020) %T>% 
  lines(prop.over.65 ~ Time, . ,
        lwd = 3, col = col.65) %>% 
  summarise(max = max(prop.over.65 )) -> m.65

prop.over%>% 
  filter(Location== "Syrian Arab Republic") %>% 
  filter(Time > 2020) %T>% 
  lines(prop.over.t ~ Time, . ,
        lwd = 3, col = col.threshold)

text(2053, .12, "h")
text(2053, .06, "i")

dev.off() 

}


# pyramid legend
FunPyramidPlotLedge <- function(country = "Jordan",
                                labels = NA, 
                                top.labels = NULL, 
                                main = "", 
                                laxlab = NULL, raxlab = NULL, unit = "", 
                                gap = 0, space = 0.2, 
                                ppmar = c(2.5, 0, 0, 0), 
                                labelcex = 1, add = FALSE, 
                                xlim = c(3,1.5), show.values = FALSE, ndig = 1, 
                                do.first = NULL, 
                                axes=FALSE, 
                                density=NULL, 
                                angle=45, 
                                lwd = 3, 
                                ylab="",
                                col.overlay = "red",
                                lxcol = col.pyramid, 
                                rxcol = col.pyramid, 
                                border = col.pyramid,
                                col.bg = NA,
                                write = TRUE,
                                width = 8,
                                height = 5) {
  # get data
  
  pop %>% 
    filter(Location == country,
           Time == 2015) -> pyramid
  pop %>% 
    filter(Location == country,
           Time == 2050) -> pyramid.50
  lx <- pyramid$PropMale
  rx <- pyramid$PropFemale
  lx.50 <- pyramid.50$PropMale
  rx.50 <- pyramid.50$PropFemale
  # get thresholds and pop at that age
  lx.t <- threshold.1y %>% 
    filter(Location == country,
           Time == 2015) %>% 
    pull(Male)
  lx.t.pop <- lx[floor(lx.t )]
  rx.t <- threshold.1y %>% 
    filter(Location == country,
           Time == 2015) %>% 
    pull(Female)
  rx.t.pop <- rx[floor(rx.t )]
  # get thresholds and pop at that age in 2050
  lx.t.50 <- threshold.1y %>% 
    filter(Location == country,
           Time == 2050) %>% 
    pull(Male)
  lx.t.50.pop <- lx.50[floor(lx.t.50)]
  rx.t.50 <- threshold.1y %>% 
    filter(Location == country,
           Time == 2050) %>% 
    pull(Female)
  rx.t.50.pop <- rx.50[floor(rx.t.50 )]
  if(write) {
    postscript(file=here::here(paste0("figures/","pyramid-legend.eps")),
               horiz=FALSE,onefile=FALSE,
               width=width,
               height=height,
               paper="special") }
  par(bg = col.bg )
  show.values <- FALSE
  
  ncats <- length(lx)
  if (length(labels) == 1) 
    labels <- 1:ncats
  nlabels <- ifelse(length(dim(labels)), length(labels[, 1]), length(labels))
  
  
  if (!is.null(laxlab) && xlim[1] < max(laxlab)) 
    xlim[1] <- max(laxlab)
  if (!is.null(raxlab) && xlim[2] < max(raxlab)) 
    xlim[2] <- max(raxlab)
  
  oldmar <- par("mar")
  
  par(mar = ppmar, cex.axis = labelcex)
  
  plot(0, xlim = c(-(xlim[1] + gap), xlim[2] + gap), 
       ylim = c(30, ncats + 2+30), type = "n", axes = FALSE, xlab = "", 
       ylab = ylab, xaxs = "r", yaxs = "i", main = main)
  
  
  if (!is.null(do.first)) 
  {eval(parse(text = do.first))} else {
    lines(c(1,1), c(0,70), col = lxcol)
    lines(c(-1,-1), c(0,70), col = lxcol)
    lines(c(.5,.5), c(0,85), col = lxcol, lty = "62")
    lines(c(-.5,-.5), c(0,85), col = lxcol, lty = "62")
  }
  if (is.null(laxlab)) {
    laxlab <- seq(xlim[1] - gap, 0, by = -1)
    if (axes==TRUE) axis(1, at = -xlim[1]:-gap, labels = laxlab)
  }  else {if (axes==TRUE) axis(1, at = -(laxlab + gap), labels = laxlab)}
  if (is.null(raxlab)) {
    raxlab <- 0:(xlim[2] - gap)
    if (axes==TRUE) axis(1, at = gap:xlim[2], labels = raxlab)
  }  else {if (axes==TRUE) axis(1, at = raxlab + gap, labels = raxlab)}
  if (gap > 0) {
    if (axes==TRUE) {
      axis(2, at = 1:ncats, 
           labels = rep("", ncats), pos = gap, tcl = -0.25)
      axis(4, at = 1:ncats, 
           labels = rep("", ncats), pos = gap, tcl = -0.25)
    }}
  if (is.null(dim(labels)) & axes==TRUE) {
    if (gap) 
      text(0, 1:ncats, labels, cex = labelcex)
    else {
      text(xlim[1], 1:ncats, labels, cex = labelcex, 
           adj = 0)
      text(xlim[2], 1:ncats, labels, cex = labelcex, 
           adj = 1)
    }
  }  else { if ( axes==TRUE) {
    if (gap) {
      lpos <- -gap
      rpos <- gap
    }    else {
      lpos <- -xlim[1]
      rpos <- xlim[2]
    }
    text(lpos, 1:ncats, labels[, 1], pos = 4, cex = labelcex, 
         adj = 0)
    text(rpos, 1:ncats, labels[, 2], pos = 2, cex = labelcex, 
         adj = 1)
  }}
  mtext(top.labels, 3, 0, at = c(-1, 0, 1), 
        adj = c(0.5,0.5), cex = labelcex)
  
  
  halfwidth <- 0.5 
  
  # main plotting of pyramid
  rect(-(lx + gap), 1:ncats - halfwidth, rep(-gap, ncats), 
       1:ncats + halfwidth, col = lxcol, density=density, angle=angle, lwd=lwd, border=border)
  
  rect(rep(gap, ncats), 1:ncats - halfwidth, (rx + gap), 
       1:ncats + halfwidth, col = rxcol, density=density, angle=angle, lwd=lwd, border=border)
  lines(x = c(-1.5, 1.1), y = c(65, 65), lty = "11", col = border, lwd = lwd)
  
  lines(x = c(-lx.t.pop, 0), y = c(lx.t, lx.t), lty = 1, col = col.male, lwd = lwd)
  lines(x = c(rx.t.pop, 0), y = c(rx.t, rx.t), lty = 1, col = col.male, lwd = lwd)
  
  # plotting of 2050 overlay
  lines(x = c(-lx.t.50.pop, 0), y = c(lx.t.50, lx.t.50), lty = 1, col = col.male, lwd = lwd)
  lines(x = c(rx.t.50.pop, 0), y = c(rx.t.50, rx.t.50), lty = 1, col = col.female, lwd = lwd)
  
  lines(x = c(-gap, rep(rev(-(lx.50 + gap)), each = 2), -gap), 
        y = c(rep((ncats):0 + halfwidth, each = 2) ), 
        col = col.overlay, lwd = 3)
  
  lines(x = c(-gap, rep(rev((rx.50 + gap)), each = 2), -gap), 
        y = c(rep((ncats):0 + halfwidth, each = 2) ), 
        col = col.overlay, lwd = 3 )
  
  lines(x = c(0,0), y = c(0,100), col = "white")
  text(-0.5, 100, "o")
  
  rect(-3.1,38, -2.9, 42, col = lxcol, border = lxcol)
  rect(-3.1,30, -2.9, 35, border = col.overlay, lwd = 3)
  
  
  dev.off() 
}
