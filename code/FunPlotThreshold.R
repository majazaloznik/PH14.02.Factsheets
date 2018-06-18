##=============================================================================
## FunPlotThreshod  ===========================================================
## FunPlotProportions  ========================================================
##=============================================================================


## FunPlotThreshod  ===========================================================
#' FunPlotThreshod - Plot old-age threshold estimates and projections for 
#' individual countries
#' 
#' Using a preformated dataframe `threshold.1y` which has data on the 
#' following variables: 
#' Location = country 
#' Time = year
#' Female, Male and AgeGrp are the thresholds for women, men and both
#' This function plots for a single country the three trends - men, women and 
#' both, while also in gray printing the trends for the other 20 countries
#' for total only. 
#'
#' @param country - country of interest
#' @param col.bg - colour of background
#' @param col.20- colour for the 20 grayed out countries underlayed 
#' @param col.total - main colour for old age threshold of whole population
#' @param col.male - colour for men
#' @param col.female - colour for women
#' @param write - logical whether to write to postscript
#' @param height - height of postscript file
#' @param width - width of postscript file
#'
#' @return - plot to device or written to .eps into the 'figures/` folder
#' @export
#'
#' @examples
#' 
#' 
FunPlotThreshold <- function(country = "Algeria", 
                          col.bg = "white",
                          col.20 = "gray85",
                          col.total = "palevioletred4",
                          col.male = "palevioletred3",
                          col.female = "palevioletred3",
                          write = TRUE, 
                          height = 4,
                          width = 6,
                          col.grid = "white",
                          lty.grid = 1,
                          lwd.grid = 2){

  if(write) {
    postscript(file=here::here(paste0("figures/","threshold-",country,".eps")),
               horiz=FALSE,onefile=FALSE,
               width=width,
               height=height,
               paper="special") }
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

  for (i in seq(50, 80, 5)){
    lines(c(1980, 2050), c(i,i), col = col.grid, lty = lty.grid,
          lwd = lwd.grid)
  }
  
  # background lines
  threshold.1y %>% 
    group_by(Location) %>% 
    lines(AgeGrp ~ Time, . ,
          lwd = 3, col = col.20)
  lines(c(1980, 2050), c(65,65), col = "gray50", lty = 3, lwd = 2)
  
  # total
  threshold.1y %>% 
    filter(Location == country) %>% 
    lines(AgeGrp ~ Time, . ,
          lwd = 2, col = col.total )
  
  # men
  threshold.1y %>% 
    filter(Location == country) %>% 
    lines(Male ~ Time, . ,
          lwd = 1, col = col.male)
  
  #women
  threshold.1y %>% 
    filter(Location == country) %>% 
    lines(Female ~ Time, . ,
          lwd = 3, col = col.female)
  
  
  abline(v = 2015, lty = 2, lwd = 2, col = "gray50")
  
  axis(2, pos = 1980, las = 2)
  axis(1)
  
  if(write) {
    dev.off() }
}



## FunPlotProportions  ========================================================

#' Title
#'
#' @param country 
#' @param col.bg 
#' @param col.20 
#' @param col.65 
#' @param col.threshold 
#' @param write 
#' @param height 
#' @param width 
#' @param col.grid 
#' @param lty.grid 
#' @param lwd.grid 
#'
#' @return
#' @export
#'
#' @examples
FunPlotProportions <- function(country = "Algeria", 
                             col.bg = "white",
                             col.20 = "gray85",
                             col.65 = "mediumorchid2",
                             col.threshold = "darkgoldenrod1",
                             write = TRUE, 
                             height = 4,
                             width = 6,
                             col.grid = "white",
                             lty.grid = 1,
                             lwd.grid = 2){
  
  if(write) {
    postscript(file=here::here(paste0("figures/","proportion-",country,".eps")),
               horiz=FALSE,onefile=FALSE,
               width=width,
               height=height,
               paper="special") }
  prop.over %>% 
    filter(Time > 1980, Time < 2050) -> prop.over
  par(bg = col.bg)
  par(mar = c(3,3,1,0))
  plot(2000, 50, 
       xlim = c(1980, 2050), 
       ylim = c(0, .2),
       bty = "n",
       type = "n",
       ylab = "", xlab = "",
       axes = FALSE,
       main = country)
  
  for (i in seq(0, .2, .05)){
    lines(c(1980, 2050), c(i,i), col = col.grid, lty = lty.grid,
          lwd = lwd.grid)
  }

  
  prop.over%>% 
    filter(Location== country) %>% 
    lines(prop.over.65 ~ Time, . ,
          lwd = 3, col = col.65)
  prop.over%>% 
    filter(Location== country) %>% 
    lines(prop.over.t ~ Time, . ,
          lwd = 3, col = col.threshold)
  
  
  abline(v = 2015, lty = 2, lwd = 2, col = "gray50")
  
  axis(2, pos = 1980, las = 2)
  axis(1)
  

  if(write) {
    dev.off() }
}



#' Title
#'
#' @param lx 
#' @param rx 
#' @param labels 
#' @param top.labels 
#' @param main 
#' @param laxlab 
#' @param raxlab 
#' @param unit 
#' @param lxcol 
#' @param rxcol 
#' @param gap 
#' @param space 
#' @param ppmar 
#' @param labelcex 
#' @param add 
#' @param xlim 
#' @param show.values 
#' @param ndig 
#' @param do.first 
#' @param axes 
#' @param density 
#' @param angle 
#' @param lwd 
#' @param border 
#' @param ylab 
#' @param col.overlay 
#' @param lx50 
#' @param rx50 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
FunPyramidPlotNoAxes <- function (lx, rx, labels = NA, top.labels = c("Male", "Age", 
                                                                      "Female"), main = "", 
                                  laxlab = NULL, raxlab = NULL, unit = "", 
                                  lxcol, rxcol, gap = 1, space = 0.2, ppmar = c(4, 2, 4, 2), 
                                  labelcex = 1, add = FALSE, xlim, show.values = FALSE, ndig = 1, 
                                  do.first = NULL, axes=FALSE, density=NULL, angle=45, lwd = 2, border="black",
                                  ylab="",
                                  col.overlay = "red", lx50, rx50) 
{if (axes==FALSE) show.values <- FALSE
if (any(c(lx, rx) < 0, na.rm = TRUE)) 
  stop("Negative quantities not allowed")
lxdim <- dim(lx)
rxdim <- dim(rx)
ncats <- ifelse(!is.null(lxdim), dim(lx)[1], length(lx))
if (length(labels) == 1) 
  labels <- 1:ncats
ldim <- length(dim(labels))
nlabels <- ifelse(ldim, length(labels[, 1]), length(labels))
if (nlabels != ncats) 
  stop("lx and labels must all be the same length")
if (missing(xlim)) 
  xlim <- rep(ifelse(!is.null(lxdim), 
                     ceiling(max(c(rowSums(lx), rowSums(rx)), na.rm = TRUE)),
                     ceiling(max(c(lx, rx, lx50, rx50),  na.rm = TRUE))), 2)
if (!is.null(laxlab) && xlim[1] < max(laxlab)) 
  xlim[1] <- max(laxlab)
if (!is.null(raxlab) && xlim[2] < max(raxlab)) 
  xlim[2] <- max(raxlab)
oldmar <- par("mar")
if (!add) {
  par(mar = ppmar, cex.axis = labelcex)
  plot(0, xlim = c(-(xlim[1] + gap), xlim[2] + gap), 
       ylim = c(0, ncats + 1), type = "n", axes = FALSE, xlab = "", 
       ylab = ylab, xaxs = "i", yaxs = "i", main = main)
  if (!is.null(do.first)) 
    eval(parse(text = do.first))
  if (is.null(laxlab)) {
    laxlab <- seq(xlim[1] - gap, 0, by = -1)
    if (axes==TRUE) axis(1, at = -xlim[1]:-gap, labels = laxlab)
  }
  else {if (axes==TRUE) axis(1, at = -(laxlab + gap), labels = laxlab)}
  if (is.null(raxlab)) {
    raxlab <- 0:(xlim[2] - gap)
    if (axes==TRUE) axis(1, at = gap:xlim[2], labels = raxlab)
  }
  else {if (axes==TRUE) axis(1, at = raxlab + gap, labels = raxlab)}
  if (gap > 0) {
    if (!is.null(lxdim)) 
      if (axes==TRUE) axis(2, at = 1:ncats, labels = rep("", ncats), 
                           pos = gap, tcl = -0.25)
    else {if (axes==TRUE) axis(2, at = 1:ncats * as.logical(rx + 1), labels = rep("", 
                                                                                  ncats), pos = gap, tcl = -0.25)}
    if (!is.null(lxdim)) 
      if (axes==TRUE) axis(4, at = 1:ncats, labels = rep("", ncats), 
                           pos = -gap, tcl = -0.25)
    else {if (axes==TRUE) axis(4, at = 1:ncats * as.logical(lx + 1), labels = rep("", 
                                                                                  ncats), pos = gap, tcl = -0.25)}
  }
  if (is.null(dim(labels)) & axes==TRUE) {
    if (gap) 
      text(0, 1:ncats, labels, cex = labelcex)
    else {
      text(xlim[1], 1:ncats, labels, cex = labelcex, 
           adj = 0)
      text(xlim[2], 1:ncats, labels, cex = labelcex, 
           adj = 1)
    }
  }
  else { if ( axes==TRUE) {
    if (gap) {
      lpos <- -gap
      rpos <- gap
    }
    else {
      lpos <- -xlim[1]
      rpos <- xlim[2]
    }
    text(lpos, 1:ncats, labels[, 1], pos = 4, cex = labelcex, 
         adj = 0)
    text(rpos, 1:ncats, labels[, 2], pos = 2, cex = labelcex, 
         adj = 1)
  }}
  mtext(top.labels, 3, 0, at = c(-xlim[1]/2, 0, xlim[2]/2), 
        adj = 0.5, cex = labelcex)
  mtext(c(unit, unit), 1, 2, at = c(-xlim[1]/2, xlim[2]/2))
}
halfwidth <- 0.5 


rect(-(lx + gap), 1:ncats - halfwidth, rep(-gap, ncats), 
     1:ncats + halfwidth, col = lxcol, density=density, angle=angle, lwd=lwd, border=border)

rect(rep(gap, ncats), 1:ncats - halfwidth, (rx + gap), 
     1:ncats + halfwidth, col = rxcol, density=density, angle=angle, lwd=lwd, border=border)

if (lx50 & rx50) {
  lines(x = c(-gap, rep(rev(-(lx50 + gap)), each = 2), -gap), 
        y = c(rep((ncats):0 + halfwidth, each = 2) ), 
        col = col.overlay, lwd = 2 )
  
  lines(x = c(-gap, rep(rev((rx50 + gap)), each = 2), -gap), 
        y = c(rep((ncats):0 + halfwidth, each = 2) ), 
        col = col.overlay, lwd = 2 )
}


if (show.values) {
  par(xpd = TRUE)
  text(-(gap + lx), 1:ncats, round(lx, ndig), pos = 2, 
       cex = labelcex)
  text(gap + rx, 1:ncats, round(rx, ndig), pos = 4, 
       cex = labelcex)
  par(xpd = FALSE)
}
return(oldmar)
}
