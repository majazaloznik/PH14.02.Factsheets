###############################################################################
## MAPS for PopHzn 14.2. "factsheet
###############################################################################
## 0. preliminaries  ##########################################################
## 1. plot regular map ########################################################
## 2. plot three cluster maps  ################################################
## 3. cluster diagrams ########################################################
###############################################################################


## 0. preliminaries  ##########################################################
###############################################################################
require(tidyverse)
require(rworldmap)
require(rworldxtra)
library(RColorBrewer)
library(maps)

.pardefault <- par(no.readonly = T)

## 0. load data      ##########################################################

# threshold for each country/age/gender combination
threshold.1y <-readRDS("data/processed/threshold.1y.rds")
# prop over threshold and over 65 for each country/age/gender combination
prop.over <- readRDS("data/processed/prop.over.rds")

# # get country and region code table
# urlfile<-'https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv'
# dsin <-read.csv(urlfile)
# ## This looks like a very useful file, so I'll save it for future use:
# write.csv(dsin, file = "data/raw/ISO.country.codes.csv", row.names = FALSE)
dsin <- read.csv( file = "data/raw/ISO.country.codes.csv")


## 0.2 clean data #############################################################
threshold.1y %>% 
  filter(Time == 2018) %>% 
  select(Location, threshold) -> threshold.2018

prop.over %>% 
  filter(Time == 2018) %>% 
  select(Location, prop.over.t) -> proportion.2018

# join with iso codes         
df <- as.data.frame(left_join(left_join(threshold.2018, proportion.2018),
                              dsin, 
                              by = c("Location" = "official_name_en")))

## add the data to the starter map
sPDF <- joinCountryData2Map(df, joinCode="ISO3", nameJoinColumn="ISO3166.1.Alpha.3")
# subset only Mena countries
sPDF.mena<-sPDF[which(sPDF$ISO3 %in% df$ISO3166.1.Alpha.3),]

## coordinates for country labels
sPDF.mena@data$X <- sPDF.mena@data$LON
sPDF.mena@data$Y <- sPDF.mena@data$LAT
levels(sPDF.mena@data$NAME) <- c(levels(sPDF.mena@data$NAME), "Palestine", "UAE")
sPDF.mena@data$NAME[sPDF.mena@data$NAME == "West Bank"] <- "Palestine"
sPDF.mena@data$NAME[sPDF.mena@data$NAME == "United Arab Emirates"] <- "UAE"
sPDF.mena@data$Y[sPDF.mena@data$NAME == "Morocco"] <- 32
sPDF.mena@data$X[sPDF.mena@data$NAME == "Israel"] <- 32
sPDF.mena@data$Y[sPDF.mena@data$NAME == "Israel"] <- 32
sPDF.mena@data$X[sPDF.mena@data$NAME == "Palestine"] <- 31
sPDF.mena@data$Y[sPDF.mena@data$NAME == "Palestine"] <- 33.5
sPDF.mena@data$X[sPDF.mena@data$NAME == "Lebanon"] <- 33
sPDF.mena@data$Y[sPDF.mena@data$NAME == "Lebanon"] <- 35
sPDF.mena@data$Y[sPDF.mena@data$NAME == "Bahrain"] <- 27
sPDF.mena@data$Y[sPDF.mena@data$NAME == "Saudi Arabia"] <- 24
sPDF.mena@data$X[sPDF.mena@data$NAME == "Qatar"] <- 52
sPDF.mena@data$X[sPDF.mena@data$NAME == "Jordan"] <- 38

sPDF.mena@data$psfrag <- as.character(sPDF.mena@data$ISO_A2)
sPDF.mena@data$psfrag[sPDF.mena@data$psfrag == "LY"] <- "L"

## colour palates
threshold.pal <- c(rev(brewer.pal(4, "OrRd")[3:4]),
                   "gold",
                   brewer.pal(5, "YlGn")[2:5])

## 1. plot regular map ########################################################
###############################################################################
#leg.x <- -12
#leg.y <- 45
w = 10
h = 4.5
#cex = 0.9
#yi = 1.2

par(xpd = TRUE)
par(.pardefault)
par(mar = c(0,0,0,0))



postscript(file=here::here(paste0("figures/","map1",".eps")),
           horiz=FALSE,onefile=FALSE,
           width=w,
           height=h,
           paper="special")
# plot background

par(mar = c(0,0,0,0))
plot(sPDF, xlim = c(-17, 62), ylim = c(13, 42),
     border=col.20, col=NA)
scale <- map.scale(ratio = FALSE, relwidth=0.2)

threshold.legend <- mapPolys(sPDF.mena, nameColumnToPlot = "threshold",
                             addLegend=FALSE,
                             mapTitle ="",  borderCol="gray40", 
                             numCats = 7,
                             catMethod = c(61, 63, 65, 66, 68, 70, 72, 74),
                             add = TRUE, 
                             colourPalette = threshold.pal)
addMapLegend( cutVector  = threshold.legend$cutVector, 
              legendLabels =  "all",
              colourVector = threshold.legend$colourVector,
              horizontal = FALSE,
              legendShrink = 0.7,
              legendMar = 65,
              labelFontSize = 0.7,
              mgp = c(3,.4,0),
              tcl = -.3)
text(sPDF.mena@data$X, sPDF.mena@data$Y, sPDF.mena@data$psfrag)

dev.off()




## Map of proportions #########################################################


postscript(file=here::here(paste0("figures/","map2",".eps")),
           horiz=FALSE,onefile=FALSE,
           width=w,
           height=h,
           paper="special")
# plot background

par(mar = c(0,0,0,0))
plot(sPDF, xlim = c(-12, 67), ylim = c(13, 42),
     border="grey70", col="white")
scale <- (map.scale(ratio = FALSE, relwidth=0.2))
proportion.legend <- mapPolys(sPDF.mena, nameColumnToPlot = "prop.over.t",
                             addLegend=FALSE,
                             mapTitle ="",  borderCol="gray40", 
                             numCats = 7,
                             catMethod = "pretty",
                             add = TRUE, 
                             colourPalette = (brewer.pal(7, "YlOrRd")))
addMapLegend( cutVector  = proportion.legend$cutVector, 
              legendLabels =  "all",
              colourVector = proportion.legend$colourVector,
              horizontal = FALSE,
              legendShrink = 0.7,
              legendMar = 3,
              labelFontSize = 0.7,
              mgp = c(3,.4,0),
              tcl = -.3)

text(sPDF.mena@data$X, sPDF.mena@data$Y, sPDF.mena@data$psfrag)


dev.off()

