getwd()
projectManchester <- "~/Desktop/Manchester Crimes/"
setwd(projectManchester)

library(sp)
library(maptools)
library(readr)

mancr1611 <- read.csv("2016-11-greater-manchester-street.csv") 
coords <- cbind(Longitude = as.numeric(as.character(mancr1611$Longitude)), Latitude = as.numeric(as.character(mancr1611$Latitude)))
crime.pts <- SpatialPointsDataFrame(coords, mancr1611[,-(5:6)], proj4string = CRS("+init=epsg:4326"))
#reproject the crime.pts data so that it can be display with Manchester LSOA data
crime.pts <- spTransform(crime.pts, CRS("+init=epsg:27700"))


library(rgdal)
#unzip BoundaryData.zip first
#read shapefile
manch.lsoa <- readShapeSpatial("england_low_soa_2001")

asb.pts <- crime.pts[crime.pts$Crime.type == "Anti-social behaviour", ]

#rgeos packages use for overlay operations in R
library(rgeos)

# This defines a new R function - it counts how many points fall into each polygon
poly.counts <- function (pts, polys) colSums(gContains(polys, pts, byid = TRUE))

#counts the number of crimes in each LSOA
asb.count <- poly.counts(asb.pts,manch.lsoa)


# draw choropleth map of asb counts for each LSOA
library(classInt)
library(RColorBrewer)
# First, add an ASB event count column to the 'manch.lsoa' SpatialPolygonsDataFrame
manch.lsoa@data$asb.count <- asb.count
# Now draw a choropleth map - using the same method as before
var <- manch.lsoa@data[,"asb.count"]
breaks <- classIntervals(var, n = 6, style = "fisher")$brk
my_colours <- brewer.pal(6, "Greens")
plot(manch.lsoa, col = my_colours[findInterval(var, breaks, all.inside = TRUE)],   axes = FALSE, border = rgb(0.8,0.8,0.8))
breaks <- list(b=breaks,c=my_colours)
