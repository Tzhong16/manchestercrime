library(ggmap)

qmap('Manchester')
# use postcodes to get the google map eg: qmap('M1 6FT', zoom = 16)
qmap('M1 6FT', zoom = 16, maptype = 'hybrid') # other maptype 'satellite'


# plotting spatial data
projectManchester <- "~/Desktop/Manchester Crimes/" # path to folder that holds multiple .csv files
setwd(projectManchester)

crimes <- read.csv("2016-11-greater-manchester-street.csv") 

head(crimes)

#change data.frame object to spatial object 
library(sp)
coords <- cbind(Longitude = as.numeric(as.character(crimes$Longitude)), Latitude = as.numeric(as.character(crimes$Latitude)))
crime.pts <- SpatialPointsDataFrame(coords, crimes[, -(5:6)], proj4string = CRS("+init=epsg:4326"))
plot(crime.pts, pch = ".", col = "darkred")


#plot the hybrid Google Maps 
map <- qmap('Manchester', zoom = 12, maptype = 'hybrid')
#plot the crime points on top
map + geom_point(data = crimes, aes(x = Longitude, y = Latitude), color="red", size=2, alpha=0.2)


#plotting polygons 
library(maptools)
#read in as BNG 
manchester <- readShapeSpatial('Great Manchester/england_low_soa_2001', proj4string = CRS("+init=epsg:27700"))
plot(manchester)

#reproject to lat long
manchester <- spTransform(manchester, CRS("+init=epsg:4326"))
#convert to a data.frame for use with ggplot2/ggmap
data <- fortify(manchester)
#plot, centered on  to show the whole Great Manchester area
qmap('Salford', zoom = 10) +
  geom_polygon(aes(x = long, y = lat, group = group), data = data,
               colour = 'white', fill = 'black', alpha = .3, size = .3)




##########################################################
#plot choropleth
##########################################################
#plotting polygon focus on Anti-society behavior in Greate Manchester
asb.pts <- crime.pts[crime.pts$Crime.type == "Anti-social behaviour", ]
plot(manchester)
plot(asb.pts, pch = ".", col = "red", add = TRUE)

library(rgeos) 
#This defines a new R function - it counts how many points fall into each polygon
poly.counts <- function (pts, polys) colSums(gContains(polys, pts, byid = TRUE))
#The line below actually counts the number of crimes in each LSOA
asb.count <- poly.counts(asb.pts, manchester)

# First, add an ASB event count column to the 'manchester.lsoa' SpatialPolygonsDataFrame
manchester@data$asb.count <- asb.count
# Fortify the manchester data for use with ggplot
data <- fortify(manchester, region = "zonecode")
# Add on the asb.count to the object data
data <- merge(data,manchester@data, by.x="id", by.y = "zonecode")
#plot, centered on Melling to show the whole Merseyside area
qmap('Salford', zoom = 10) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asb.count), data = data)





################################################################
#Density plots
################################################################
#plot the hybrid Google Maps basemap
map <- qmap('Manchester', zoom = 12, maptype = 'hybrid')
#plot the crime points on top
map + geom_point(data = crimes, aes(x = Longitude, y = Latitude), color="red", size=2, alpha=0.3)

#plot the roads Google Maps basemap
map <- qmap('Manchester', zoom = 12, maptype = 'roadmap')

#plot the density map
map + stat_density2d(
  aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..*2), 
  size = 2, bins = 5, data = crimes, geom = "polygon") +
  scale_fill_gradient(low = "black", high = "red")





#####################################################################

##########################################################
#plot location of totals crimes
library(dplyr)
library(stringr)
library(data.table)
library(zoo)
crimes <- read.csv("2016-11-greater-manchester-street.csv") 
coords <- cbind(Longitude = as.numeric(as.character(crimes$Longitude)), Latitude = as.numeric(as.character(crimes$Latitude)))
crime.pts <- SpatialPointsDataFrame(coords, crimes[,-(5:6)], proj4string = CRS("+init=epsg:4326"))
plot(crime.pts,pch='.',col='darkred')

library(maptools)
library(rgdal)
manch.lsoa <- readShapeSpatial("england_low_soa_2001")
plot(manch.lsoa)
crime.pts <- spTransform(crime.pts, CRS("+init=epsg:27700"))
plot(manch.lsoa,border='grey')
plot(crime.pts,pch='*',col='red',add=TRUE)
title(main="Figure.4 Crimes in Manchester, 11-2016", col.main="grey50", font.main=3,cex.main=2)





#plot locations of robbery
crimes <- read.csv("2016-11-greater-manchester-street.csv") 
coords <- cbind(Longitude = as.numeric(as.character(mancr1611$Longitude)), Latitude = as.numeric(as.character(mancr1611$Latitude)))
crime.pts <- SpatialPointsDataFrame(coords, mancr1611[, -(5:6)], proj4string = CRS("+init=epsg:4326"))
plot(crime.pts, pch = ".", col = "darkred")
map <- qmap('Manchester', zoom = 12, maptype = 'hybrid')
#plot the crime points on top
map + geom_point(data = mancr1611, aes(x = Longitude, y = Latitude), color="red", size=3, alpha=0.5)
library(maptools)




#plot location of totals crimes
library(dplyr)
library(stringr)
library(data.table)
library(zoo)
crimes <- read.csv("2016-11-greater-manchester-street.csv") 
crimes %>%    # ` ` used to select the column name which has space
  filter(str_detect(LSOA.name, 'Manchester')) %>%
  filter(str_detect(Crime.type, 'Other theft'))    -> crimes
coords <- cbind(Longitude = as.numeric(as.character(crimes$Longitude)), Latitude = as.numeric(as.character(crimes$Latitude)))
crime.pts <- SpatialPointsDataFrame(coords, crimes[,-(5:6)], proj4string = CRS("+init=epsg:4326"))
plot(crime.pts,pch='.',col='darkred')

library(maptools)
library(rgdal)
manch.lsoa <- readShapeSpatial("england_low_soa_2001")
plot(manch.lsoa)
crime.pts <- spTransform(crime.pts, CRS("+init=epsg:27700"))
plot(manch.lsoa,border='grey')
plot(crime.pts,pch='*',col='red',add=TRUE)
title(main="Figure.8 Other theft in Manchester, 11-2016", col.main="grey50", font.main=3,cex.main=2)
