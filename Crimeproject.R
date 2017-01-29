getwd()
projectManchester <- "~/Desktop/Manchester Crimes/"
setwd(projectManchester)

library(data.table)
library(ggplot2)
library(dplyr)
mancr1611 <- fread("2016-11-greater-manchester-street.csv")
head(mancr1611)
names(mancr1611)
str(mancr1611)
#library(dtplyr)
glimpse(mancr1611)
summary(mancr1611)
table(mancr1611$"Crime type")

#union(table1, table2)

names(mancr1611)

mancr1611 %>%
  select(Month, Longitude, Latitude, `Crime type`) %>%   # ` ` used to select the column name which has space
  rename( Crimetype =`Crime type`) %>%  
  group_by(Crimetype) %>%
  summarise(nCrime = n())  -> crime1611
#sum the crimes
summarize(crime1611, sum = sum(nCrime))

# g <- ggplot(mancr1611, aes(`Crime type`)) # sums the crime type
# g + geom_bar()

#plot bar
# hjust scale 0-1 to set the theme location
# str_wrap function comes from stringr, use for adjusting long label name
library(stringr)

ggplot(crime1611, aes(Crimetype, nCrime, fill = Crimetype)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = function(Crimetype) str_wrap(Crimetype, width = 10)) +
  ggtitle("Criminal Numbers in Manchester, 2016 Nov") +    
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Crime type") + 
  ylab("Number of Crimes") 

# reorder bar in geom_bar
ggplot(crime1611, aes(x = reorder(Crimetype, -nCrime), nCrime, fill = Crimetype)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = function(Crimetype) str_wrap(Crimetype, width = 10)) +
  ggtitle("Criminal Numbers in Manchester, 2016 Nov") +    
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Crime type") + 
  ylab("Number of Crimes") 

library(ggplot2)
library(ggmap)

#get the latitude and longtitude of Manchester from google
#one way to get the map
city <- "MANCHESTER" 
geocode(city)
mapImageData3 <- get_map(location = c(lon = -2.242631, lat = 53.48076),
                         color = "color",
                         source = "google",
                         maptype = "roadmap",
                         zoom = 10)

ggmap(mapImageData3,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")

# another way to get the map
qmap('MANCHESTER', zoom = 10, maptype ="roadmap" )

map <- qmap('Manchester', zoom = 12, maptype = 'hybrid')
map + geom_point(data = mancr1611, aes(x = Longitude, y = Latitude), color="red", size=0.5, alpha=0.5)







#plotting Polygons

getwd()
projectManchester <- "~/Desktop/Manchester Crimes/"
setwd(projectManchester)

#load library
library(sp)
library(maptools)
library(data.table)
library(dplyr)


mancr1611 <- fread("2016-11-greater-manchester-street.csv") 
glimpse(mancr1611)

#To displays dot on polygons file, the data need to transfer to spatial obect instead of data.faram object
# collect coordinates data 
coords <- cbind(Longitude = as.numeric(as.character(mancr1611$Longitude)), Latitude = as.numeric(as.character(mancr1611$Latitude)))
#creates a SpatialPointsDataFrame object
crime.pts <- SpatialPointsDataFrame(coords, mancr1611[,-(5:6)], proj4string = CRS("+init=epsg:4326"))
plot(crime.pts,pch='.',col='darkred')

#Create a  Layer of Lower Super Output areas (LSOA)
library(maptools)
library(rgdal)

unzip("BoundaryData.zip")
# check working directory files
list.files("/Users/Peahat/Desktop/Manchester Crimes")

#read in shape file
manch.lsoa <- readShapeSpatial("england_low_soa_2001")
plot(manch.lsoa)
#reprject the location infor of crime data, so that can display it with manch.lsoa data
crime.pts <- spTransform(crime.pts, CRS("+init=epsg:27700"))

#plot the shape file and SpatialPoints together 
plot(manch.lsoa,border='grey')
plot(crime.pts,pch='.',col='darkblue',add=TRUE)




#Focusing on One type of crimes, such as Antisocial Behaviour

getwd()
projectManchester <- "~/Desktop/Manchester Crimes/"
setwd(projectManchester)

#load library
library(sp)
library(maptools)
library(data.table)
library(dplyr)


mancr1611 <- read.csv("2016-11-greater-manchester-street.csv") 

#To displays dot on polygons file, the data need to transfer to spatial obect instead of data.faram object
# collect coordinates data 
coords <- cbind(Longitude = as.numeric(as.character(mancr1611$Longitude)), Latitude = as.numeric(as.character(mancr1611$Latitude)))
#creates a SpatialPointsDataFrame object
crime.pts <- SpatialPointsDataFrame(coords, mancr1611[,-(5:6)], proj4string = CRS("+init=epsg:4326"))
crime.pts <- spTransform(crime.pts, CRS("+init=epsg:27700"))


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
var <- as.vector(manch.lsoa@data[,"asb.count"])
breaks <- classIntervals(var, n = 6, style = "fisher")$brk
my_colours <- brewer.pal(6, "Greens")
plot(manch.lsoa, col = my_colours[findInterval(var, breaks, all.inside = TRUE)],   axes = FALSE, border = rgb(0.8,0.8,0.8))
