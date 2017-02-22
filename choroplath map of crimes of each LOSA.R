getwd()
projectManchester <- "~/Desktop/Manchester Crimes/"
setwd(projectManchester)

library(sp)
library(maptools)
library(rgdal)
library(stringr)
library(dplyr)

mancr <- read.csv("2016-11-greater-manchester-street.csv") 
crimes <- filter(mancr, str_detect(LSOA.name, 'Manchester'))  # alternative method: grep() 

filter(crimes, str_detect(Crime.type, 'Burglary')) -> crimes  
coords <- cbind(Longitude = as.numeric(as.character(crimes$Longitude)), Latitude = as.numeric(as.character(crimes$Latitude)))
crime.pts <- SpatialPointsDataFrame(coords, crimes[,-(5:6)], proj4string = CRS("+init=epsg:4326"))
plot(crime.pts,pch='.',col='darkred')

#unzip BoundaryData.zip first
#read shapefile
manch.lsoa <- readShapeSpatial("england_low_soa_2001")
#reproject the crime.pts data so that it can be display with Manchester LSOA data
crime.pts <- spTransform(crime.pts, CRS("+init=epsg:27700"))
# display all crimes in mancherster
plot(manch.lsoa)
plot(crime.pts,pch='.',col='red',add=TRUE)


# focus on antisocial behaviour
asb.pts <- crime.pts[crime.pts$Crime.type == "Anti-social behaviour", ]
#bur.pts <- crime.pts[crime.pts$Crime.type == "Burglary", ]

#rgeos packages use for overlay operations in R
library(rgeos)

# This defines a new R function - it counts how many points fall into each polygon
poly.counts <- function (pts, polys) colSums(gContains(polys, pts, byid = TRUE))

#counts the number of crimes in each LSOA
#bur.count <- poly.counts(crime.pts ,manch.lsoa)
asb.count <- poly.counts(asb.pts, manch.lsoa)
#crime.count <- poly.counts(crime.pts,manch.lsoa)

# draw choropleth map of asb counts for each LSOA
library(classInt)
library(RColorBrewer)
# First, add an ASB event count column to the 'manch.lsoa' SpatialPolygonsDataFrame
manch.lsoa@data$asb.count <- asb.count
# manch.lsoa@data$crime.count <- crime.count

var <- manch.lsoa@data[,"asb.count"]
breaks <- classIntervals(var, n = 6, style = "fisher")$brk
my_colours <- brewer.pal(6, "Greens")
plot(manch.lsoa, col = my_colours[findInterval(var, breaks, all.inside = TRUE)],   axes = FALSE, border = rgb(0.8,0.8,0.8))
breaks <- list(b=breaks,c=my_colours)
#title(main="Figure.9 Crime Occurrence in Manchester, 11-2016", col.main="grey50", font.main=3,cex.main=2)
#title(main="Figure.9 Crime Occurrence in Manchester By LSOA", col.main="grey50", font.main=4,cex.main=1.5)


##############################################
##############################################
# Rate of Anti-Social behaviour(ASBs) Crime Occurrence
##############################################
##############################################
pop <- read.csv("pop2.csv")

#match the location in pop that eacht LSOA code in manche.lsoa
#see code above for creating manch.lsoa file 
lsoa.lookup <- match(manch.lsoa$zonecode,pop$lsoa)
head(lsoa.lookup)
head(manch.lsoa@data)

#add a column with the appropriate population estimates to the manchester (SpatialPolygonsDataFrame)
manch.lsoa@data$pop <- pop[lsoa.lookup,"pop"]
head(manch.lsoa@data)

#add column contains the rate of ASBs per head of population
#Note the multiplication by 10000 - this means rates are per 10000 heads of population.
manch.lsoa@data$asb.rate <- 10000 * (manch.lsoa@data$asb.count / manch.lsoa@data$pop)
head(manch.lsoa@data) 

var <- manch.lsoa@data[,"asb.rate"]
breaks <- classIntervals(var, n = 6, style = "fisher")$brk
my_colours <- brewer.pal(6, "Greens")
par(xpd=T, mar=par()$mar+c(0,0,0,3))
plot(manch.lsoa, col = my_colours[findInterval(var, breaks, all.inside = TRUE)],   axes = FALSE, border = rgb(0.8,0.8,0.8))
breaks <- list(b=breaks,c=my_colours)
#cex is the whole scale of legend
legend("bottomright", legend = leglabs(round(breaks$b,1)), fill = breaks$c, cex = 0.6, bty = "n", title="Times per 10,000 people", title.adj=0.3)
title(main="Figure.9 Crime Occurrence in Manchester By LSOA", col.main="grey50", font.main=4,cex.main=1.5)

#############################################
#############################################
#Indicators of Deprivatior 
#############################################
#############################################

deprivation <- read.csv("2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv")

library(dplyr)
glimpse(deprivation)
names(deprivation)

deprivation <- deprivation[, c(1,5,8,11,17,14,23,20,26)]
colnames(deprivation) <- c("LSOA","Overall","Income","Employment","Health","Education","Housing","Crime","Environment")
head(deprivation)
lsoa.lookup <- match(manch.lsoa$zonecode,deprivation$LSOA)
manch.lsoa@data <- cbind(manch.lsoa@data,deprivation[lsoa.lookup,-1])
head(manch.lsoa@data)

#draw a choropleth map 

var <- manch.lsoa@data[,"Overall"]
breaks <- classIntervals(var, n = 6, style = "fisher")$brk
my_colours <- brewer.pal(6, "Greens")
par(xpd=T, mar=par()$mar+c(0,0,0,3))
plot(manch.lsoa, col = my_colours[findInterval(var, breaks, all.inside = TRUE)],   axes = FALSE, border = rgb(0.8,0.8,0.8))
breaks <- list(b=breaks,c=my_colours)
legend("bottomright", legend = leglabs(round(breaks$b,1)), fill = breaks$c, cex = 0.6, title="Index of Deprivation", title.adj=0.3, bty ="n")
title(main="Figure.10 Overall Deprivation in Mancheser (By LSOA)", col.main="grey50", font.main=4,cex.main=1.5)


#Find the possible relationship between Anti-social behavior incidents and deprivation
plot(manch.lsoa$Overall,manch.lsoa$asb.rate,xlab="Index of Overall Deprivation",ylab="ASB Rate (per 10,000 people)")
title("Anti-Social Behaviour and Deprivation")
abline(lm(manch.lsoa$asb.rate~manch.lsoa$Overall),col='darkblue',lwd=2)
cor(manch.lsoa@data$asb.rate, manch.lsoa@data$Overall, use = "complete.obs")
cor(manch.lsoa@data$asb.rate, manch.lsoa@data[,12:18], use = "complete.obs")
