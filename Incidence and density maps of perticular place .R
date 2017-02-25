getwd()
projectManchester <- "~/Desktop/Manchester Crimes/"
setwd(projectManchester)

library(sp)
library(maptools)
library(rgdal)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggmap)

mancr <- read.csv("2016-11-greater-manchester-street.csv") 
crimes <- filter(mancr, str_detect(LSOA.name, 'Manchester'))  # alternative method: grep() 
#filter targerts of crimes type
targets <- c('Anti-social behaviour', 'Violence and sexual offences', 'Burglary', 'Other theft')
crimes <- filter(crimes, Crime.type %in% targets)

#rank different crimes
crimes$Crime.type <- factor(crimes$Crime.type, levels = c("Burglary", "Other theft", "Violence and sexual offences", "Anti-social behaviour"))

# restrict to downtown
# longtitude & latitude get from google earth
crimes <- subset(crimes, ((-2.270859 <= Longitude)
                           & (Longitude <= -2.213429)
                           & (53.463590 <= Latitude)
                           & (Latitude <= 53.498749)))
          
map <- get_map( location = 'Manchester', zoom = 14, maptype = "roadmap", color = "bw")

#use ggmap to plot the map
p <- ggmap(map)
p <- p + geom_point(data = crimes, aes(x = Longitude, y = Latitude, size = Crime.type, colour = Crime.type))

# legend positioning, removing grid and axis labeling
p <- p + theme( legend.position = c(0.0, 0.7) # put the legend inside the plot area
                , legend.justification = c(0, 0)
                , legend.background = element_rect(colour = F, fill = "white")
                , legend.key = element_rect(fill = F, colour = F)
                , panel.grid.major = element_blank()
                , panel.grid.minor = element_blank()
                , axis.text = element_blank()
                , axis.title = element_blank()
                , axis.ticks = element_blank())

print(p)
                       
# 2D density plot
p <- ggmap(map)
overlay <- stat_density2d(data = crimes
                          , aes(x = Longitude, y = Latitude, fill = ..level.. , alpha = ..level..)
                          , size = 2, bins = 4, geom = "polygon")
p <- p + overlay
p <- p + scale_fill_gradient("Violent\nCrime\nDensity")
p <- p + scale_alpha(range = c(0.4, 0.75), guide = FALSE)
p <- p + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
#p <- p + inset(grob = ggplotGrob(ggplot() + overlay + theme_inset())
# , xmin = -95.35836, xmax = Inf, ymin = -Inf, ymax = 29.75062) print(p)                           
print(p)
