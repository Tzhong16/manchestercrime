#line graph of crimes in Manchester over this five years
getwd()
projectManchester <- "~/Desktop/Manchester Crimes/" # path to folder that holds multiple .csv files
setwd(projectManchester)

library(dplyr)
library(stringr)
library(data.table)
library(zoo)
mancrs <- fread("mergedatabymonth.csv")
mancrs %>%
  select(Month, Longitude, Latitude, Crime.type, LSOA.name) %>%   # ` ` used to select the column name which has space
  filter(str_detect(LSOA.name, 'Manchester')) %>%
  group_by(Crime.type, Month) %>%
  summarise(nCrime = n()) -> mancrsgroup

#Line graph-- Over all crimes in timeseries
mancrsgroup %>%
  group_by(Month) %>%
  summarise(nCrimeintotals = sum(nCrime)) -> crimestimeseries


#change the Month from character to date 
crimestimeseries$Month <- as.yearmon(crimestimeseries$Month)
crimestimeseries$Month <- as.Date(crimestimeseries$Month)

#use scales package to break the date from 4 month
library(ggplot2)
library(scales)
ggplot(crimestimeseries, aes(Month, nCrimeintotals)) + 
  geom_line(colour = "#00A4E6", size = 1.5) + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("4 month")) + 
  theme(axis.text.x = element_text(size=9)) + xlab(" ") + 
  ylab("Number of Total Crimes") +  ggtitle("Figure.1 Crimes of all kinds in Manchester") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.background = element_blank(),
        axis.text.y = element_text(colour="#666666", size = 10),
        axis.text.x = element_text(size = 10), axis.ticks.length = unit(.25, "cm"),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust=2.12, colour="#666666", size = 18),
        axis.title.y = element_text(size = 14, colour = "#666666")) -> c1




#umemployment rate and Overall Crimes line graphs by quarterly
library(dtplyr)
library(ggplot2)
library(grid)
uneply <- fread("unemploy_rate_ of_manchester.csv")
uneply$Month <- as.yearmon(uneply$Month, "%b-%y") #change to date type
uneply$Month <- as.Date(uneply$Month)

ggplot(uneply, aes(Month, umeply_rate)) + 
  geom_line(colour = "#FF6666", size = 1.5) + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("4 month")) + 
  theme(axis.text.x = element_text(size=9)) + xlab(" ") + 
  ylab("Unemployment (%)") +  ggtitle("Figure.2 Unemployment Rate in Manchester") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.background = element_blank(),
        axis.text.y = element_text(colour="#FF6666", size = 12),
        axis.text.x = element_text(size = 10), axis.ticks.length = unit(.25, "cm"),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust=2.12, colour="#666666", size = 18),
        axis.title.y = element_text(size = 14, colour = "#666666")) -> u1


ggplot(uneply, aes(Month, umeply_rate)) + 
  geom_line() + scale_x_date(labels = date_format("%b%y"), breaks = date_breaks("4 month")) + 
  theme(axis.text.x = element_text(size=9)) + xlab(" ") + 
  ylab("Unemployment Rate") +  
  ggtitle("Crimes and unemployment in Manchester by quarterly") + 
  theme(plot.title = element_text(hjust = 0.5)) -> plot1

ggplot(crimes_qualt_plt, aes(Year_qualt, nCrimequaterly)) +
  geom_line() + scale_x_date(labels = date_format("%b%y"), breaks = date_breaks("4 month")) + 
  theme(axis.text.x = element_text(size=9)) + xlab(" ") + 
  ylab("Number of Total Crimes") +  
  ggtitle("Crimes and unemployment in Manchester by quarterly") + 
  theme(plot.title = element_text(hjust = 0.5)) -> plot2
###################################################
#install.packages("gtable")
library(gtable)
ggplot(uneply, aes(Month, umeply_rate)) + 
  geom_line(colour = "#FF6666", size = 1.5) + scale_x_date(labels = date_format("%b%y"), breaks = date_breaks("4 month")) + 
  theme(axis.text.x = element_text(size=9)) + xlab(" ") + 
  ylab("Figure.2 unemployment in Manchester by quarterly") + 
  ggtitle("Figure.3 Crimes and unemployment in Manchester by quarterly") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank(), legend.position="top") + 
  theme(panel.background = element_blank()) +
  theme(axis.text.y = element_text(colour="#FF6666", size = 12),
        axis.text.x = element_text(size = 10)) +
  theme(axis.ticks.length = unit(.25, "cm"),
        axis.ticks.y = element_blank()) +
  ggtitle("Figure.3 Crimes and Unemployment in Manchester, Quarterly\n") + labs(x=NULL, y= NULL) +
  theme(plot.title = element_text(hjust = 0.5, vjust=2.12, colour="#666666", size = 18)) -> p1

ggplot(crimes_qualt_plt, aes(Year_qualt, nCrimequaterly)) +
  geom_line(colour = "#00A4E6", size = 1.5) +
  ggtitle("Total Crimes Number\n") +
  labs(x=NULL,y=NULL) + 
  scale_x_date(labels = date_format("%b%y"), breaks = date_breaks("4 month")) + 
  theme(axis.text.x = element_text(size=9)) + xlab(" ") +  
  theme(panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(colour="#00A4E6", size=12),
    axis.text.x = element_text(size = 10),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.6, vjust=2.12, colour = "#00a4e6", size = 14)) -> p2

# Combine two tables
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

pp <- c(subset(g1$layout, name == "panel", se = t:r))

g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]

ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")

g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

g$grobs[[8]]$children$GRID.text.1767$label <- c("Unemployment Rate\n", "Total Crimes Number\n")

# change color
g$grobs[[8]]$children$GRID.text.1767$gp$col <- c("#FF6666","#00A4E6")

# change x-coordinate
g$grobs[[8]]$children$GRID.text.1767$x <- unit(c(1.2, 2.12), "npc")
grid.draw(g)

#################


#transfer the Overall Crimes into quarlterly  
library(stringr)
library(tidyr)
crimes_quarterly <- fread("crimes_quarterly.csv")
crimes_quarterly_cls <- separate(crimes_quarterly, Month, c("Year", "Month"))
crimes_quarterly_cls %>%
  select(Year, Quarter, nCrimeintotals) %>%
  group_by(Year, Quarter) %>%
  summarise(nCrimequaterly = sum(nCrimeintotals)) -> crimes_qualt

#create a seq for quarter for each year 
Q <- c("Mar", "Jun", "Sep", "Nov" , "Mar", "Jun", "Sep", "Nov" , "Mar", "Jun", "Sep", "Nov" , "Mar", "Jun", "Sep", "Nov" , "Mar", "Jun", "Sep", "Nov" , "Mar", "Jun", "Sep", "Nov")
crimes_qualt$Qua <- Q
crimes_qualt_plt <- unite(crimes_qualt, Year_qualt, Year, Qua, sep = "-")

crimes_qualt_plt$Year_qualt <- as.yearmon(crimes_qualt_plt$Year_qualt, "%Y-%b")
crimes_qualt_plt$Year_qualt <- as.Date(crimes_qualt_plt$Year_qualt)

library(ggplot2)
ggplot(crimes_qualt_plt, aes(Year_qualt, nCrimequaterly)) +
  geom_line() + scale_x_date(labels = date_format("%b%y"), breaks = date_breaks("4 month")) + 
  theme(axis.text.x = element_text(size=9)) + xlab(" ") + 
  ylab("Number of Total Crimes") +  
  ggtitle("Crimes and unemployment in Manchester by quarterly") + 
  theme(plot.title = element_text(hjust = 0.5))

#Anti-social behaviour timeseries --Line graph
#filter data first 
mancrsgroup %>%
  filter(str_detect(Crime.type, 'Anti')) %>%
  group_by(Month) -> ASBtimeseries

#change the Month from character to date 
ASBtimeseries$Month <- as.yearmon(ASBtimeseries$Month)
ASBtimeseries$Month <- as.Date(ASBtimeseries$Month)

#use scales package to break the date from 4 month
library(ggplot2)
library(scales)
ggplot(ASBtimeseries, aes(Month, nCrime)) + geom_line() + scale_x_date(labels = date_format("%Y-%b"), breaks = date_breaks("4 month")) + theme(axis.text.x = element_text(size=9)) + xlab("Over 6 years") + ylab("Number of ASB") +  ggtitle("Timeseries of Anti-social behaviour in Manchester") + theme(plot.title = element_text(hjust = 0.5))


#xlab("") + ylab("Number of ASB") +  ggtitle("Timeseries of Anti-social behaviour in Manchester")
months_lables=list.files(path="~/Desktop/Manchester Crimes/original_files",pattern="*.csv",all.files=TRUE) 
months_lables<-substr(months_lables, 1, 7)



#
#    ggplot(data = dm, aes(Date, Visits)) + 
#geom_line() + 
#  scale_x_date(format = "%b %d", major =  "1 day")
#
#


#October Crime 
Gmancr1610 <- fread("2016-10-greater-manchester-street.csv")
library(dplyr)
library(stringr)
glimpse(Gmancr1610)

Gmancr1610 %>%
  select(Month, Longitude, Latitude, `Crime type`, `LSOA name`) %>%   # ` ` used to select the column name which has space
  rename( Crimetype =`Crime type`, LSOA.name = `LSOA name`) %>%  
  filter(str_detect(LSOA.name, 'Manchester')) %>%
  group_by(Crimetype, Month) %>%
  summarise(nCrime = n()) -> crime1610

sum(crime1610$nCrime)


