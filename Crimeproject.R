getwd()
projectManchester <- "~/Desktop/Manchester Crimes/"
setwd(projectManchester)

library(data.table)
library(ggplot2)
mancr1611 <- fread("2016-11-greater-manchester-street.csv")
head(mancr1611)
names(mancr1611)
str(mancr1611)
#library(dtplyr)
library(dplyr)
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

# g <- ggplot(mancr1611, aes(`Crime type`)) # sums the crime type
# g + geom_bar()

#plot bar
# hjust scale 0-1 to set the theme location
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
 




