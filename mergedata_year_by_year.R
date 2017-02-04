#Merging multiple .csv files into the same data frame
# Method One
getwd()
crimefiles <- "~/Desktop/Manchester Crimes/original_files/" # path to folder that holds multiple .csv files
setwd(crimefiles)

file_list <- list.files(path=crimefiles, pattern="*.csv") # create list of all .csv files in folder
library(dplyr)
# read in each .csv file in file_list and rbind them into a data frame called data 
data <- do.call("bind_rows", lapply(file_list, function(x) read.csv(paste(crimefiles, x, sep=''), stringsAsFactors = FALSE)))
write.csv(data, file = "mergedata.csv", row.names = T )


#Method Two

getwd()
crimefiles <- "~/Desktop/Manchester Crimes/original_files/" # path to folder that holds multiple .csv files
setwd(crimefiles)

file_list <- list.files(path=crimefiles, pattern="*.csv") # create list of all .csv files in folder
library(dplyr)
# read in each .csv file in file_list and rbind them into a data frame called data 
data <- do.call("bind_rows", lapply(file_list, function(x) fread(paste(crimefiles, x, sep=''))))
write.csv(data, file = "mergedata.csv", row.names = T )




#Method Three
# Inputting crimes' data of Great Manchester from last five years
Gmancr1101 <- fread("2011-01-greater-manchester-street.csv")
Gmancr1102 <- fread("2011-02-greater-manchester-street.csv")
Gmancr1103 <- fread("2011-03-greater-manchester-street.csv")
Gmancr1104 <- fread("2011-04-greater-manchester-street.csv")
Gmancr1105 <- fread("2011-05-greater-manchester-street.csv")
Gmancr1106 <- fread("2011-06-greater-manchester-street.csv")
Gmancr1107 <- fread("2011-07-greater-manchester-street.csv")
Gmancr1108 <- fread("2011-08-greater-manchester-street.csv")
Gmancr1109 <- fread("2011-09-greater-manchester-street.csv")
Gmancr1110 <- fread("2011-10-greater-manchester-street.csv")
Gmancr1111 <- fread("2011-11-greater-manchester-street.csv")
Gmancr1112 <- fread("2011-12-greater-manchester-street.csv")
Gmancr1201 <- fread("2012-01-greater-manchester-street.csv")
Gmancr1202 <- fread("2012-02-greater-manchester-street.csv")
Gmancr1203 <- fread("2012-03-greater-manchester-street.csv")
Gmancr1204 <- fread("2012-04-greater-manchester-street.csv")
Gmancr1205 <- fread("2012-05-greater-manchester-street.csv")
Gmancr1206 <- fread("2012-06-greater-manchester-street.csv")
Gmancr1207 <- fread("2012-07-greater-manchester-street.csv")
Gmancr1208 <- fread("2012-08-greater-manchester-street.csv")
Gmancr1209 <- fread("2012-09-greater-manchester-street.csv")
Gmancr1210 <- fread("2012-10-greater-manchester-street.csv")
Gmancr1211 <- fread("2012-11-greater-manchester-street.csv")
Gmancr1212 <- fread("2012-12-greater-manchester-street.csv")
Gmancr1301 <- fread("2013-01-greater-manchester-street.csv")
Gmancr1302 <- fread("2013-02-greater-manchester-street.csv")
Gmancr1303 <- fread("2013-03-greater-manchester-street.csv")
Gmancr1304 <- fread("2013-04-greater-manchester-street.csv")
Gmancr1305 <- fread("2013-05-greater-manchester-street.csv")
Gmancr1306 <- fread("2013-06-greater-manchester-street.csv")
Gmancr1307 <- fread("2013-07-greater-manchester-street.csv")
Gmancr1308 <- fread("2013-08-greater-manchester-street.csv")
Gmancr1309 <- fread("2013-09-greater-manchester-street.csv")
Gmancr1310 <- fread("2013-10-greater-manchester-street.csv")
Gmancr1311 <- fread("2013-11-greater-manchester-street.csv")
Gmancr1312 <- fread("2013-12-greater-manchester-street.csv")
Gmancr1401 <- fread("2014-01-greater-manchester-street.csv")
Gmancr1402 <- fread("2014-02-greater-manchester-street.csv")
Gmancr1403 <- fread("2014-03-greater-manchester-street.csv")
Gmancr1404 <- fread("2014-04-greater-manchester-street.csv")
Gmancr1405 <- fread("2014-05-greater-manchester-street.csv")
Gmancr1406 <- fread("2014-06-greater-manchester-street.csv")
Gmancr1407 <- fread("2014-07-greater-manchester-street.csv")
Gmancr1408 <- fread("2014-08-greater-manchester-street.csv")
Gmancr1409 <- fread("2014-09-greater-manchester-street.csv")
Gmancr1410 <- fread("2014-10-greater-manchester-street.csv")
Gmancr1411 <- fread("2014-11-greater-manchester-street.csv")
Gmancr1412 <- fread("2014-12-greater-manchester-street.csv")
Gmancr1501 <- fread("2015-01-greater-manchester-street.csv")
Gmancr1502 <- fread("2015-02-greater-manchester-street.csv")
Gmancr1503 <- fread("2015-03-greater-manchester-street.csv")
Gmancr1504 <- fread("2015-04-greater-manchester-street.csv")
Gmancr1505 <- fread("2015-05-greater-manchester-street.csv")
Gmancr1506 <- fread("2015-06-greater-manchester-street.csv")
Gmancr1507 <- fread("2015-07-greater-manchester-street.csv")
Gmancr1508 <- fread("2015-08-greater-manchester-street.csv")
Gmancr1509 <- fread("2015-09-greater-manchester-street.csv")
Gmancr1510 <- fread("2015-10-greater-manchester-street.csv")
Gmancr1511 <- fread("2015-11-greater-manchester-street.csv")
Gmancr1512 <- fread("2015-12-greater-manchester-street.csv")


n <- bind_rows(Gmancr1610, Gmancr1611)


