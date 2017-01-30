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



