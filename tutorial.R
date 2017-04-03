#
# Data are accessed through NMD API. Common questions are answered at : https://confluence.imr.no/pages/viewpage.action?pageId=1048637
# You can explore the database at: http://tomcat7.imr.no:8080/DatasetExplorer/v1/html/main.html
# (you can download files from here as well)

# to locate data by cruise
# Identify the file through data set explorer (http://tomcat7.imr.no:8080/DatasetExplorer/v1/html/main.html)
# Make note of vessel and subfolder
# use http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v1/Forskningsfart%C3%B8y/<year>/<vessel>/<subfolder>
#
# e.g.: http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v1/Forskningsfart%C3%B8y/2016/G O Sars-LMEL/2016101
# download.file("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v1/Forskningsfart%C3%B8y/2016/G%20O%20Sars-LMEL/2016101", "temp.xml")

# to locate data by serial number.
# find the appropriate serial number range in:  https://docs.google.com/spreadsheets/d/1qel6xfvuTiYIdJmBEeAhCDeyQaEAgQVi2jeGJAzYivE
# form url to download data: http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v1/<year>/<first_serial_no>/<last_serial_no>/serial
# example: part of reference fleet 2015
url <- "http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v1/2015/86000/87000/serial"
destfile <- "refl_2015_86000_87000.xml"
ret <- download.file(url, destfile=destfile, mode="wb", method="wget")

#load xml file to flat tables
source("biotic.R")
bioticdata <- parse_biotic(destfile)

#
# make a table linking individuals to positions
#

# select which tables and columns to keep, make sure to keep all key columns (see keys_biotic1_4 in biotic.R)
#
# For now, I'll make a better way to merge data.
#
keep <- list()
keep$Mission <- bioticdata$Mission[,c("missiontype", "missionnumber", "year", "platform")]
keep$Fishstation <- bioticdata$Fishstation[,c("Mission.missiontype", "Mission.missionnumber", "Mission.year", "Mission.platform", "serialno", "system", "area", "location")]
keep$Catchsample <- bioticdata$Catchsample[,c("Mission.missiontype", "Mission.missionnumber", "Mission.year", "Mission.platform", "Fishstation.serialno", "species", "samplenumber", "aphia", "noname")]
keep$Individual <- bioticdata$Individual[,c("Mission.missiontype", "Mission.missionnumber", "Mission.year", "Mission.platform", "Fishstation.serialno", "Catchsample.species", "Catchsample.samplenumber", "specimenno", "weight", "length", "sex")]
keep$Agedetermination <- bioticdata$Agedetermination[,c("Mission.missiontype", "Mission.missionnumber", "Mission.year", "Mission.platform", "Fishstation.serialno", "Catchsample.species", "Catchsample.samplenumber", "Individual.specimenno", "age", "spawningage")]
table <- flatten(keep)