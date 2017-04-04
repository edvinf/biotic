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
# Make one big flat table.
# Note that records will be repeated. If there are n individuals sampled from one catchsample. The data for catchsample will be repeated at least n times (more if some individuals have for instance several age-readings)
#
table <- flatten(bioticdata)

#
# Use datatypes from xml. Set location of test_schema in file biotic.R
#
biotic_w_dtypes <- parse_biotic(destfile, schema=test_schema, set_data_types = T)
table_w_dtypes <- flatten(biotic_w_dtypes)


#
# To control better where repition might occour, use merge with the dataframes on bioticdata or biotic_w_dtypes
# In the following example, for instance, you will know that no catchsamples are repated, and that any fishstation were no catchsample was recorded is excluded.
#
catches <- merge(biotic_w_dtypes$Catchsample, biotic_w_dtypes$Fishstation)

#
# since column names are rather long, it might be useful to know that standard data frames allow shorthand notations.
# That is, you only have to specify colun names up to the piint when they are unique
# First convert the data to standard data frame
std_df <- as.data.frame(table_w_dtypes)
# then compare for instance
print(all(std_df$year == std_df$year.Mission))
# if abbreviation is not unique, the column vectir us null
print(std_df$weight)
#while these are resolved
print(length(std_df$weight.Individual))
print(length(std_df$weight.Catchsample))
print(length(std_df$weight.C))