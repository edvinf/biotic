tempspace <- tempdir()
source("biotic.R")

fetch <- function(year, start, send){
  url <- paste("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v1", year, start, send, "serial", sep="/")
  print(url)
  destfile <- paste(year, "_", start, "_", send, ".xml", sep="")
  destfile <- paste(tempspace, destfile, sep="/")
  print(destfile)
  ret <- download.file(url, destfile=destfile, mode="wb", method="wget")
  return(destfile)
}

check <- function(file){
  bioticdata <- parse_biotic(file, handlers=biotic_1_4_handlers[c("mission", "fishstation")])
  return(unique(bioticdata$fishstation[duplicated(bioticdata$fishstation$serialno.fishstation),]$serialno.fishstation))
}

start <- 0
end <- 99999
#start <- 27000
#end <- 36999
year <- 2016

dupl <- check(fetch(year, start, end))
print(paste("Tempfiles in: ", tempspace))
if (length(dupl)>0){
  print(paste(length(dupl), "duplicated serial numbers"))  
}
for (s in dupl){
  print(s)
}