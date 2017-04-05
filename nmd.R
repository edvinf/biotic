library(downloader)
nmd_base_url <- "http://tomcat7.imr.no:8080/apis/nmdapi"
#' Download mission records within a year and serial number range
#' @param serial_lower lower serial number in range (inclusive)
#' @param serial_upper upper serial number in range (inclusive)
#' @param year year
#' @param dir target directory for downloads
#' @param filename target filename, one will be generated if NULL.
#' @param overwrite logical: if TRUE existing files in dir will be overwritte
#' @return filename or NULL of something went wrong
load_biotic_by_serial_year <- function(serial_lower, serial_upper, year, dir, filename=NULL, overwrite=F){
  version <- "v1"
  
  if (!is.null(filename)){
    destfile <- paste(dir, filename, sep="/")
  }
  else{
    destfile <- paste(dir, paste("biotic", version, serial_lower, serial_upper, year, ".xml", sep="_") ,sep="/")    
  }
  
  if (file.exists(destfile) & !overwrite){
    stop(paste("File", destfile, "already exists."))
  }
  
  url <- paste(nmd_base_url, "biotic", version, year, serial_lower, serial_upper, "serial", sep="/")
  ret <- download.file(url, destfile=destfile, mode="wb", method="wget")
  if (ret!=0){
    return(NULL)
  }
  else{
    return(destfile)
  }
}

#' Downloads all biotic data for a given year from NMD and saves as excel file
#' @param excelfile name of target excelfile. Will be overwritten if already existing.
#' @param year 
#' @param tempdir location for storing temp-data (existing files will be overwritten, but will not be automatically deleted after completion)
save_year_to_excel <- function(excelfile, year, tempdir){
  require(openxlsx)
  source("biotic.R")
  maxs=99999
  bio_xml <- load_biotic_by_serial_year(0, maxs, year, tempdir, overwrite=T)
  if (is.null(bio_xml)){
    stop("Data could not be downloaded.")
  }
  tab <- flatten(parse_biotic(bio_xml))  
  write.xlsx(tab, excelfile, overwrite=T)
}

test <- function(){
  file <- load_biotic_by_serial_year(00000, 10000, 1990, "/Users/a5362/t", T)
  print(file)
  file <- load_biotic_by_serial_year(00000, 10000, 1881, "/Users/a5362/t", T)
  print(file)
}

test_save_year_to_excel <- function(){
  system.time(save_year_to_excel("test_ste.xlsx", 2015, "."))
}

