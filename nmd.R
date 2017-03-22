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
load_biotic_by_serial_year <- function(serial_lower, serial_upper, year, dir, filename=NULL, overwrite=Fr){
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

#' Parses to a flat table all data for a given list of species, for range of serial numbers and a range of years.
#' @param serial_lower first serial number to include
#' @param serial_upper last serial number to include
#' @param year_lower first year to include
#' @param year_upper last year to include
#' @param specieslist list of tns codes to include
#' @param handlers passed to parse_biotic
#' @param chunks Denotes how many download to split the serial number range in. Total downloads: chunks*(year_upper-year_lower+1)
#' @return Tibble with all data as flat file.
filter_by_species <- function(serial_lower, serial_upper, year_lower, year_upper, specieslist, handlers=biotic_1_4_handlers, chunks = 20){
  if (year_lower > year_upper){
    stop("Illegal range of years")
  }
  source("/Users/a5362/code/github/hi_biotic_parser/biotic.R")
  tmpdir <- "/Users/a5362/t"
  tmp_file <- "tmp.xml"
  
  flat <- NULL
  stepsize = ceiling((serial_upper-serial_lower)/chunks)
  for (y in year_lower:year_upper){
    for (s in seq(serial_lower, serial_upper, stepsize)){
      xml <- load_biotic_by_serial_year(s, min(s+stepsize, serial_upper), y, tmpdir, tmp_file, T)
      if (!is.null(xml)){
        bioticdata <- parse_biotic(xml, handlers=handlers)
        cflat <- flatten(bioticdata)
        cflat <- cflat[cflat$species %in% specieslist,]
        if (is.null(flat)){
          flat <- cflat        
        }
        else{
          flat <- bind_rows(flat, cflat)        
        }
      }
      
    }
  }
  return(flat)
}


example_blb <- function(){
  specieslist <- c("164740", "164760", "164760") #brosme, lange, blålange
  ss <- 0
  se <- 99999
  ys <- 1970
  ye <- 2016
  path <- "/Users/a5362/t"
  data <- filter_by_species(ss,se,ys,ye, specieslist, handlers=biotic_1_4_handlers[c("mission", "fishstation", "catchsample", "individual", "agedetermination")])
  if (nrow(data)==0){
    stop("No matches found")
  }
  filename_xls <- paste(path, paste(paste(specieslist, collapse="_"), ".xlsx", sep=""), sep="/")
  filename_csv <- paste(path, paste(paste(specieslist, collapse="_"), ".csv", sep=""), sep="/")
  write.csv(data, filename_csv)
  library(xlsx)
  write.xlsx2(data, filename_xls)
  return(data)
}



test <- function(){
  file <- load_biotic_by_serial_year(00000, 10000, 1990, "/Users/a5362/t", T)
  print(file)
  file <- load_biotic_by_serial_year(00000, 10000, 1881, "/Users/a5362/t", T)
  print(file)
}



