require(XML)
require(dplyr)

test_schema <- "/Users/a5362/code/hi_formats/commons-biotic-jaxb/src/main/resources/bioticv1_4.xsd"
test_data <- "/Users/a5362/hi_home/stox/tests/Capelin BS 2012/input/biotic/gs_2012.xml"
test_data <- "/Users/a5362/Desktop/4-2015-4174-14.xml"
test_refl_2015 <- "/Users/a5362/hi_home/bifangst/bruskfisk/referansefl_test/refl_hav_2015.xml"

# Parser for mapping biotic xml format to relational table structure represented as dplyr Tibbles.
# Each data frame / Tibble is represented by a complex type in the XML, with a corresponding name (except for the suffix "Type" in XML).
# All variables (columns) have corresponding elements or attributes in these complex types and inherit their documentation from there.
# In addition foreing keys are introduced to link tables. They are named "targettable.varaible"
#
# Parsing is not schema based. Therefore:
# - Variales that are not registered in the XML (but are allowed in biotic) will not be represented by any column in the parsed data frames
# - If path to schema is not given, all data types will be char. 
#
# Notes for adapting to future versions of biotic:
# Parser relies on keys being identified for each complex type that has non-simple children. See the list keys_biotic1_4 for an example
# Parser relies on element names being unique for the elements that map to data frames. 
# This needs to be checked for updates as xml allows element names to be reused in different types
# It is OK if an attribute of some element has the same name as another element. In fact this is the case in biotic 1.4 for copepodedevstage
#

# Mission is uniquely identified by its keys.
# All keys are attributes in the xml.
# All types occuring below Fishstation will also have as keys the foreing keys of all parent elements
# c()-> No keys defined (need not be referenced in other types)
keys_biotic1_4 <- list(MissionsType=c(), 
                       MissionType=c("missiontype", "missionnumber", "year", "platform"),
                       FishstationType=c("serialno"), 
                       CatchsampleType=c("species", "samplenumber"),
                       IndividualType=c("specimenno"),
                       AgedeterminationType=c(),
                       TagType=c(),
                       PreyType=c(),
                       PreylengthType=c(),
                       CopepodedevstageType=c()
                       )

hardcoded_schematype_function <- function(node){
  schematypes <- list(missions="MissionsType",
        mission="MissionType",
        fishstation="FishstationType",
        catchsample="CatchsampleType",
        individual="IndividualType",
        agedetermination="AgedeterminationType",
        tag="TagType",
        prey="PreyType",
        preylength="PreylengthType",
        copepodedevstage="CopepodedevstageType"
  )
  return(schematypes[[xmlName(node)]])
}

dm <- list(StringDescriptionType="as.character", "xs:integer"="as.integer", "xs:string"="as.character", "xs:decimal"="as.double", "key"="as.character")
#'Set data types for bioticdata. 
#'Relies on the assumption that each tibble is named as the complexType in the xsd, excluding the suffix "Type"
#'Assumes fixed namespace prefix ns for http://www.w3.org/2001/XMLSchema
#'@param bioticdata list of tibbles as returned by parse_biotic
#'@param schema XMLInternalDocument representing the xsd
#'@param datatype_mapping list mapping data types in schema to names of functions used to convert to R data types
#'@param keys list mapping schematypes to keys
set_data_types <- function(bioticdata, schema, keys, datatype_mapping=dm){
  get_data_types <- function(framename){
    elements <- getNodeSet(schema, paste("/xs:schema/xs:complexType[@name='",framename,"Type']//xs:element", sep=""), c(xs="http://www.w3.org/2001/XMLSchema"))
    attribs <- getNodeSet(schema, paste("/xs:schema/xs:complexType[@name='",framename,"Type']//xs:attribute", sep=""), c(xs="http://www.w3.org/2001/XMLSchema"))
    fieldnames <- lapply(elements, xmlGetAttr, "name")
    fieldnames <- append(fieldnames, lapply(attribs, xmlGetAttr, "name"))
    fieldtypes <- lapply(elements, xmlGetAttr, "type")
    fieldtypes <- append(fieldtypes, lapply(attribs, xmlGetAttr, "type"))
    names(fieldtypes) <- fieldnames
    return(fieldtypes)
  }
  
  #convert data types
  for (fr in names(bioticdata)){
    dtypes <- get_data_types(fr)
    frame <- bioticdata[[fr]]
    for (coln in names(frame)){
      t<-dtypes[[coln]]
      if (is.null(t)){
        #deal with type of keys
        t <- "key"
      }
      ff <- datatype_mapping[[t]]
      
      if (is.null(ff)){
        stop(paste("Data type", t, "not mapped"))
      }
      frame[[coln]] <- eval(call(ff, frame[[coln]]))
    }
    bioticdata[[fr]] <- frame
  }
  
  #Fix key types
  add_key_types <- function(data, typename, keys, elementname){
    types <- get_data_types(typename)
    for (fr in names(data)){
      frame <- data[[fr]]
      for (k in keys){
        keycol <- paste(elementname, k, sep=".")
        if (keycol %in% names(frame)){
          t<-types[[k]]
          if (is.null(t)){
            stop()
          }
          ff<-datatype_mapping[[t]]
          if (is.null(ff)){
            stop()
          }
          frame[[keycol]] <- eval(call(ff, frame[[keycol]]))
        }
      }
      data[[fr]] <- frame
    }
    return(data)
  }
  bioticdata <- add_key_types(bioticdata, "Mission", keys$MissionType, "mission")
  bioticdata <- add_key_types(bioticdata, "Fishstation", keys$FishstationType, "fishstation")
  bioticdata <- add_key_types(bioticdata, "Catchsample", keys$CatchsampleType, "catchsample")
  bioticdata <- add_key_types(bioticdata, "Individual", keys$IndividualType, "individual")
  return(bioticdata)
}

#' Makes a list mapping foreing key names to their values, for the arument nodes and for all parent nodes up til root or <missions/>
#' @param node node The foreign key will reference
#' @param keys list mapping schematypes to keys
#' @param schematype_function function mapping node names to their schematype
#' @return list with names <node name>.<key attribute> and values the value of the <key attribute> for this node
make_foreign_keys <- function(node, keys, schematype_function){
  if (is.null(node)){
    return(list())
  }
  schematype <- schematype_function(node)
  if (is.null(schematype)){
    return(list())
  }
  foreign_keys <- list()
  
  if (schematype!="MissionType"){
    foreign_keys <- append(make_foreign_keys(xmlParent(node), keys, schematype_function), foreign_keys)
  }
  for (key in keys[[schematype]]){
    foreign_keys[paste(xmlName(node),key,sep=".")] <- xmlGetAttr(node, key)
  }
  return(foreign_keys)
}


bioticdata <- list()
#creates handler for parsing specific xml elements
make_data_frame_parser <- function(framename, foreign_key_generator, drop=c(), verbose=T){
  parser <- function(node){
    nlist <- xmlApply(node, xmlValue)
    nlist[which(names(nlist) %in% drop)]<-NULL
    nlist <- append(nlist, foreign_key_generator(xmlParent(node)))
    nlist <- append(nlist, as.list(xmlAttrs(node)))
    bioticdata[[framename]] <<- bind_rows(bioticdata[[framename]], nlist)
    return(NULL)
  }
  verbose_parser <- function(node){
    print(paste("Loading:", xmlName(node), ": ", xmlAttrs(node)))
    return(parser(node))
  }
  if (verbose){
    return(verbose_parser)
  }
  else{
    return(parser)
  }
}

foreing_key_generator_1_4 <- function(node){return(make_foreign_keys(node, keys_biotic1_4, hardcoded_schematype_function))}
biotic_1_4_handlers <- list(
        #<text/> is added to drop, because xmlInternalTreeParse(trim=T) does not handle \n
        #missions=make_data_frame_parser("Missions", foreing_key_generator_1_4, c("mission", "text")),
        mission=make_data_frame_parser("Mission", foreing_key_generator_1_4, c("fishstation", "text")),
        fishstation=make_data_frame_parser("Fishstation", foreing_key_generator_1_4, c("catchsample", "text")), 
        catchsample=make_data_frame_parser("Catchsample", foreing_key_generator_1_4, c("prey", "individual", "text")),
        individual=make_data_frame_parser("Individual", foreing_key_generator_1_4, c("agedetermination", "tag", "text")),
        prey=make_data_frame_parser("Prey", foreing_key_generator_1_4, c("preylength", "copepodedevstage", "text")),
        tag=make_data_frame_parser("Tag", foreing_key_generator_1_4, c("text")),
        agedetermination=make_data_frame_parser("AgeDetermination", foreing_key_generator_1_4, c("text")),
        preylength=make_data_frame_parser("Preylength", foreing_key_generator_1_4, c("text")),
        copepodedevstage=make_data_frame_parser("Copepodedevstage", foreing_key_generator_1_4, c("text"))
        )

#' Sets all blank entries in data frames to NA.
#' @param dataframes named list of Tibbles
#' @return named list of tibbles where all occurances of "" is set to NA.
set_blanks_to_NA <- function(dataframes){
  d <- dataframes
  for (n in names(d)){
    frame <- d[[n]]
    for (cn in names(frame)){
      frame[!is.na(frame[,cn]) & frame[,cn]=="",] <-NA
    }
    d[[n]]<-frame
  }
  return(d)
}

#' Parses biotic XML to relational data frames / Tibbles, with foreign keys.
#' @param xmlfile String : path to xml file to be parsed
#' @param handlers list of handlers determining which table to parse and which version of biotic is parsed. Default: all and 1.4.
#' @param set_data_types logical: Indicate if data types should be set for columns. If False all datatypes will be character.
#' @param schema path to schemafile used for setting datatypes. Only used if set_data_types=T
#' @return named list of data frames / Tibbles, one for each complex type in xml.
#' @usage 
#' parse_biotic(xmlfile)
#' ## for default version and all data
#' 
#' parse_biotic(xmlfile, handlers=biotic_1_4_handlers[c("mission", "fishstation")])
#' ## for parsing only tables mission and fishstation with version 1.4
parse_biotic <- function(xmlfile, handlers=biotic_1_4_handlers, set_data_types=F, schema=NULL){
  if (set_data_types & is.null(schema)){
    stop("Can not find schema for dataframe annotation.")
  }
  if (!set_data_types & !is.null(schema)){
    warning("Schemafile specified, but set_data_types is False.")
  }
  
  bioticdata <<- list()
  xmlInternalTreeParse(xmlfile, handlers=handlers, ignoreBlanks=T, trim=T)
  d <- bioticdata
  bioticdata <<- list()
  d <- set_blanks_to_NA(d)
  if (set_data_types & !is.null(schema)){
    schema <- xmlParse(schema)
    d <- set_data_types(d, schema, keys=keys_biotic1_4)
  }
  return(d)
}

#' Converts biotic xml file to a relational model and saves this in a set of csv files.
#' @param bioticxml character: path to XML file
#' @param target_dir character: path to directory where csv files will be written.
#' @param overwrite logical: specifies if existing csv files should be overwritten
convert_to_csv <- function(bioticxml, target_dir=".", overwrite=F){
  bioticdata <- parse_biotic(bioticxml, schema=NULL)
  for (n in names(bioticdata)){
    filename <- paste(target_dir, "/", n, ".csv", sep="")
    if (file.exists(filename) & !overwrite){
      print(paste("File:", filename, "exists."))
    }
    else{
      write.csv(bioticdata[n], file=filename)  
    }
  }
}

#' rbind all dataframes in frames1, with corresponding frames in frames2
#' Any frame sin frames2, not in frames 1 is ignored.
#' Columns only present in one of the paired frames will be padded with NA values in the concatenated frame.
#' Apart from this no consistency or uniqueness checks are made
#' @param frames1 named list of Tibbles
#' @param frames2 named list of Tibbles
#' @return named list of Tibbles, one for each in frame1, with correspdonding frames in frames2 appended.
cat_dataframes <- function(frames1, frames2){
  dataframes <- frames1
  for (n in names(frames1)){
    if (!is.null(frames2[[n]])){
      dataframes[[n]] <- bind_rows(dataframes[[n]], frames2[[n]])      
    }
  }
  return(dataframes)
}

consolidate_data_frames <- function(){
  
}

test <- function(){
  dd<- parse_biotic(test_data, handlers=biotic_1_4_handlers[c("mission", "fishstation", "catchsample")], set_data_types=T, schema = test_schema)
  print(dd)
}