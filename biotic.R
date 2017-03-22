require(XML)
require(dplyr)

definitions <- "/Users/a5362/code/github/hi_biotic_parser/definitions"
example_files <- "/Users/a5362/code/github/hi_biotic_parser/example_files"
test_schema <- paste(definitions,"bioticv1_4_201703211557.xsd", sep="/")
test_refl_2015 <- paste(example_files, "refl_hav_2015.xml", sep="/")

#
# Development notes. See documentation for parse_biotic for user documentation.
#
# Parser for mapping biotic xml format to relational table structure represented as dplyr Tibbles.
# Each data frame / Tibble is represented by a complex type in the XML, with a corresponding name (except for the suffix "Type" in XML).
# All variables (columns) have corresponding elements or attributes in these complex types and inherit their documentation from there.
# In addition foreing keys are introduced to link tables. They are named "targettable.varaible"
#
# Parsing is not fully schema based. Therefore:
# - Variales that are not registered in the XML (but are allowed in biotic) will not be represented by any column in the parsed data frames
# - If path to schema is not given, all data types will be char. 
#
# Notes for adapting to future versions of biotic:
# Parser relies on keys being identified for each complex type that has non-simple children. See the list keys_biotic1_4 for an example
# Parser also relies on a table for identifying correspodning foreign keys. See the list foreign_keys_biotic1_4.
# Parser relies on element names being unique for the elements that map to data frames. 
# For example every occurance of an element mission will be attempted parsed as the complexType MissionType.
# This needs to be checked for updates as xml allows element names to be reused in different types.
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
                       PreyType=c("species", "partno"),
                       PreylengthType=c(),
                       CopepodedevstageType=c()
                       )

# Foreign keys
# Data frames will have foreign key columns relating it to all its parent elements
# This list defines the column names for the foreign keys and must be structured exactly like the keys-table above.
foreign_keys_biotic1_4 <- list(MissionsType=c(), 
                       MissionType=c("Mission.missiontype", "Mission.missionnumber", "Mission.year", "Mission.platform"),
                       FishstationType=c("Fishstation.serialno"), 
                       CatchsampleType=c("Catchsample.species", "Catchsample.samplenumber"),
                       IndividualType=c("Individual.specimenno"),
                       AgedeterminationType=c(),
                       TagType=c(),
                       PreyType=c("Prey.species", "Prey.partno"),
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
#'@param foreign_keys list mapping schematypes to foreign keys
set_data_types <- function(bioticdata, schema, keys, foreign_keys, datatype_mapping=dm){
  get_data_types <- function(typename){
    elements <- getNodeSet(schema, paste("/xs:schema/xs:complexType[@name='",typename,"']//xs:element", sep=""), c(xs="http://www.w3.org/2001/XMLSchema"))
    attribs <- getNodeSet(schema, paste("/xs:schema/xs:complexType[@name='",typename,"']//xs:attribute", sep=""), c(xs="http://www.w3.org/2001/XMLSchema"))
    fieldnames <- lapply(elements, xmlGetAttr, "name")
    fieldnames <- append(fieldnames, lapply(attribs, xmlGetAttr, "name"))
    fieldtypes <- lapply(elements, xmlGetAttr, "type")
    fieldtypes <- append(fieldtypes, lapply(attribs, xmlGetAttr, "type"))
    names(fieldtypes) <- fieldnames
    return(fieldtypes)
  }
  
  #convert data types
  for (fr in names(bioticdata)){
    typename <- paste(fr, "Type", sep="")
    dtypes <- get_data_types(typename)
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
  add_key_types <- function(data, typename){
    p_keys <- keys[[typename]]
    if (is.null(p_keys)){
      return(data)
    }
    f_keys <- foreign_keys[[typename]]
    types <- get_data_types(typename)
    for (fr in names(data)){
      frame <- data[[fr]]
      for (i in 1:length(p_keys)){
        k <- p_keys[[i]]
        fk <- f_keys[[i]]
        if (fk %in% names(frame)){
          t<-types[[k]]
          if (is.null(t)){
            stop()
          }
          ff<-datatype_mapping[[t]]
          if (is.null(ff)){
            stop()
          }
          frame[[fk]] <- eval(call(ff, frame[[fk]]))
        }
      }
      data[[fr]] <- frame
    }
    return(data)
  }
  for (tn in names(keys)){
    bioticdata <- add_key_types(bioticdata, tn)
  }
  return(bioticdata)
}

#' Makes a list mapping foreing key names to their values, for the arument nodes and for all parent nodes up til root or <missions/>
#' @param node node The foreign key will reference
#' @param keys_table list mapping schematypes to keys
#' @param foreign_keys_table list mapping schematypes to foreign keys
#' @param schematype_function function mapping node names to their schematype
#' @return list with names <node name>.<key attribute> and values the value of the <key attribute> for this node
make_foreign_keys <- function(node, keys_table, foreign_keys_table, schematype_function){
  if (is.null(node)){
    return(list())
  }
  schematype <- schematype_function(node)
  if (is.null(schematype)){
    return(list())
  }
  foreign_keys <- list()
  
  if (schematype!="MissionType"){
    foreign_keys <- append(make_foreign_keys(xmlParent(node), keys_table, foreign_keys_table, schematype_function), foreign_keys)
  }
  if (length(keys_table[[schematype]])==0){
    return(foreign_keys)
  }
  for (i in 1:length(keys_table[[schematype]])){
    k <- keys_table[[schematype]][[i]]
    fk <- foreign_keys_table[[schematype]][[i]]
    foreign_keys[fk] <- xmlGetAttr(node, k)
  }
  return(foreign_keys)
}

#' Sets all blank entries in data frames to NA.
#' @param dataframes named list of Tibbles
#' @return named list of tibbles where all occurances of "" is set to NA.
set_blanks_to_NA <- function(dataframes){
  d <- dataframes
  for (n in names(d)){
    frame <- d[[n]]
    for (cn in names(frame)){
      frame[!is.na(frame[,cn]) & frame[,cn]=="",cn] <-NA
    }
    d[[n]]<-frame
  }
  return(d)
}

bioticdata <- list()
#creates handler for parsing specific xml elements
make_data_frame_parser <- function(framename, foreign_key_generator, drop=c(), verbose=F){
  bioticdata[[framename]] <<- list()
  num=1
  parser <- function(node){
    nlist <- xmlApply(node, xmlValue)
    nlist[which(names(nlist) %in% drop)]<-NULL
    nlist <- append(foreign_key_generator(xmlParent(node)), nlist)
    nlist <- append(nlist, as.list(xmlAttrs(node)))
    bioticdata[[framename]][[num]] <<- nlist
    num <<- num + 1
    return(NULL)
  }
  verbose_parser <- function(node){
    cat("Parsing:", xmlName(node), " ", xmlAttrs(node), "\n")
    return(parser(node))
  }
  if (verbose){
    return(verbose_parser)
  }
  else{
    return(parser)
  }
}

foreing_key_generator_1_4 <- function(node){return(make_foreign_keys(node, keys_biotic1_4, foreign_keys_biotic1_4, hardcoded_schematype_function))}
biotic_1_4_handlers <- list(
  #<text/> is added to drop, because xmlInternalTreeParse(trim=T) does not handle \n
  #missions=make_data_frame_parser("Missions", foreing_key_generator_1_4, c("mission", "text")),
  mission=make_data_frame_parser("Mission", foreing_key_generator_1_4, c("fishstation", "text"), T),
  fishstation=make_data_frame_parser("Fishstation", foreing_key_generator_1_4, c("catchsample", "text"), T), 
  catchsample=make_data_frame_parser("Catchsample", foreing_key_generator_1_4, c("prey", "individual", "text")),
  individual=make_data_frame_parser("Individual", foreing_key_generator_1_4, c("agedetermination", "tag", "text")),
  prey=make_data_frame_parser("Prey", foreing_key_generator_1_4, c("preylength", "copepodedevstage", "text")),
  tag=make_data_frame_parser("Tag", foreing_key_generator_1_4, c("text")),
  agedetermination=make_data_frame_parser("Agedetermination", foreing_key_generator_1_4, c("text")),
  preylength=make_data_frame_parser("Preylength", foreing_key_generator_1_4, c("text")),
  copepodedevstage=make_data_frame_parser("Copepodedevstage", foreing_key_generator_1_4, c("text"))
)

#' Merges data into one flat Tibble
#' @param list of Tibbles as returned from parse_biotic
#' @return Tibble with merged data.
#' @details 
#' Assumes hierarchy is preserved, that is: No fishstations can be present if not mission present and so forth..
#' Otherwise only assumes presence of key and foreign key columns, so columns may be dropped before flattening to avoid name conflicts.
#' Platform is renamed on Fishstation, if present due to naming conflict with key column in mission. If the two platform columns are equal, one is removed.
flatten <- function(bioticdata, keys=keys_biotic1_4, foreign_keys=foreign_keys_biotic1_4) {
  require(tibble) # dplyr joins are slow for chars, for some reason. Use merge and cast
  flat <- bioticdata$Mission
  if (!is.null(bioticdata$Fishstation) && nrow(bioticdata$Fishstation)>0) {

    # merge does not handle renaming duplicated column names that are used as keys (by=)
    fs <- bioticdata$Fishstation
    if ("platform" %in% names(fs)){
      fs<-rename(fs, FS.platform = platform)
    }
    flat <- 
      merge(
        flat,
        fs,
        by.x = keys$MissionType,
        by.y = foreign_keys$MissionType,
        suffixes = c("", ".fishstation")
      )
    if ("platform" %in% names(flat) & all(flat$platform == flat$FS.platform)){
      flat <- flat[,names(flat)!="FS.platform"]
    }
    flat <- as_tibble(flat)
  }
  if (!is.null(bioticdata$Catchsample) && nrow(bioticdata$Catchsample)>0) {
    flat <- as_tibble(
      merge(
        flat,
        bioticdata$Catchsample,
        by.x = c(keys$MissionType, keys$FishstationType),
        by.y = c(foreign_keys$MissionType, foreign_keys$FishstationType),
        suffixes = c("", ".catchsample")
      ))
  }
  if (!is.null(bioticdata$Individual) && nrow(bioticdata$Individual)>0) {
    flat <- as_tibble(
      merge(
        flat,
        bioticdata$Individual,
        by.x = c(keys$MissionType, keys$FishstationType, keys$CatchsampleType),
        by.y = c(foreign_keys$MissionType, foreign_keys$FishstationType, foreign_keys$CatchsampleType),
        all.y=T,
        suffixes = c("", ".individual")
      ))
  }
  
  if (!is.null(bioticdata$Prey) && nrow(bioticdata$Prey)>0) {
    flat <- as_tibble(
      merge(
        flat,
        bioticdata$Prey,
        by.x = c(keys$MissionType, keys$FishstationType, keys$CatchsampleType),
        by.y = c(foreign_keys$MissionType, foreign_keys$FishstationType, foreign_keys$CatchsampleType),
        all.y=T,
        suffixes = c("", ".prey")
      ))
  }
  
  if (!is.null(bioticdata$Agedetermination) && nrow(bioticdata$Agedetermination)>0) {
    flat <- as_tibble(
      merge(
        flat,
        bioticdata$Agedetermination,
        by.x = c(keys$MissionType, keys$FishstationType, keys$CatchsampleType, keys$IndividualType),
        by.y = c(foreign_keys$MissionType, foreign_keys$FishstationType, foreign_keys$CatchsampleType, foreign_keys$IndividualType),
        all.y=T,
        suffixes = c("", ".agedetermination")
      ))
  }
  
  if (!is.null(bioticdata$Tag) && nrow(bioticdata$Tag)>0) {
    flat <- as_tibble(
      merge(
        flat,
        bioticdata$Tag,
        by.x = c(keys$MissionType, keys$FishstationType, keys$CatchsampleType, keys$IndividualType),
        by.y = c(foreign_keys$MissionType, foreign_keys$FishstationType, foreign_keys$CatchsampleType, foreign_keys$IndividualType),
        all.y=T,
        suffixes = c("", ".tag")
      ))
  }
  
  if (!is.null(bioticdata$PreyLength) && nrow(bioticdata$PreyLength)>0) {
    flat <- as_tibble(
      merge(
        flat,
        bioticdata$PreyLength,
        by.x = c(keys$MissionType, keys$FishstationType, keys$CatchsampleType, keys$IndividualType, keys$PreyType),
        by.y = c(foreign_keys$MissionType, foreign_keys$FishstationType, foreign_keys$CatchsampleType, foreign_keys$IndividualType, foreign_keys$PreyType),
        all.y=T,
        suffixes = c("", ".preylength")
      ))
  }
  if (!is.null(bioticdata$Copepodedevstage) && nrow(bioticdata$Copepodedevstage)>0) {
    flat <- as_tibble(
      merge(
        flat,
        bioticdata$Copepodedevstage,
        by.x = c(keys$MissionType, keys$FishstationType, keys$CatchsampleType, keys$IndividualType, keys$PreyType),
        by.y = c(foreign_keys$MissionType, foreign_keys$FishstationType, foreign_keys$CatchsampleType, foreign_keys$IndividualType, foreign_keys$PreyType),
        all.y=T,
        suffixes = c("", ".copepodedevstage")
      ))
  }
  return(flat)
}

#
# Functions for parsing and data handling.
#

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
#' @details 
#' Note that each column derives its name from the xml format. For documentation for Fishstation$serialno, see the documentation for 
#' the element serialno in FishstationType in the XML format.
#' Foreign keys derives their documentation from the corresponding primary key and are named according to the convention <target table>.<primary key>.
#' For instance the data frame / Tibble Catchsample has a column Fishstation.serialno.
#' The first columns in each frame / Tibble are the foreign keys.
parse_biotic <- function(xmlfile, handlers=biotic_1_4_handlers, set_data_types=F, schema=NULL){
  if (!file.exists(xmlfile)){
    stop(paste("Can not find file:", xmlfile))
  }
  if (set_data_types & is.null(schema)){
    stop("Can not find schema for dataframe annotation.")
  }
  if (!set_data_types & !is.null(schema)){
    warning("Schemafile specified, but set_data_types is False.")
  }
  
  bpre <- bioticdata
  xmlInternalTreeParse(xmlfile, handlers=handlers, ignoreBlanks=T, trim=T)
  d <- bioticdata
  bioticdata <<- bpre
  
  for (n in names(d)){
    d[[n]]<-bind_rows(d[[n]])
  }
  
  d <- set_blanks_to_NA(d)
  if (set_data_types & !is.null(schema)){
    schema <- xmlParse(schema)
    d <- set_data_types(d, schema, keys=keys_biotic1_4, foreign_keys=foreign_keys_biotic1_4)
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

test <- function(){
  dd<- parse_biotic(test_refl_2015, handlers=biotic_1_4_handlers[c("mission", "fishstation")], set_data_types=T, schema = test_schema)
  print(dd)
}