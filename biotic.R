require(XML)
require(dplyr)

test_schema <- "/Users/a5362/code/hi_formats/commons-biotic-jaxb/src/main/resources/bioticv1.xsd"
test_data <- "/Users/a5362/hi_home/stox/tests/Capelin BS 2012/input/biotic/gs_2012.xml"
test_refl_2015 <- "/Users/a5362/hi_home/bifangst/bruskfisk/referansefl_test/refl_hav_2015.xml"

# Parser for mapping biotic xml format to relational table structure represented as dplyr Tibbles.
# Each data frame / Tibble is represented by a complex type in the XML, with a corresponding name (except for the suffix "Type" in XML).
# All variables (columns) have corresponding elements or attributes in these complex types and inherit their documentation from there.
# In addition foreing keys are introduced to link tables. They are named "targettable.varaible"
#
# Schema is not handled explcitly. Therefore:
# - Variales that are not registered in the XML (but are allowed in biotic) will not be represented by any column in the parsed data frames
# - Variables are not assinged a data type (they are all strings).
#
#
#
# Notes for adapting to future versions of biotic:
# Parser relies on keys being identified for each complex type that has non-simple children. See the list keys_biotic1_4 for an example
# Parser relies on element names being unique for the elements that map to data frames. 
# This needs to be checked for updates as xml allows element names to be reused in different types
# It is OK if an attribute som some element has the same name as another element. In fact this is the case in biotic 1.4 for copepodedevstage
#

# Mission is uniquely identified by its keys.
# All keys are attributes in the xml.
# All types occuring below FishStation will also have as keys the foreing keys of all parent elements
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


data_frames <- list()
#creates handler for parsing specific xml elements
make_data_frame_parser <- function(framename, foreign_key_generator, drop=c(), verbose=T){
  parser <- function(node){
    nlist <- xmlApply(node, xmlValue)
    nlist[which(names(nlist) %in% drop)]<-NULL
    nlist <- append(nlist, foreign_key_generator(xmlParent(node)))
    nlist <- append(nlist, as.list(xmlAttrs(node)))
    data_frames[[framename]] <<- bind_rows(data_frames[[framename]], nlist)
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
        mission=make_data_frame_parser("Mission", foreing_key_generator_1_4, c("fishstation", "text")),
        fishstation=make_data_frame_parser("Fishstation", foreing_key_generator_1_4, c("catchsample", "text")), 
        catchsample=make_data_frame_parser("Catchsample", foreing_key_generator_1_4, c("prey", "individual", "text")),
        individual=make_data_frame_parser("Individual", foreing_key_generator_1_4, c("agedetermination", "tag", "text")),
        prey=make_data_frame_parser("Prey", foreing_key_generator_1_4, c("preylength", "copepodedevstage", "text")),
        tag=make_data_frame_parser("Tag", foreing_key_generator_1_4, c("text")),
        agedetermination=make_data_frame_parser("Agedetermination", foreing_key_generator_1_4, c("text")),
        preylength=make_data_frame_parser("Preylength", foreing_key_generator_1_4, c("text")),
        copepodedevstage=make_data_frame_parser("Copepodedevstage", foreing_key_generator_1_4, c("text"))
        )

#' Parses biotic XML to relational data frames / Tibbles, with foreign keys.
#' @param xmlfile String : path to xml file to be parsed
#' @param handlers list of handlers determining which table to parse and which version of biotic is parsed. Default: all and 1.4.
#' @return named list of data frames / Tibbles, one for each complex type in xml.
#' @usage 
#' parse_biotic(xmlfile)
#' ## for default version and all data
#' parse_biotic(xmlfile, handlers=biotic_1_4_handlers[c("mission", "fishstation")])
#' ## for parsing only tables mission and fishstation with version 1.4
parse_biotic <- function(xmlfile, handlers=biotic_1_4_handlers){
  data_frames <<- list()
  xmlInternalTreeParse(xmlfile, handlers=handlers)
  d <- data_frames
  data_frames <<- list()
  return(d)
}

test <- function(){
  df <- parse_biotic(test_refl_2015, biotic_1_4_handlers["mission"])
  return(df)
}
