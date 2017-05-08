library(XML)

example_files <- "/Users/a5362/code/github/hi_biotic_parser/example_files"
provebat <- paste(example_files, "11-2017-3654-1.xml", sep="/")

#
# Functions for filtering xml based on xpath expressions. This is generally faster than parsing for large files.
#


#' Removes from tree all nodes matching the xpath expression.
#' Tree is modified in place.
#' @param tree xml tree as return by xmlParse
#' @param xpath xpath expression to evaluate
#' @param namespaces: named character vector mapping namespace prefixes to namespace
#' @return NULL
drop_nodes <-function(tree, xpath, namespaces){
  nodes <- getNodeSet(tree, xpath, namespaces)
  for (n in nodes){
    p <- xmlParent(n)
    removeChildren(p, n)
  }
  return(NULL)
}

#
# file handling
#

#' Applies filters to xml file
#' @param infile xml file to filter
#' @param outfile file name to save filtered results
#' @param filters: list of functions, each shold acccept a tree (as returned bu xmlParse) and modify it in place.
#' @details filters will be applied in order
#' @return NULL
apply_xml_filters <- function(infile, outfile, filters){
  tree <- xmlParse(infile)
  for (f in filters){
    f(tree)
  }
  saveXML(tree, outfile)
  return(NULL)
}

#
# Specific filters
#


#' Removes all catchsample nodes that is not a catch in specieslist
#' Tree is modified in place.
#' @param tree tree to filter.
#' @param specieslist vector of tsn-codes to retain
#' @param return NULL
retain_species_catch <- function(tree, specieslist){
  species_sel <- paste(paste("@species!='", specieslist, "'", sep=""), collapse=" and ")
  xpath <- paste("//m:catchsample[", species_sel, "]", sep="")
  drop_nodes(tree, xpath, "m")
  return(NULL)
}

#' Removes all fishstations with no children
#' @param tree tree to be filtered
#' @return NULL
drop_childless_stations <- function(tree){
  xpath = "//m:fishstation[not(m:catchsample)]"
  drop_nodes(tree, xpath, "m")
  return(NULL)
}

#' Removes all mission with no fishstations
#' @param tree tree to be filtered
#' @return NULL
drop_childless_missions <- function(tree){
  xpath = "//m:mission[not(m:fishstation)]"
  drop_nodes(tree, xpath, "m")
  return(NULL)
}

