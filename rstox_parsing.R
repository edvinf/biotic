library(Rstox) #version 1.6
example_files <- "/Users/a5362/code/github/hi_biotic_parser/example_files"
provebat <- paste(example_files, "11-2017-3654-1.xml", sep="/")

# loads data similar to biotic.R, but much faster, and with a simpler key-structure.
# Unfortuntaly mission details are not parsed, some columns are dropped wrp biotic1_4 and some extra columns are introduced (partly for keying-purposes).
bioticdata <- readXMLfiles(provebat)

# Using NMD interface (taken from RStox examples)
# NB: Rstudio may prevent file downloads with libcurl, if using Rstudio, uncheck Tools>Global Options: Packages - "Use secure download ..."
CS <- getNMDinfo("cs")
names(CS)
myCS <- names(CS)[1]
CS[myCS]

projects <- getNMDdata(cruise=myCS, group="year", subset=1:2, StationLengthDist=list(LengthDistType="LengthDist"), ow=TRUE)

#Access to NMD reference tables from RStox doc
# A list of available reference data:
g1 <- getNMDinfo()
#get specific reference table.
g9 <- getNMDinfo("taxa")
