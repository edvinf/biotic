library(worrms)
library(Rstox)
library(memoise)

#
# Funksjon for å hente ut spesifikt taksonomisk nivå fra aphia-kode
#


#' Get classification for aphiaid at given rank from worrms db
#' @param aphiaid chr or integer
#' @param ranks taxonomic rank, case sensitive (E.g: Kingdom Phylum Subphylum Superclass Superclass Class Order Family Genus Species)
#' @return scientific name for the species at the given taxonomic rank. NA if aphiaid is not classified at given rank.
taxonomy <- function(aphiaid, rank){
	classification <- wm_classification(as.integer(aphiaid))
	if (is.na(aphiaid) | nrow(classification)==0){
		stop(paste("No classification found for aphia id ", aphiaid))
	}
	
	l <- classification[classification[,"rank"]==rank,]
	if (nrow(l) == 0){
		return(NA)
	}
	else if(nrow(l) == 1){
		return(l$"scientificname")
	}
	else{
		stop(paste("Classification not unqiue at rank", rank, aphiaid))
	}
	return(cl)
}
# make function store results in memory, to avoid repeated internet lookups for the same arguments
taxonomy <- memoise(taxonomy, cache=cache_memory(algo = "sha512"))
# make function take vectors as arguments
taxonomy <- Vectorize(taxonomy)


#Last ned og les fangstdata
CS <- getNMDinfo("cs")
myCS <- names(CS)[1]
projects <- getNMDdata(cruise=myCS, group="year", subset=1:1, StationLengthDist=list(LengthDistType="LengthDist"), ow=TRUE)
ddir <- paste(getProjectPaths(), projects[1], sep="/") 
dat <- readXMLfiles(ddir)
catches <- dat$ReadBioticXML$`2_ReadBioticXML_BioticData_CatchSample.txt`
#rensk ut fangster som mangler aphiakode
mangler <- is.na(catches$aphia)
warning(paste(sum(mangler)), " Fangstprøver tatt bort grunnet manglende aphiaid.")
catches <- catches[!mangler,]

#Slå opp classification med worrms for en art. Dette vil gjøre et kall til worms-databasen over internett.
wm_classification(catches$aphia[1])

#annoter fangster med familie og orden og klasse vha funksjonen definert over.
catches[,"family"] <- taxonomy(catches$aphia, "Family")
catches[,"order"] <- taxonomy(catches$aphia, "Order")
catches[,"class"] <- taxonomy(catches$aphia, "Class")

# skriv ut artstabell. Merk at aphiakoder som refererer til høyere klassifisering får NA-verdier for familie og orden.
# Om man lagrer en slik tabell lokalt kan man senere annotere fangster som har de samme aphia-kodene vha merge(catch, txatable, by="aphia")
print(unique(catches[c("aphia", "noname", "family", "order", "class")]))

#skriv ut vekt og art for fangster av torskefisk
print(catches[!is.na(catches$order) & catches$order=="Gadiformes",c("weight", "noname")])

#worrms tilbyr også oppslag av andre artskoder via funksjonen wm_external
wm_external(catches$aphia[1], type="tsn") #for tsn kode.