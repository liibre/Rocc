#' Gets occurrence data from speciesLink
#'
#' This function access version beta 0.1 of speciesLink API and returns occurrence data from species.
#'
#' @param dir Path to directory where the file will be saved. Default is to create a "results/" directory
#' @param filename Name of the output file
#' @param basisofrecord Character. Any in 'PreservedSpecimen', 'LivingSpecimen', 'FossilSpecimen',
#' 'HumanObservation', 'MachineObservation' or 'MaterialSample'. Default is 'PreservedSpecimen' for museum and herbarium search.
#' @param scientificname Genus and epithet separated by space
#' @param collectioncode Any collection available at speciesLink. Example: ALCB, E, INPA, MOBOT_BR
#' @param country Any country name. No ASCII characters allowed
#' @param county Any municipality name. No ASCII characters allowed
#' @param stateprovince
#' @param typestatus
#' @param Scope Group to be required. If NULL searches all groups. Any in "plants", "animals", "microrganisms" or "fossils"
#' @param Synonyms If species names should be checked for synonyms in a specific dictionary. Set to "species2000" for search in Catálogo da Vida species2000, "flora2020" for Flora do Brasil 2020, "MycoBank" for MycoBank, "AlgaeBase" for AlgaeBase, "DSMZ" for  DSMZ Prokaryotic Nomenclature Up-to-Date, "Moure" for Catálogo de Abelhas Moure or "no synonyms".
#' @param Typus Logic. If TRUE select only typus
#' @param Coordinates Specify if records should have coordinates. Default is "no check" but it also accepts "Yes", No", "Original", "Automatic", "Blocked" or "no check"
#' @param CoordinatesQuality Any character in "Good" or "Bad" to select specific type of coordinates
# @param Format Output format of file. Default is to "TAB, but it also accepts "JSON", "XML", or "CSV"
# @param Separator Column separator for CSV format. Default is "comma" but is can also be "semicolon"
#' @param RedList
#' @param MaxRecords Numeric. Maximum number of records to be required
#' @param Model If file should be prepared in DarwinCore(DwC) or "modelling" format. Default is DwC
#' @param Images If select only records with images. Default is NULL. It accepts: "Yes", "Live", "Polen", "Wood"
#' @return A list of two elemnts. The first element is a character string containing the url search and the second element is a data.frame with the search result
#' @author Sara Mortara
#' @examples
#'
#'ex01 <- spLink_url(filename = "ex01",
#'scientificname =  c("Eugenia platyphylla", "Chaetocalyx acutifolia"),
#'Scope="plants")
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom readr read_tsv
#' @importFrom readr locale
#' @importFrom utils write.table
#' @export
spLink_url <-function(dir = "results/",
                      filename = "output",
                      scientificname=NULL,
                      collectioncode=NULL,
                      county=NULL,
                      Coordinates=NULL, #		Yes | No | Original | Automatic | Blocked
                      coordinatesQuality=NULL,	#Good | Bad
                      #Format="TAB", #	so far it is only working w/ TAB -	JSON | XML | CSV | TAB
                      #Separator="comma", #		comma | semicolon	Valid only for Format = CSV
                      MaxRecords=NULL, #		n > 0	 all records
                      Model=NULL, # DwC | modelling
                      Scope=NULL, #			plants, animals, microrganisms,fossils
                      Synonyms="no synomyms", #species2000 | flora2020 | MycoBank | AlgaeBase | DSMZ | Moure no synonyms
                      Images=NULL) { # Yes | No | Live | Polen | Wood
  # speciesLink url
  my_url <- "http://api.splink.org.br/records/"
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  url_query <-  function(vector, name){
    char <- paste(paste0(vector, "/"), collapse="")
    url <- paste0(name, "/", char)
    return(url)
  }
  # Collection code
  if (is.null(collectioncode)) {
    my_url
  } else {
    if (is.character(collectioncode)){
      cc <- url_query(collectioncode, "collectioncode")
      my_url <- paste0(my_url, cc)
    }
  }
  # Species name
  if (is.null(scientificname)) {my_url}
    else  {if (is.character(scientificname)){
      scientificname <- gsub(" ", "%20", scientificname)
      sp <- url_query(scientificname, "scientificname")
      my_url <- paste0(my_url, sp)
    }
      else stop("scientificname must be a character")
    }
  # county
  if (is.null(county)) {
    my_url
  } else {
    if (is.character(county)){
      county <- gsub(" ", "%20", county)
      co <- url_query(county, "county")
      my_url <- paste0(my_url, co)
    }
  }
  # Coordinates
  if (is.null(Coordinates)) {
    my_url
  } else {
    if (Coordinates%in%c("Yes", "No", "Original", "Automatic", "Blocked")) {
      xy <- url_query(Coordinates, "Coordinates")
      my_url <- paste0(my_url, xy)
    }
  }
  # Coordinates quality
  if(is.null(CoordinatesQuality)) {
    my_url
  } else {
    if(CoordinatesQuality%in%c("Good", "Bad")){
      cq <- url_query(CoordinatesQuality, "CoordinatesQuality")
      my_url <- paste0(my_url, cq)
    }
  }
  # Format
  # if(is.null(Format)) {
  #   my_url
  # } else {
  #   if(Format%in%c("CSV", "JSON", "XML", "CSV", "TAB")){
  #     fo <- url_query(Format, "Format")
  #     my_url <- paste0(my_url, fo)
  #   }
  # }
  # Separator
  # if(is.null(collectioncode)) {
  #   my_url
  # } else {
  #   if(Separator%in%c("comma", "semicolon")){
  #     se <- url_query(Separator, "Separator")
  #     my_url <- paste0(my_url, se)
  #   }
  # }
  # MaxRecords
  if(is.null(MaxRecords)) {
    my_url
  } else {
    if(is.numeric(MaxRecords)){
      mr <- url_query(MaxRecords, "MaxRecords")
      my_url <- paste0(my_url, mr)
    }
  }
  # Model
  if(is.null(Model)) {
    my_url
  } else {
    if(Model%in%c("DwC","modelling")){
      mo <- url_query(Model, "Model")
      my_url <- paste0(my_url, mo)
    }
  }
  # Scope
  if(is.null(Scope)) {
    my_url
  } else {
    if(Scope%in%c("plants", "animals", "microrganisms", "fossils")){
      sc <- url_query(Scope, "Scope")
      my_url <- paste0(my_url, sc)
    }
  }
  # Synonyms
  if(is.null(Synonyms)) {
    my_url
  } else {
    if(Synonyms%in%c("species2000", "flora2020", "MycoBank", "AlgaeBase", "DSMZ")){
      sy <- url_query(Synonyms, "Synonyms")
      my_url <- paste0(my_url, sy)
    }
  }
  # Images # "Yes", "Live", "Polen", "Wood"
  if(is.null(Images)) {
    my_url
  } else {
    if(Images%in%c("Yes", "Live", "Polen", "Wood")){
      im <- url_query(Images, "Images")
      my_url <- paste0(my_url, im)
    }
  }
  message("Making request to speciesLink...")
  r <- httr::GET(my_url)
  message("Extracting content ...")
  rr <- httr::content(r, as="parse") # text content
  message("Converting content to an R object.")
  # so far requesting only tab format
  rrr <- readr::read_tsv(rr, locale = readr::locale(encoding = "UTF-8"))
  write.table(rrr, paste0(dir, filename, ".csv"), sep=",", row.names = FALSE, col.names = TRUE)
  return(list(data=rrr, url=my_url))
  }
