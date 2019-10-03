#' Gets occurrence data from jabot
#'
#' This function gets occurrence data in the Jabot database (http://jabot.jbrj.gov.br/v3/consulta.php).
# rjabot <-function(dir="results/",
#                   filename="output",
#                   basisofrecord=NULL,
#                   scientificname=NULL,
#                   collectioncode=NULL,
#                   country=NULL,
#                   stateprovince=NULL,
#                   county=NULL,
#                   Coordinates=NULL, #		Yes | No | Original | Automatic | Blocked
#                   CoordinatesQuality=NULL,	#Good | Bad
#                   Scope=NULL, #			plants, animals, microrganisms,fossils
#                   Synonyms="no synomyms", #species2000 | flora2020 | MycoBank | AlgaeBase | DSMZ | Moure no synonyms
#                   Typus=FALSE,
#                   Images=NULL,
#                   RedList=FALSE,
#                   MaxRecords=NULL #		n > 0	 all records
# ) { # Yes | No | Live | Polen | Wood
#   # speciesLink url
#   my_url <- "http://aplicacoes.jbrj.gov.br/jabot/v2/ws/server.php?coordenada=S&"
#   # creting dir
#   dir.create(dir, recursive = TRUE, showWarnings = FALSE)
#   # helper function
#   url_query <-  function(vector, name){
#     char <- paste(paste0(vector, "/"), collapse="")
#     url <- paste0(name, "/", char)
#     return(url)
#   }
#   # basis of record
#   if (is.null(basisofrecord)) {
#     my_url
#   } else {
#     if (basisofrecord%in%c('PreservedSpecimen','LivingSpecimen','FossilSpecimen',
#                            'HumanObservation','MachineObservation','MaterialSample')){
#       br <- url_query(basisofrecord, "basisofrecord")
#       my_url <- paste0(my_url, br)
#     }
#   }
#   # Species name
#   if (is.null(scientificname)) {my_url}
#   else  {if (is.character(scientificname)){
#     scientificname <- gsub(" ", "%20", scientificname)
#     sp <- url_query(scientificname, "scientificname")
#     my_url <- paste0(my_url, sp)
#   }
#     else stop("scientificname must be a character")
#   }
#   # Collection code
#   if (is.null(collectioncode)) {
#     my_url
#   } else {
#     if (is.character(collectioncode)){
#       cc <- url_query(collectioncode, "collectioncode")
#       my_url <- paste0(my_url, cc)
#     }
#   }
#   # country
#   if (is.null(country)) {
#     my_url
#   } else {
#     if (is.character(country)){
#       country <- gsub(" ", "%20", country)
#       ct <- url_query(country, "country")
#       my_url <- paste0(my_url, ct)
#     }
#   }
#   # stateprovince
#   if (is.null(stateprovince)) {
#     my_url
#   } else {
#     if (is.character(stateprovince)){
#       stateprovince <- gsub(" ", "%20", stateprovince)
#       st <- url_query(stateprovince, "stateprovince")
#       my_url <- paste0(my_url, st)
#     }
#   }
#   # county
#   if (is.null(county)) {
#     my_url
#   } else {
#     if (is.character(county)){
#       county <- gsub(" ", "%20", county)
#       co <- url_query(county, "county")
#       my_url <- paste0(my_url, co)
#     }
#   }
#   # Coordinates
#   if (is.null(Coordinates)) {
#     my_url
#   } else {
#     if (Coordinates%in%c("Yes", "No", "Original", "Automatic", "Blocked")) {
#       xy <- url_query(Coordinates, "Coordinates")
#       my_url <- paste0(my_url, xy)
#     }
#   }
#   # Coordinates quality
#   if(is.null(CoordinatesQuality)) {
#     my_url
#   } else {
#     if(CoordinatesQuality%in%c("Good", "Bad")){
#       cq <- url_query(CoordinatesQuality, "CoordinatesQuality")
#       my_url <- paste0(my_url, cq)
#     }
#   }
#   # Scope
#   if(is.null(Scope)) {
#     my_url
#   } else {
#     if(Scope%in%c("plants", "animals", "microrganisms", "fossils")){
#       sc <- url_query(Scope, "Scope")
#       my_url <- paste0(my_url, sc)
#     }
#   }
#   # Synonyms
#   if(is.null(Synonyms)) {
#     my_url
#   } else {
#     if(Synonyms%in%c("species2000", "flora2020", "MycoBank", "AlgaeBase", "DSMZ")){
#       sy <- url_query(Synonyms, "Synonyms")
#       my_url <- paste0(my_url, sy)
#     }
#   }
#   if(Typus==FALSE) {
#     my_url
#   } else {
#     my_url <- paste0(my_url, "Typus/Yes/")
#   }
#   # Images # "Yes", "Live", "Polen", "Wood"
#   if(is.null(Images)) {
#     my_url
#   } else {
#     if(Images%in%c("Yes", "Live", "Polen", "Wood")){
#       im <- url_query(Images, "Images")
#       my_url <- paste0(my_url, im)
#     }
#   }
#   # RedList
#   if(RedList==FALSE) {
#     my_url
#   } else {
#     my_url <- paste0(my_url, "RedList/Yes/")
#   }
#   # MaxRecords
#   if(is.null(MaxRecords)) {
#     my_url
#   } else {
#     if(is.numeric(MaxRecords)){
#       mr <- url_query(MaxRecords, "MaxRecords")
#       my_url <- paste0(my_url, mr)
#     }
#   }
#   my_url <- paste0(my_url, "Format/JSON/") #Model/DwC is already default
#   message("Making request to speciesLink...")
#   #r <- httr::GET(my_url)
#   #message("Extracting content ...")
#   #rr <- httr::content(r, as="parse") # text content
#   # requesting JSON format
#   rrr <- jsonlite::fromJSON(my_url)$result
#   #rrr <- readr::read_tsv(rr, locale = readr::locale(encoding = "UTF-8"))
#   fullname <- paste0(dir, filename, ".csv")
#   message(paste0("Writing ", fullname, " on disk."))
#   write.table(rrr, fullname, sep=",", row.names = FALSE, col.names = TRUE)
#   return(list(data=rrr, url=my_url))
# }
