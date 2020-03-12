#' Identifies open nomenclature in species scientific name and classification under species level
#'
#' This function identifies aff., cf. and subsp. var. in species names. It creates a new column with the original string and the new suggested name.
#'
#' @param scientificName species name in a raw format
#' @author Sara Mortara
#'
#' @examples
#'
#' @importFrom stringr str_detect
#' @export
#'
check_status <- function(scientificName = NULL){
  aff <- stringr::str_detect(scientificName, " aff.")
  cf <- stringr::str_detect(scientificName, " cf.")
  subsp <- stringr::str_detect(scientificName, " subsp.")
  var <- stringr::str_detect(scientificName, " var.")
  check <- data.frame(scientificName = scientificName)
  check$status <- NA
  check$status[aff] <- "affinis"
  check$status[cf] <- "conferre"
  check$status[subsp] <- "subspecies"
  check$status[var] <- "variety"
}
