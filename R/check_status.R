#' Identifies open nomenclature in species scientific name and classification under species level
#'
#' This function identifies aff., cf. and subsp. var. in species names. It creates a new column with the original string and the new suggested name. It also flags problematic names (character string with numbers or other names besides genus and epithet).
#'
#' @param scientificName species name in a raw format
#' @author Sara Mortara
#'
#' @examples
#' check_status("Lindsaea lancea var. falcata")
#' check_status(c("Lindsaea lancea var. falcata", "Asplenium Aff. truncorum"))
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_split
#' @importFrom stringr str_trim
#' @export
#'
check_status <- function(scientificName = NULL){
  aff_string <- " aff |  Aff | aff.| Aff."
  cf_string <- " cf | Cf | cf. | Cf."
  subsp_string <-  "subsp |  Subsp | subsp.| Subsp."
  var_string <- " var |  Var | var.| Var."
  aff_cf <- paste(aff_string, cf_string, sep = "|")
  subsp_var <- paste(subsp_string, var_string, sep = "|")
  aff <- stringr::str_detect(scientificName, aff_string)
  cf <- stringr::str_detect(scientificName, cf_string)
  subsp <- stringr::str_detect(scientificName, subsp_string)
  var <- stringr::str_detect(scientificName, var_string)
  check <- data.frame(scientificName = scientificName)
  check$scientificName_status <- NA
  check$scientificName_status[aff] <- "affinis"
  check$scientificName_status[cf] <- "conferre"
  check$scientificName_status[subsp] <- "subspecies"
  check$scientificName_status[var] <- "variety"
  clean_uncertain <- function(x)  {
    x_new <- stringr::str_trim(stringr::str_replace(x, aff_cf, ""))
    return(x_new)
  }
  clean_sub <- function(x){
    x_new <- stringr::str_trim(unlist(lapply(stringr::str_split(x, subsp_var),
                                               function(x) x[1])))
    return(x_new)
  }
 check$scientificName_new <- ifelse(check$scientificName_status
                                     %in% c("affinis", "conferre"),
                                     clean_uncertain(check$scientificName),
                                     clean_sub(check$scientificName))
  return(check)
}
