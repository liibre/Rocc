#' Identifies open nomenclature in species scientific name and classification under species level
#'
#' This function identifies aff., cf. and subsp. var. in species names. It creates a new column with the original string and the new suggested name. It also flags problematic names (character string with numbers, authors, wrong case, or other names besides genus and epithet).
#'
#' @return
#' Data frame with `scientificName` as in the original input, `scientificName_status` with the flags in original data and `scientificName_new` with a suggestion for a more correct species name. The column `scientificName_status` accepts:
#'\describe{
#'\item{`possibly_ok`: }{scientific name following the expected pattern Genus epithet}
#'\item{`not_Genus_epithet_format`: }{scientific name not following the expected pattern Genus epithet}
#'\item{`variety`: }{species scientific name with variety}
#'\item{`subspecies`: }{species scientific name with subspecies}
#'\item{`conferre`: }{open nomenclature cf. in species scientific name}
#'\item{`affinis`: }{open nomenclature aff. in species scientific name}
#'\item{`name_w_authors`: }{species scientific name has authors}
#'\item{`not_name_has_digits`: }{species scientific name has digits, not a valid name}
#'}
#'
#' @details
#' Flags returned by column
#'
#' @param scientificName species name in a raw format
#' @author Sara Mortara
#'
#' @examples
#' check_status("Lindsaea lancea var. falcata")
#' check_status(c("Lindsaea lancea var. falcata", "Asplenium Aff. truncorum"))
#'
#' @importFrom stringr str_detect str_replace str_split str_trim
#' @importFrom flora remove.authors
#'
#' @export
#'
check_status <- function(scientificName = NULL){
  # definings possible string options
  aff_string <- " aff |  Aff | aff.| Aff."
  cf_string <- " cf | Cf | cf. | Cf."
  subsp_string <-  "subsp |  Subsp | subsp.| Subsp."
  var_string <- " var |  Var | var.| Var."
  aff_cf <- paste(aff_string, cf_string, sep = "|")
  subsp_var <- paste(subsp_string, var_string, sep = "|")
  # detecting status
  aff <- stringr::str_detect(scientificName, aff_string)
  cf <- stringr::str_detect(scientificName, cf_string)
  subsp <- stringr::str_detect(scientificName, subsp_string)
  var <- stringr::str_detect(scientificName, var_string)
  check <- data.frame(scientificName = scientificName)
  # defining status
  check$scientificName_status <- NA
  check$scientificName_status[aff] <- "affinis"
  check$scientificName_status[cf] <- "conferre"
  check$scientificName_status[subsp] <- "subspecies"
  check$scientificName_status[var] <- "variety"
  # accessory functions
  clean_uncertain <- function(x)  {
    x_new <- stringr::str_trim(stringr::str_replace(x, aff_cf, ""))
    return(x_new)
  }
  clean_sub <- function(x){
    x_new <- stringr::str_trim(unlist(lapply(stringr::str_split(x, subsp_var),
                                             function(x) x[1])))
    return(x_new)
  }
  # providing cleaned name
  check$scientificName_new <- ifelse(check$scientificName_status
                                     %in% c("affinis", "conferre"),
                                     clean_uncertain(check$scientificName),
                                     clean_sub(check$scientificName))
  ## other types of basic cleaning
  # recognizig authors
  no_authors <- lapply(check$scientificName_new, remove.authors)
  id_authors <- check$scientificName_new != no_authors
  check$scientificName_status[id_authors] <- "name_w_authors"
  check$scientificName_new[id_authors] <- no_authors[id_authors]
  # recognizig digits
  id_digits <- stringr::str_detect(check$scientificName, '\\d')
  check$scientificName_status[id_digits] <- "not_name_has_digits"
  # names not matching Genus + species pattern
  id_not_gensp <- sapply(stringr::str_split(check$scientificName_new, " "),
                         length) > 2
  check$scientificName_status[id_not_gensp] <- "not_Genus_epithet_format"
  # matching case
  low <- tolower(check$scientificName_new)
  ## fix case
  case <- paste(toupper(substring(low, 1, 1)),
                substring(low, 2), sep = "", collapse = " ")
  id_case <- check$check$scientificName_new != case
  check$scientificName_status[id_case] <- "name_w_wrong_case"
  check$scientificName_new[id_case] <- case[id_case]
  check$scientificName_status[is.na(check$scientificName_status)] <- "possibly_ok"
  return(check)
}
