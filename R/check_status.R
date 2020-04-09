#' Check in the scientific name string
#'
#' Identifies open nomenclature (aff., cf.) in species scientific name, classification under species level (var. and subsp.), and common mistakes in the a species scientific name. It creates a new column with the original string and the new suggested name. It also flags problematic names (character string with numbers, authors, wrong case, or other names besides genus and epithet etc).
#'
#' @return
#' Data frame with `scientificName` as in the original input, `scientificName_status` with the flags in original data and `scientificName_new` with a suggestion for a more correct species name. See Details for a description of flags in the column `scientificName_status`.
#'
#' @details
#' Possible flags returned in `scientificName_status`:
#'\describe{
#'\item{\code{possibly_ok}}{scientific name following the expected pattern Genus epithet}
#'\item{\code{not_Genus_epithet_format}}{scientific name not following the expected pattern Genus epithet}
#'\item{\code{variety}}{species scientific name with variety}
#'\item{\code{subspecies}}{species scientific name with subspecies}
#'\item{\code{conferre}}{open nomenclature cf. in species scientific name}
#'\item{\code{affinis}}{open nomenclature aff. in species scientific name}
#'\item{\code{name_w_authors}}{species scientific name has authors}
#'\item{\code{not_name_has_digits}}{species scientific name has digits, not a valid name}
#'\item{\code{indet}}{species identified only at genus level}
#'\item{\code{family_as_genus}}{species family as genus, not a valid name}
#'\item{\code{species_nova}}{species name contains an indication of a new species, possibly not yet a valid name}
#'\item{\code{non_ascii}}{species name has non ASCII characters, not a valid name}
#'}
#'
#' @param scientificName species name in a raw format
#' @author Sara Mortara
#'
#' @examples
#' check_status("Lindsaea lancea var. falcata")
#' check_status(c("Lindsaea lancea var. falcata", "Asplenium Aff. truncorum"))
#' check_status("Asplenium sp.")
#'
#' @importFrom stringr str_detect str_replace str_split str_trim
#' @importFrom flora remove.authors fixCase
#' @importFrom stringi stri_enc_mark
#'
#' @export
#'
check_status <- function(scientificName = NULL){

  #1. Open nomenclature and infraspecies class ####
  aff_string <- "^aff\\.|^aff[[:space:]]|[[:space:]]aff\\.|[[:space:]]aff[[:space:]]"
  cf_string <- "^cf\\.|^cf[[:space:]]|[[:space:]]cf\\.|[[:space:]]cf[[:space:]]"
  subsp_string <-  "[[:space:]]ssp\\.|[[:space:]]subsp\\.|[[:space:]]subsp[[:space:]]|[[:space:]]ssp[[:space:]]"
  var_string <- "[[:space:]]var\\.|[[:space:]]var[[:space:]]"
  aff_cf <- paste(aff_string, cf_string, sep = "|")
  subsp_var <- paste(subsp_string, var_string, sep = "|")
  # detecting status
  aff <- stringr::str_detect(scientificName, stringr::regex(aff_string, ignore_case = TRUE))
  cf <- stringr::str_detect(scientificName, stringr::regex(cf_string, ignore_case = TRUE))
  subsp <- stringr::str_detect(scientificName, stringr::regex(subsp_string, ignore_case = TRUE))
  var <- stringr::str_detect(scientificName, stringr::regex(var_string, ignore_case = TRUE))
  check <- data.frame(scientificName = as.character(scientificName))
  # defining status
  check$scientificName_status <- NA
  check$scientificName_status[aff] <- "affinis"
  check$scientificName_status[cf] <- "conferre"
  check$scientificName_status[subsp] <- "subspecies"
  check$scientificName_status[var] <- "variety"
  # accessory functions
  clean_uncertain <- function(x)  {
    x_new <- stringr::str_replace(x, stringr::regex(aff_cf, ignore_case = TRUE), " ")
    x_new <- flora::trim(x_new)
    return(x_new)
  }
  clean_sub <- function(x){
    x_new <- unlist(lapply(stringr::str_split(x, stringr::regex(subsp_var, ignore_case = TRUE)),
                           function(x) x[1]))
    return(x_new)
  }
  # providing cleaned name
  check$scientificName_new <- NA
  ## affinis e conferre
  check$scientificName_new[check$scientificName_status
                           %in% c("affinis", "conferre")] <- clean_uncertain(check$scientificName[check$scientificName_status
                                                                                                  %in% c("affinis", "conferre")])
  check$scientificName_new[check$scientificName_status
                           %in% c("subspecies", "variety")] <- clean_sub(check$scientificName[check$scientificName_status
                                                                                              %in% c("subspecies", "variety")])
  # other types of basic cleaning
  ## first filling scientificName_new for all
  check$scientificName_new <- ifelse(is.na(check$scientificName_new),
                                     as.character(check$scientificName),
                                     check$scientificName_new)

  # definindo prevalencia
  prev <- c("affinis", "conferre", "subspecies", "variety", "indet")

  #2. sp. or genus only ####
  indet_regex <- "[[:space:]]sp\\.$|[[:space:]]sp$|[[:space:]]sp\\.|[[:space:]]indet\\.|[[:space:]]ind\\.|\\ssp\\s"
  no_sp <- sapply(stringr::str_split(check$scientificName_new, " "),
                  length) < 2
  indet <- stringr::str_detect(check$scientificName,
                               stringr::regex(indet_regex,
                                              ignore_case = TRUE))
  check$scientificName_status[no_sp | indet] <- "indet"

  #3. recognizig authors ####
  no_authors <- sapply(check$scientificName_new, flora::remove.authors)
  # aqui aff cf subsp var e indet prevalescem
  id_authors <- is.na(check$scientificName_status) & check$scientificName_new != no_authors &
    sapply(strsplit(as.character(check$scientificName), " "), length) > 2 &
    !check$scientificName_status %in% prev
  check$scientificName_status[id_authors] <- "name_w_authors"
  check$scientificName_new[id_authors] <- no_authors[id_authors]

  #4. recognizig digits ####
  id_digits <- stringr::str_detect(check$scientificName, '\\d') &
    !check$scientificName_status %in% prev
  check$scientificName_status[id_digits] <- "not_name_has_digits"

  #5. sp. nov.####
  #sp. nov., spec. nov., sp. n., nov. sp., nov. spec. or n. sp.
  spnov_regex <- "\\ssp\\.\\snov\\.|\\sspec\\.\\snov\\.|\\ssp\\.\\sn\\.|\\snov\\.\\ssp\\.
  |\\snov\\.\\sspec\\.|\\sn\\.\\sp\\."
  spnov <- stringr::str_detect(check$scientificName,
                               stringr::regex(spnov_regex,
                                              ignore_case = TRUE))
  check$scientificName_status[spnov] <- "species_nova"

  #6. names not matching Genus + species pattern
  # de novo incluir prevalencia
  id_not_gensp <- sapply(stringr::str_split(check$scientificName_new, " "),
                         length) > 2 &
    !check$scientificName_status %in% c(prev, "species_nova")
  check$scientificName_status[id_not_gensp] <- "not_Genus_epithet_format"

  #7. case ####
  case <- sapply(check$scientificName_new, flora::fixCase)
  # aff cf subsp var e indet prevalescem
  id_case <- check$scientificName_new != case &
    !check$scientificName_status %in% prev
  check$scientificName_status[id_case] <- "name_w_wrong_case"
  check$scientificName_new[id_case] <- case[id_case]

  #6. aceae in first string ####
  gen <- sapply(stringr::str_split(check$scientificName_new, " "),
                function(x) x[1])
  id_gen <- endsWith(gen, "aceae")
  check$scientificName_status[id_gen] <- "family_as_genus"

  #8. possibly ok ####
  check$scientificName_status[is.na(check$scientificName_status)] <- "possibly_ok"

  #9. non-ascii ####
  string_type <- stringi::stri_enc_mark(check$scientificName_new)
  check$scientificName_status[check$scientificName_status == "possibly_ok"
                              & string_type != "ASCII"] <- "name_w_non_ascii"

  return(check)
}
