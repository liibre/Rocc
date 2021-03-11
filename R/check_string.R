#' Check in the scientific name string
#'
#' Identifies open nomenclature (aff., cf.) in species scientific name, classification under species level (var. and subsp.), and common mistakes in the a species scientific name. It creates a new column with the original string and the new suggested name. It also flags problematic names (character string with numbers, authors, wrong case, or other names besides genus and epithet etc).
#'
#' @return
#' Data frame with `verbatimSpecies` as in the original input, `speciesStatus` with the flags in original data and `species` with a suggestion for a more correct species name. See Details for a description of flags in the column `speciesStatus`.
#'
#' @details
#' Possible flags returned in `species_status`:
#'\describe{
#'\item{\code{possibly_ok}}{scientific name following the expected pattern Genus epithet}
#'\item{\code{not_Genus_epithet_format}}{scientific name not following the expected pattern Genus epithet}
#'\item{\code{variety}}{species scientific name with variety}
#'\item{\code{subspecies}}{species scientific name with subspecies}
#'\item{\code{form}}{species scientific name with form}
#'\item{\code{conferre}}{open nomenclature cf. in species scientific name}
#'\item{\code{affinis}}{open nomenclature aff. in species scientific name}
#'\item{\code{group}}{open nomenclature gr. in species scientific name}
#'\item{\code{name_w_authors}}{species scientific name has authors}
#'\item{\code{not_name_has_digits}}{species scientific name has digits, not a valid name}
#'\item{\code{indet}}{species identified only at genus level}
#'\item{\code{family_as_genus}}{family as genus, not a valid name}
#'\item{\code{order_as_genus}}{order as genus, not a valid name}
#'\item{\code{species_nova}}{species name contains an indication of a new species, possibly not yet a valid name}
#'\item{\code{non_ascii}}{species name has non ASCII characters, not a valid name}
#'#'\item{\code{hybrid_species}}{hybrid species}
#'}
#'
#' @param species species name in a raw format
#' @param drop_infra Logical. Either to remove any infraspecies classification. Defalts to FALSE
#' @param drop_author Logial. Either to remove authors in species name. Defaults to TRUE
#'
#' @examples
#' check_string("Lindsaea lancea var. falcata")
#' check_string(c("Lindsaea lancea var. falcata", "Asplenium Aff. truncorum"))
#' check_string("Asplenium sp.")
#'
#' @importFrom stringr str_detect str_replace str_split str_trim
#' @importFrom flora remove.authors fixCase trim
#' @importFrom stringi stri_enc_mark
#'
#' @export
#'
#' @author Sara Mortara & Andrea Sánchez-Tapia
#'
check_string <- function(species = NULL,
                         drop_infra = FALSE){

  # ö: implement status parasite "f. sp." not f. from forma

  # 0. cleaning undesired spacies ----------------------------------------------
  species <- trimws(species, whitespace = "[ \\t\\r\\n\U00A0]")
  species <- gsub("\\t|\\r|\\n|\U00A0", " ", species)

  # 1. Open nomenclature and infraspecies class ####
  gr_string <- "[[:space:]]gr\\.[[:space:]]|[[:space:]]gr[[:space:]]"
  form_string <- "[[:space:]]f\\.[[:space:]]sp\\.[[:space:]]|[[:space:]]f\\.[[:space:]]|[[:space:]]form\\.[[:space:]]"
  inc_string <- "inc\\.[[:space:]]sed\\.|Incertae[[:space:]]sedis"
  aff_string <- "^aff\\.|^aff[[:space:]]|[[:space:]]aff\\.|[[:space:]]aff[[:space:]]|aff\\."
  cf_string <- "^cf\\.|^cf[[:space:]]|[[:space:]]cf\\.|[[:space:]]cf[[:space:]]|cf\\."
  subsp_string <-  "[[:space:]]ssp\\.|[[:space:]]subsp\\.|[[:space:]]subsp[[:space:]]|[[:space:]]ssp[[:space:]]|subsp\\."
  var_string <- "[[:space:]]var\\.|[[:space:]]var[[:space:]]|f\\.[[:space:]]var[[:space:]]|var\\."
  aff_cf_gr <- paste(aff_string, cf_string, gr_string, sep = "|")
  subsp_var <- paste(subsp_string, var_string, form_string, sep = "|")

  # detecting status
  gr <- stringr::str_detect(species, stringr::regex(gr_string, ignore_case = TRUE))
  aff <- stringr::str_detect(species, stringr::regex(aff_string, ignore_case = TRUE))
  cf <- stringr::str_detect(species, stringr::regex(cf_string, ignore_case = TRUE))
  subsp <- stringr::str_detect(species, stringr::regex(subsp_string, ignore_case = TRUE))
  var <- stringr::str_detect(species, stringr::regex(var_string, ignore_case = TRUE))
  inc <- stringr::str_detect(species, stringr::regex(inc_string, ignore_case = TRUE))
  form <- stringr::str_detect(species, stringr::regex(form_string, ignore_case = FALSE))
  check <- data.frame(species = as.character(species))

  # defining status
  check$species_status <- NA
  check$species_status[gr] <- "group"
  check$species_status[aff] <- "affinis"
  check$species_status[cf] <- "conferre"
  check$species_status[subsp] <- "subspecies"
  check$species_status[inc] <- "incertae_sedis"
  check$species_status[form] <- "forma"
  check$species_status[var] <- "variety"

  # accessory functions
  remove_authors <- function(x) {
    x2 = flora::remove.authors(x)
    x3 = gsub("f\\.", "", x2)
    return(x3)
  }

  clean_open <- function(x)  {
    x_new <- stringr::str_replace(x, stringr::regex(aff_cf_gr, ignore_case = TRUE), " ")
    x_new <- flora::trim(x_new)
    x_new_woa <- sapply(x_new, remove_authors)
    author_name <- x_new != x_new_woa
    return(c(species = x_new_woa, author_name = author_name))
  }

  clean_infra <- function(x, drop_infra){

    check_infra <- x$species_status
    check_string <- NA
    infra_string <- NA

    check_string[check_infra %in% "subspecies"] <- subsp_string
    infra_string[check_infra %in% "subspecies"] <- "subsp."

    check_string[check_infra %in% "forma"] <- form_string
    infra_string[check_infra %in% "forma"] <- "f."

    check_string[check_infra %in% "variety"] <- var_string
    infra_string[check_infra %in% "variety"] <- "var."

    x_list <- stringr::str_split(x$species, stringr::regex(check_string, ignore_case = TRUE))
    x_new <-  unlist(lapply(x_list, function(x) x[1]))
    x2_new <- unlist(lapply(x_list, function(x) x[2]))
    # Creating new names w/o author names
    x_new_woa <- sapply(x_new, remove_authors)
    x2_new_woa <- sapply(x2_new, remove_authors)
    # Creating string to detect if author name was removed
    author_name <- x_new != x_new_woa | x2_new != x2_new_woa
    if (drop_infra) {
      infra_new <- paste(x_new_woa)
    } else {
      infra_new <- paste(x_new_woa, infra_string, x2_new_woa)
    }
    return(c(species = as.character(infra_new),
             author_name = author_name))
  }

  # providing cleaned name
  check$species_new <- NA
  # adding column to inform if author name was retained
  check$remove_author <- NA

  ## affinis, conferre, group
  open_status <- c("affinis", "conferre", "group")
  replace_cols <- c("species_new", "remove_author")

  check[check$species_status %in% open_status, replace_cols] <- clean_open(
    x = check$species[check$species_status %in% open_status]
  )

  # infraspecies
  infra_status <- c("subspecies", "variety", "forma")

  check[check$species_status %in% infra_status, replace_cols] <- clean_infra(
    x = check[check$species_status %in% infra_status, c("species", "species_status")],
    drop_infra = drop_infra
  )

  # other types of basic cleaning
  ## first filling species_new for all
  check$species_new <- ifelse(is.na(check$species_new),
                              as.character(check$species),
                              check$species_new)

  # Defining status to prevail
  prev <- c(open_status, infra_status, "incertae_sedis", "species_nova", "indet")

  # 2. sp. nov. ----------------------------------------------------------------
  # sp. nov., spec. nov., sp. n., nov. sp., nov. spec. or n. sp.
  spnov_regex <- "\\ssp\\.\\snov\\.|\\sspec\\.\\snov\\.|\\ssp\\.\\sn\\.|\\snov\\.\\ssp\\.
  |\\snov\\.\\sspec\\.|\\sn\\.\\sp\\."
  spnov <- stringr::str_detect(check$species,
                               stringr::regex(spnov_regex,
                                              ignore_case = TRUE))
  spnov_new <- gsub(spnov_regex, "", check$species[spnov])
  spnov_new_woa <- sapply(spnov_new, remove_authors)
  author_spnov <- spnov_new !=  spnov_new_woa
  check$species_status[spnov] <- "species_nova"
  check$remove_author[spnov] <- author_spnov
  check$species_new[spnov] <- spnov_new_woa

  # 3. recognizing authors -----------------------------------------------------
  no_authors <- sapply(check$species_new,
                       function(x) flora::remove.authors(flora::fixCase(x)))

  id_authors <- check$species_new != no_authors &
    sapply(strsplit(as.character(check$species), " "), length) > 2

  id_authors <- id_authors & !check$species_status %in% prev |
    id_authors & sapply(strsplit(as.character(no_authors), " "), length) > 2 |
    sapply(strsplit(as.character(no_authors), " "), length) == 1 # genus + author

  # removing f. in the end of author name
  no_authors <- flora::trim(gsub("f\\.$", "", no_authors))
  no_authors <- ifelse(sapply(stringr::str_split(no_authors, " "), length) > 2,
                       sapply(stringr::str_split(no_authors, " "), function(x) paste(x[1], x[2])),
                       no_authors)

  check$species_status[id_authors & !check$species_status %in% c(open_status, infra_status)] <- "possibly_ok"
  check$species_new[id_authors] <- no_authors[id_authors]
  # skipping infraspecific, open nomenclature or sp nova status (authors already removed)
  check$remove_author[!check$species_status %in% c(infra_status, open_status, "species_nova")] <- id_authors[!check$species_status %in% c(infra_status, open_status, "species_nova")]

  # 4. recognizig digits -------------------------------------------------------
  id_digits <- stringr::str_detect(check$species, '\\d') &
    !check$species_status %in% prev
  digits_new <- ifelse(id_digits, trimws(gsub("\\d", "", check$species)),
                       check$species)[id_digits]
  digits_woa <- sapply(digits_new, remove_authors)
  check$remove_author[id_digits] <- digits_woa != digits_new
  check$species_new[id_digits] <- digits_woa
  check$species_status[id_digits] <- "not_name_has_digits"

  # 5. Indet: sp. or genus only ------------------------------------------------
  indet_regex <- "[[:space:]]sp\\.$|[[:space:]]sp$|[[:space:]]sp\\.|[[:space:]]indet\\.|[[:space:]]ind\\.|[[:space:]]sp[[:space:]]"
  indet_split <- stringr::str_split(check$species_new, " ")
  no_sp <- sapply(indet_split, length) < 2
  indet <- stringr::str_detect(check$species,
                               stringr::regex(indet_regex,
                                              ignore_case = TRUE)) &
    !check$species_status %in% prev
  question <- stringr::str_detect(check$species, "\\?")
  check$species_status[no_sp | indet | question] <- "indet"
  check$species_new[no_sp | indet | question] <- paste(sapply(indet_split[no_sp | indet | question],
                                                              function(x) x[1]), "sp.")
  indet_new <- gsub(paste0(indet_regex, "|\\?"), "",
                    check$species[no_sp | indet | question])
  indet_new_woa <- sapply(indet_new, remove_authors)
  check$remove_author[no_sp | indet | question] <- indet_new != indet_new_woa


  # 6. names not matching Genus + species pattern ------------------------------
  id_not_gensp <- sapply(stringr::str_split(check$species_new, " "),
                         length) > 2 &
    !check$species_status %in% c(prev, "species_nova")
  check$species_status[id_not_gensp] <- "not_Genus_epithet_format"

  # 7. case --------------------------------------------------------------------
  case <- sapply(check$species_new, flora::fixCase)
  # aff cf subsp var e indet prevalescem
  id_case <- check$species_new != case &
    !check$species_status %in% c(prev, "incertae_sedis")
  check$species_status[id_case] <- "name_w_wrong_case"
  check$species_new[id_case] <- case[id_case]

  # 8. aceae in first string ---------------------------------------------------
  gen <- sapply(stringr::str_split(check$species_new, " "),
                function(x) x[1])
  id_gen <- endsWith(gen, "aceae")
  check$species_status[id_gen] <- "family_as_genus"

  # 9. order as genus ----------------------------------------------------------
  ord <- sapply(stringr::str_split(check$species_new, " "),
                function(x) x[1])
  id_ord <- endsWith(gen, "ales")
  check$species_status[id_ord] <- "order_as_genus"

  # 10. hybrid -----------------------------------------------------------------
  hybrid_symbol <- str_detect(check$species, "\u00D7")
  hybrid_string <- "[[:space:]]x[[:space:]]|[[:space:]]X[[:space:]]"
  hybrid_x <- stringr::str_detect(check$species,
                                  stringr::regex(hybrid_string, ignore_case = TRUE))
  hybrid <- hybrid_symbol | hybrid_x
  check$species_status[hybrid] <- "hybrid_species"
  hybrid_new <- as.character(check$species)
  hybrid_new <- gsub(hybrid_string, paste0(" ", "\u00D7"), hybrid_new)

  hybrid_new_woa <- ifelse(sapply(strsplit(hybrid_new, " "), length) > 2,
                           sapply(hybrid_new, remove.authors),
                           hybrid_new)
  author_hybrid <- hybrid_new != hybrid_new_woa

  check$species_new[hybrid] <- hybrid_new_woa[hybrid]
  check$remove_author[hybrid] <- author_hybrid[hybrid]

  # 11 abreviated genus --------------------------------------------------------
  genus <- sapply(str_split(check$species_new, " "), function(x) x[1])
  abbrev_gen <- gsub("\\.", "", genus)
  char_gen <- nchar(abbrev_gen)
  abbrev_gen <- char_gen == 1
  check$species_status[abbrev_gen] <- "abbreviated_genus"

  # 12. possibly ok ------------------------------------------------------------
  check$species_status[is.na(check$species_status)] <- "possibly_ok"

  # 13. non-ascii --------------------------------------------------------------
  string_type <- stringi::stri_enc_mark(check$species_new)
  check$species_status[check$species_status %in% c("possibly_ok", "name_w_wrong_case",
                                                   "subspecies", "variety", "forma")
                       & string_type != "ASCII"] <- "name_w_non_ascii"

  # Standardizing nomenclature style
  names(check)[names(check) == "species"] <- "verbatimSpecies"
  names(check)[names(check) == "species_new"] <- "species"
  names(check)[names(check) == "species_status"] <- "speciesStatus"

  return(check)

}
