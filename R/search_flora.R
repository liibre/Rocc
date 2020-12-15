#' Searches in the List of Species of the Brazilian Flora 2020 database
#'
#' Returns species scientific name lists from phytogeographic domain,
#' state, life form and/or endemism status from the List of Species of the Brazilian Flora
#' 2020 database
#'
#' @param domain a domain in Mata Atlântica, Cerrado, Pantanal, Pampa, Amazônia
#' @param stateProvince Two-letter code for Brazilian states (e.g. "MA")
#' @param endemism logical, return species that are endemic or not from Brazil
#'  Defaults to NULL to return all species
#' @param life_form character, search species with the following life forms
#' @param habitat character, search species in the habitat options according to the FB2020
#' @param vegetation_type character, filter by vegetation type according to the FB2020
#' @param force_update Logical. Update the flora IPT database in cache?
#' @return a data frame with taxon IDs, scientific name and binomial name
#' without authors
#'
#' @importFrom finch dwca_read dwca_cache
#' @importFrom stringr str_detect regex
#' @importFrom utils globalVariables
#' @importFrom textclean replace_non_ascii
#'
#' @export
#'
#' @author Andrea Sánchez-Tapia & Sara Mortara
#'
#' @examples
#' # All endemic species from the caatinga in Pernambuco
#' \dontrun{
#' search_flora(domain = "Caatinga",
#'              state = "PE",
#'              endemism = TRUE)
#'}
#'
#'
search_flora <- function(domain = NULL,
                         stateProvince = NULL,
                         life_form = NULL,
                         habitat = NULL,
                         vegetation_type = NULL,
                         endemism = NULL,
                         force_update = FALSE) {
  ipt_flora <- update_flora(force_update = force_update)

    if (!is.null(domain)) {
    domain <- tolower(textclean::replace_non_ascii(domain))
    #if (domain %in% biomas) {
    distribution <- ipt_flora$data$distribution.txt
    regex_domain <- paste(domain, collapse = "|")
    temp_df <- distribution[
      stringr::str_detect(string = tolower(textclean::replace_non_ascii(distribution$occurrenceRemarks)),
                          pattern = stringr::regex(regex_domain)),]
    id_d <- unique(temp_df$id)
    #}
  }

  if (!is.null(stateProvince)) {
    if (!exists("distribution"))
      distribution <- ipt_flora$data$distribution.txt
    states_regex <- paste(tolower(textclean::replace_non_ascii(stateProvince)), collapse = "|")
    temp_df <- distribution[
      stringr::str_detect(string = tolower(textclean::replace_non_ascii(distribution$locationID)),
                          pattern = stringr::regex(states_regex)), ]
    id_e <- unique(temp_df$id)
  }

  if (!is.null(endemism)) {
    if (!exists("distribution"))
    distribution <- ipt_flora$data$distribution.txt
    endemism_regex <- ifelse(endemism == TRUE, "Endemica", "N\u00e3o endemica")
    temp_df <- distribution[
      stringr::str_detect(string = distribution$occurrenceRemarks,
                          pattern = stringr::regex(endemism_regex)), ]
    id_end <- unique(temp_df$id)
  }
  # lifeform and habitat----

  if (!is.null(life_form) | !is.null(habitat) | !is.null(vegetation_type)) {
    #if (any(life_form %in% forma_de_vida == FALSE))
     # stop("one or more life forms not recognized")
    #ast ia filtrar assim mas fica fixo demais, melhor a pessoa fazer um filtro que não dê em nada sem forçar um erro.
    if (!exists("speciesprofile"))
      speciesprofile <- ipt_flora$data$speciesprofile.txt
  }
  if (!is.null(life_form)) {
    lf_regex <- paste(tolower(textclean::replace_non_ascii(life_form)), collapse = "|")
    temp_df <- speciesprofile[
      stringr::str_detect(string = tolower(textclean::replace_non_ascii(speciesprofile$lifeForm)),
                          pattern = stringr::regex(lf_regex)), ]
    id_lf <- unique(temp_df$id)
  }
  if (!is.null(habitat)) {
    hab_regex <- paste(tolower(textclean::replace_non_ascii(habitat)), collapse = "|")
    temp_df <- speciesprofile[
      stringr::str_detect(string = tolower(textclean::replace_non_ascii(speciesprofile$lifeForm)),
                          pattern = stringr::regex(hab_regex)), ]
    id_hab <- unique(temp_df$id)
    }
  if (!is.null(vegetation_type)) {
    veg_regex <- paste(tolower(textclean::replace_non_ascii(vegetation_type)), collapse = "|")
    temp_df <- speciesprofile[
      stringr::str_detect(string = tolower(textclean::replace_non_ascii(speciesprofile$lifeForm)),
                          pattern = stringr::regex(veg_regex)), ]
    id_veg <- unique(temp_df$id)
    }

  ## intersects ids
  #Bioma e estado ou só bioma ou só estado

  # if subsetting both by state and domain
  if (!is.null(domain)) ids <- id_d                 # só bioma
  if (!is.null(stateProvince))  ids <- id_e         # só estado
  if (!is.null(domain) & !is.null(stateProvince)) { # ambas: intersecao
    ids <- intersect(id_d, id_e)
  }

  if (!is.null(endemism)) {
    if (!exists("ids")) ids <- id_end
    ids <- intersect(ids, id_end)
  }
  if (!is.null(life_form)) {
    if (!exists("ids")) ids <- id_lf
    ids <- intersect(ids, id_lf)
  }
  if (!is.null(habitat)) {
    if (!exists("ids")) ids <- id_hab
    ids <- intersect(ids, id_hab)
  }
  if (!is.null(vegetation_type)) {
    if (!exists("ids")) ids <- id_veg
    ids <- intersect(ids, id_veg)
  }

  #regex_ids <- paste(ids, collapse = "|")


  # get names----
  taxon <- ipt_flora$data$taxon.txt
  taxon <- subset(taxon, taxon$taxonRank %in% c("ESPECIE",
                                                "VARIEDADE",
                                                "SUB_ESPECIE",
                                                "FORMA"))
  taxon_df <-
    taxon[taxon$id %in% ids,
          c("id", "scientificName", "scientificNameAuthorship",
            "genus", "specificEpithet")]
  taxon_df$names_wo_author <- paste(taxon_df$genus, taxon_df$specificEpithet)
  taxon_df <- taxon_df[, c("id", "scientificName", "names_wo_author")]
  return(taxon_df)
}
