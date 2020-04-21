#' Returns species scientific name lists from phytogeographic domain,
#' state and/or endemism status from the List of Species of the Brazilian Flora
#' 2020 database
#'
#' @param domain a domain in Mata Atlântica, Cerrado, Pantanal, Pampa, Amazônia
#' @param states Brazilian states, capitalized (e.g. "MA")
#' @param endemism logical, return species that are endemic or not from Brazil
#'  Defaults to NULL to return all species
#'
#' @return a list of taxon IDs
#'
#'
#' @importFrom finch dwca_read dwca_cache
#' @importFrom stringr str_detect regex
#'
#' @export
#'
#' @examples
#' # All endemic species from the caatinga in Pernambuco
#' search_flora(domain = "Caatinga",
#'              state = "PE",
#'              endemism = TRUE)
#'
#'
search_flora <- function(domain = NULL,
                         states = NULL,
                         endemism = NULL,
                         lifeform = NULL,
                         habitat = NULL) {
biomas <- c("Amazônia", "Caatinga", "Cerrado", "Mata Atlântica", "Pampa", "Pantanal")
  pag <- "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
  ipt_flora <- finch::dwca_read(input = pag, read = TRUE, encoding = "UTF-8")
  # distribution ----
  distribution <- ipt_flora$data$distribution.txt

  if (!is.null(domain)) {
    if (domain %in% biomas) {
    regex_domain <- paste(domain, collapse = "|")
    domain_df <- distribution[
      stringr::str_detect(string = distribution$occurrenceRemarks,
                          pattern = stringr::regex(regex_domain)), "id"]
    id_d <- unique(domain_df$id)
    }
  }

  if (!is.null(states)) {
    states_regex <- paste(states, collapse = "|")
    states_df <- distribution[
      stringr::str_detect(string = distribution$locationID,
                          pattern = stringr::regex(states_regex)), "id"]
    id_e <- unique(states_df$id)
  }

  if (!is.null(endemism)) {
    endemism_regex <- ifelse(endemism == TRUE, "Endemica", "Não endemica")
    endemism_df <- distribution[
      stringr::str_detect(string = distribution$occurrenceRemarks,
                          pattern = stringr::regex(endemism_regex)), "id"]
    id_end <- unique(endemism_df$id)
  }

  # lifeform and habitat----
  if (!is.null(lifeform) | !is.null(habitat)) {
    speciesprofile <- ipt_flora$data$speciesprofile.txt
  }
  if (!is.null(lifeform)) {
    lf_regex <- paste(lifeform, collapse = "|")
    spprof_df <- speciesprofile[
      stringr::str_detect(string = speciesprofile$lifeForm,
                          pattern = stringr::regex(lf_regex)), "id"]
    id_lf <- unique(spprof_df$id)
  }
  if (!is.null(habitat)) {
    hab_regex <- paste(habitat, collapse = "|")
    spprof_df <- speciesprofile[
      stringr::str_detect(string = speciesprofile$habitat,
                          pattern = stringr::regex(hab_regex)), "id"]
    id_hab <- unique(spprof_df$id)
    }
## intersects ids
  if (!is.null(domain)) {
    if (domain %in% biomas) {
      ids <- id_d
    }
  }
  if (!is.null(states))  ids <- id_e
  # if subsetting both by state and domain
  if (!is.null(domain) & !is.null(states)) {
    ids <- intersect(id_d, id_e)
  }
  if (!is.null(endemism)) {
    ids <- intersect(ids, id_end)
  }
  if (!is.null(lifeform)) {
    ids <- intersect(ids, id_lf)
  }
  if (!is.null(habitat)) {
    ids <- intersect(ids, id_hab)
  }

  regex_ids <- paste(ids, collapse = "|")


  # get names----
  taxon <- ipt_flora$data$taxon.txt
  taxon <- subset(taxon, taxon$taxonRank %in% c("ESPECIE",
                                                "VARIEDADE",
                                                "SUB_ESPECIE",
                                                "FORMA"))
  ids_df <-
    taxon[stringr::str_detect(string = taxon$id,
                              pattern = regex_ids),
          c("id", "scientificName", "scientificNameAuthorship",
            "genus", "specificEpithet")]
  ids_df$names_wo_author <- paste(ids_df$genus, ids_df$specificEpithet)
  ids_df <- ids_df[, c("id", "scientificName", "names_wo_author")]
  return(ids_df)
}
