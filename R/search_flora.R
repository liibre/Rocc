#' Returns species scientific name lists from phytogeographic domain,
#' state and/or endemism status from the List of Species of the Brazilian Flora
#' 2020 database
#'
#' @param domain a domain in Mata Atlântica, Cerrado, Pantanal, Pampa, Amazônia
#' @param states Brazilian states, capitalized (e.g. "MA")
#' @param endemism logical, return species that are endemic or not from Brazil
#'  Defaults to NULL to return all species
#' @param lifeform character, search species with the following life forms:
#' \describe{
#'   \item{\code{Arbusto}}{}
#'   \item{\code{Árvore}}{}
#'   \item{\code{Bambu}}{}
#'   \item{\code{Dracenoide}}{}
#'   \item{\code{Erva}}{}
#'   \item{\code{Liana/volúvel/trepadeira}}{}
#'   \item{\code{Palmeira}}{}
#'   \item{\code{Subarbusto}}{}
#'   \item{\code{Suculenta}}{}
#'   \item{\code{Desconhecida}}{}
#'}
#' @param habitat character, search species in the following habitat options:
#' \describe{
#'   \item{\code{Aquática}}{}
#'   \item{\code{Epífita}}{}
#'   \item{\code{Hemiepífita}}{}
#'   \item{\code{Hemiparasita}}{}
#'   \item{\code{Parasita}}{}
#'   \item{\code{Rupícola}}{}
#'   \item{\code{Saprófita}}{}
#'   \item{\code{Terrícola}}{}
#'   \item{\code{Desconhecido}}{}
#'}
#' @return a data frame with taxon IDs, scientific name and binomial name
#' without authors
#'
#'
#' @importFrom finch dwca_read dwca_cache
#' @importFrom stringr str_detect regex
#' @importFrom utils globalVariables
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
                         habitat = NULL,
                         ...) {
  ipt_flora <- update_flora(...)
  biomas <- c("Amazônia", "Caatinga", "Cerrado", "Mata Atlântica", "Pampa", "Pantanal")
  if (!is.null(domain)) {
    if (domain %in% biomas) {
    distribution <- ipt_flora$data$distribution.txt
    regex_domain <- paste(domain, collapse = "|")
    domain_df <- distribution[
      stringr::str_detect(string = distribution$occurrenceRemarks,
                          pattern = stringr::regex(regex_domain)),]
    id_d <- unique(domain_df$id)
    }
  }

  if (!is.null(states)) {
    distribution <- ipt_flora$data$distribution.txt
    states_regex <- paste(states, collapse = "|")
    states_df <- distribution[
      stringr::str_detect(string = distribution$locationID,
                          pattern = stringr::regex(states_regex)), ]
    id_e <- unique(states_df$id)
  }

  if (!is.null(endemism)) {
    distribution <- ipt_flora$data$distribution.txt
    endemism_regex <- ifelse(endemism == TRUE, "Endemica", "Não endemica")
    endemism_df <- distribution[
      stringr::str_detect(string = distribution$occurrenceRemarks,
                          pattern = stringr::regex(endemism_regex)), ]
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
                          pattern = stringr::regex(lf_regex)), ]
    id_lf <- unique(spprof_df$id)
  }
  if (!is.null(habitat)) {
    hab_regex <- paste(habitat, collapse = "|")
    spprof_df <- speciesprofile[
      stringr::str_detect(string = speciesprofile$habitat,
                          pattern = stringr::regex(hab_regex)), ]
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
