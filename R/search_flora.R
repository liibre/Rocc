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
                         endemism = NULL) {

  pag <- "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
  ipt_flora <- finch::dwca_read(input = pag, read = TRUE, encoding = "UTF-8")
  distribution <- ipt_flora$data$distribution.txt

  if (!is.null(domain)) {
    regex_domain <- paste(domain, collapse = "|")
    domain_df <- distribution[
      stringr::str_detect(string = distribution$occurrenceRemarks,
                          pattern = stringr::regex(regex_domain)),]
    id_d <- unique(domain_df$id)
  }

  if (!is.null(states)) {
    states_regex <- paste(states, collapse = "|")
    states_df <- distribution[
      stringr::str_detect(string = distribution$locationID,
                          pattern = stringr::regex(states_regex))
      ,]
    id_e <- unique(states_df$id)
  }

  if (!is.null(endemism)) {
    endemism_regex <- ifelse(endemism == TRUE, "Endemica", "Não endemica")
    endemism_df <- distribution[
      stringr::str_detect(string = distribution$occurrenceRemarks,
                          pattern = stringr::regex(endemism_regex))                          ,]
    id_end <- unique(endemism_df$id)
  }
  if (!is.null(domain)) ids <- id_d
  if (!is.null(states))  ids <- id_e
  #if subsetting both by state and domain
  if (!is.null(domain) & !is.null(states)) {
    ids <- intersect(id_d, id_e)
  }
  if (!is.null(endemism)) {
    ids <- intersect(ids, id_end)
  }

  # get names
  taxon <- ipt_flora$data$taxon.txt
  regex_ids <- paste(ids, collapse = "|")
  ids_df <-
    taxon[stringr::str_detect(string = taxon$id,
                              pattern = regex_ids),
          c("id", "scientificName")]
  return(ids_df)
}
