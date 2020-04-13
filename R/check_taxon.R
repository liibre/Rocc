#' Function to check species scientific name in Brazilian Flora 2020 database
#'
#' @param scientificName a species scientific name without authors, ideally already passed by the string check in `rocc::check_status`. Accepts only one name per time. Use `lapply` ou functions from `purr` package to run for multiple species.
#'
#' @return
#' A list with one or two elements. If a species has synonyms the second element of the list contains the names and additional information of synonyms.
#'
#' @importFrom flora trim suggest.names
#'
#' @export
#'
#' @examples
#' # single species
#' check_taxon("Dalbergia nigra")
#'
#' # more than one species w/ lapply from base
#' lapply(c("Darbergia nigra", "Aspidosperma discolor"),
#'        check_taxon)
#'
check_taxon <- function(scientificName) {

  # limpeza de espacos
  trim_sp <- flora::trim(scientificName)
  # limpeza de digitacao
  suggest_sp <- sapply(trim_sp, flora::suggest.names)

  # função para buscar na flora do brasil
  search_flora <- function(x){
    api <- "http://servicos.jbrj.gov.br/flora/taxon/"
    search_sp <- gsub(" ", "%20", x)
    res <- jsonlite::fromJSON(paste0(api, search_sp))
    return(res)
  }

  # fazendo a busca
  res <- search_flora(suggest_sp)

  # tem na flora?
  success <- res$success

  # output completo
  out <- res$result[, names(res$result) != "SINONIMO"]

  #sinonimo
  synonym <- res$result$SINONIMO[[1]]

  # acrescentando coluna em out com sinonimo
  out$synonym <- ifelse(!is.null(synonym),
                        TRUE,
                        FALSE)
  # acrescentando coluna com nome original
  out$scientificName_original <- scientificName
  if (success) {
    res <- list(taxon = out,
                synonym = synonym)
  } else {
    res <- list(taxon = "species scientific name not found in Brazilian Flora 2020")
  }
  return(res)
}
