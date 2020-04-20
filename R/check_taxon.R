#' Function to check species scientific name in Brazilian Flora 2020 database
#'
#' @param scientificName Character. A species scientific name without authors, ideally already passed by the string check in `rocc::check_status`. Accepts only one name per time. Use `lapply` ou functions from `purr` package to run for multiple species
#' @param get_synonyms Logical. If `get_synonyms = TRUE` (default) returns a second element containing information of all synonyms a species has
#' @param infraspecies Logical. If `infraspecies = TRUE` returns accepted name of any infraspecies classification
#'
#' @return
#' A list with one or two elements. If `get_synonyms = TRUE` the second element of the list contains the names and additional information of synonyms or a NULL element if the species has no synonyms
#'
#' @importFrom flora trim suggest.names
#'
#' @export
#'
#' @examples
#' # single species
#' check_bfg("Dalbergia nigra")
#'
#' # more than one species w/ lapply from base
#' lapply(c("Darbergia nigra", "Aspidosperma discolor"),
#'        check_bfg)
#'
check_bfg <- function(scientificName,
                      get_synonyms = TRUE,
                      infraspecies = FALSE) {

    # função para buscar na flora do brasil
  search_flora <- function(x){
    api <- "http://servicos.jbrj.gov.br/flora/taxon/"
    search_sp <- gsub(" ", "%20", x)
    res <- jsonlite::fromJSON(paste0(api, search_sp))
    return(res)
  }

  # fazendo a busca
  res <- search_flora(scientificName)
  # tem na flora?
  success <- res$success

  # output taxon ####
  cols_remove <- c("SINONIMO", "NOME ACEITO",
                   "higherclassification", "source",
                   "references")
  out <- res$result[, !names(res$result) %in% cols_remove]
  # output synonym
  synonyms <- res$result$SINONIMO[[1]]
  # acrescentando coluna em out com sinonimo
  out$synonyms <- ifelse(!is.null(synonyms$taxonid),
                        TRUE,
                        FALSE)
  # acrescentando coluna com nome original da busca
  out$scientificName_search <- scientificName
  out <- out[out$taxonomicstatus %in% "NOME_ACEITO", ]
  if (infraspecies == FALSE) {
    out <- out[is.na(out$infraspecificepithet), ]
  } else {
    out <- out
  }

  # output sinonimo ####
  # criando coluna com o basinômio em synonyms
  if (!is.null(synonyms$taxonid)) {
    syn_remove <- c("higherclassification",
                    "source",
                    "references")
    synonyms <- synonyms[, !names(synonyms) %in% syn_remove]
    synonyms_base <- out[out$taxonomicstatus == "NOME_ACEITO"
                         & is.na(out$infraspecificepithet),
                         c('taxonid', 'scientificname')]
    names(synonyms_base) <- c("taxonid_base", "scientificname_base")
    # juntando a info do basinomio com o output de synonyms
    synonyms <- cbind(synonyms, synonyms_base)
  }
  # gerando o output
  if (get_synonyms) {
    res <- list(taxon = out,
                synonyms = synonyms)
  } else {
    res <- list(taxon = out)
  }
  if (success) {
    res <- res
  } else {
    message("species scientific name not found in Brazilian Flora 2020")
    res <- NULL
  }
  return(res)
}
