#' Function to check species scientific name in Brazilian Flora 2020 database
#'
#' @param species Character. A species scientific name without authors, ideally already passed by the string check in `Rocc::check_status()`. Accepts only one name at a time. Use `lapply()` or functions from `purrr` package to run for multiple species
#' @param get_synonyms Logical. If `get_synonyms = TRUE` (default) returns a second element containing information of all synonyms a species has
#' @param infraspecies Logical. If `infraspecies = TRUE` returns accepted name of any infraspecies classification
#'
#' @return
#' A list with one or two elements. If `get_synonyms = TRUE` the second element of the list contains the names and additional information of synonyms or a `NULL` object if the species has no synonyms.
#'
#' @importFrom flora trim suggest.names
#'
#' @export
#'
#' @author Sara Mortara & Andrea Sánchez-Tapia
#'
#' @examples
#' # single species
#' check_flora("Dalbergia nigra")
#'
#' # more than one species w/ lapply from base
#' lapply(c("Dalbergia nigra", "Aspidosperma discolor"),
#'        check_flora)
#'
check_flora <- function(species,
                        get_synonyms = TRUE,
                        infraspecies = FALSE) {
  # equivalencia de campos flora2020 e DwC
  campos <- c(
    taxonID = "taxonid",
    family = "family",
    genus = "genus",
    scientificName = "scientificname",
    specificEpithet = "specificepithet",
    infraspecificEpithet = "infraspecificepithet",
    scientificNameAuthorship = "scientificnameauthorship",
    taxonomicStatus = "taxonomicstatus",
    acceptedNameUsage = "acceptednameusage",
    acceptedNameUsageID = "acceptednameusageid",
    modified = "modified"
  )
  campos.syn <- c(campos[1:8], higherClassification = "higherclassification",
                  source =  "source", references = "references")
  # função para buscar na flora do brasil
  search_flora <- function(x) {
    api <- "http://servicos.jbrj.gov.br/v2/flora/taxon/"
    search_sp <- gsub(" ", "%20", x)
    res <- jsonlite::fromJSON(paste0(api, search_sp))
    sp <- paste(res$result$genus, res$result$specificepithet)
    cual <- which(sp == x)
    res$result <- res$result[cual,]
    return(res)
  }

  # fazendo a busca
  fail_message <- "Flora 2020 API is out of service"
  res <- tryCatch(search_flora(species), error = function(e) e, finally = print(fail_message))

  # checando se a busca funcionou
  if (!"error" %in% class(res)) {
    #res <- search_flora(species)

    # tem na flora?
    success <- res$success & !is.null(res$result)

    if (success == FALSE) {
      message("species scientific name not found in Brazilian Flora 2020")
      res <- NULL
    } else {
      # output taxon ####
      cols_remove <- c("SINONIMO", "NOME ACEITO",
                       "higherclassification", "source",
                       "references")
      out <- res$result[, !names(res$result) %in% cols_remove]
      # mudando nomes para padrao DwC + recente:
      names(out) <- names(campos)
      # output synonym
      synonyms <- res$result$SINONIMO[[1]]
      # acrescentando coluna em out com sinonimo
      out$synonyms <- ifelse(!is.null(synonyms$taxonid),
                             TRUE,
                             FALSE)
      # acrescentando coluna com nome original da busca
      out$verbatimSpecies <- species
      # guardando se tem nome aceito
      accepted_name <- sum(out$taxonomicStatus == "NOME_ACEITO") >= 1
      # transformando vazio em NA no taxonomicstatus
      out$taxonomicStatus[out$taxonomicStatus == ""] <- NA
      #out <- out[out$taxonomicstatus %in% "NOME_ACEITO", ]
      # creating column w/ scientificName w/o author
      out$species <- paste(out$genus, out$specificEpithet)
      # removing infraspecies information
      if (is.null(out)) {
        out <- out
      } else {
        if (infraspecies == FALSE) {
          out <- out[is.na(out$infraspecificEpithet), ]
        } else {
          out <- out
        }
      }

      # output sinonimo ####
      # criando coluna com o basinômio em synonyms
      if (get_synonyms & accepted_name) {
        if (!is.null(synonyms$taxonid) & accepted_name) {
          # mudando nomes para DwC + recente:
          names(synonyms) <- names(campos.syn)
          syn_remove <- c("higherClassification",
                          "source",
                          "references")
          synonyms <- synonyms[, !names(synonyms) %in% syn_remove]
          synonyms_base <- out[out$taxonomicStatus %in% "NOME_ACEITO"
                               & is.na(out$infraspecificEpithet), ]
          synonyms_base <- synonyms_base[, c('taxonID', 'species')]
          names(synonyms_base) <- c("taxonID_base", "species_base")
          # creating column w/ scientificName w/o author
          syn_species <- paste(synonyms$genus, synonyms$specificEpithet)
          # juntando a info do basinomio com o output de synonyms
          synonyms <- cbind(synonyms, synonym_species = syn_species, synonyms_base)
        } else {
          synonyms <- NULL
        }
        # gerando o output
        res <- list(taxon = out,
                    synonyms = synonyms)
      } else {
        res <- list(taxon = out)
      }
    } # end of success if
    return(res)

  } # end of search test if
}
