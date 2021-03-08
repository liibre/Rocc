#' Downloads, updates and formats the data from the ipt server of the List of Species of the Brazilian Flora
#'
#' @param  force_update Logical. Forces update in case there is already an ipt file in cache
#'
#' @importFrom finch dwca_cache
#' @importFrom finch dwca_read
#'
#' @author Andrea Sánchez-Tapia & Sara Mortara
#'
#' @export
update_flora <- function(force_update = FALSE) {
  cache_path <- finch::dwca_cache$cache_path_get()
  if (force_update) {
    finch::dwca_cache$delete_all()
  }
  pag <- "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
  ipt_flora <- finch::dwca_read(input = pag, read = TRUE, encoding = "UTF-8")
  timestmp <- ipt_flora$emlmeta$dataset$pubDate
  writeLines(paste0("Cache path:", cache_path, "Timestamp:", timestmp),
             "update_flora_metadata.txt")
  return(ipt_flora)
  #distribution <- ipt_flora$data$distribution.txt
  #speciesprofile <- ipt_flora$data$speciesprofile.txt
  #taxon <- ipt_flora$data$taxon.txt
  #usethis::use_data(distribution, speciesprofile, taxon, internal = TRUE)
}
