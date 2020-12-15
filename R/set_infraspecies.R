#' Adds infraspecies identification if missing
#'
#' @param subspecies Species name containing three stings: Genus, species epithet, and infraspecies epithet
#' @param type Type of infraspecies classification. Any type in: `"var"` or `"subsp"` or `"form"`.
#'
#' @author Sara Mortara & Andrea Sánchez-Tapia
#'
#' @export
#' @examples
#' set_infraspecies("Lindsaea lancea lancea", type = "var")
#'
set_infraspecies <- function(subspecies,
                              type) {
  types <- data.frame(var = "var.",
                      subsp = "subsp.",
                      form = "form.")
  if (!type %in% names(types)) {
    stop("type must be either 'var', 'subsp' or 'form'")
  }
  infra_cat <- types[names(types) %in% type]
  infrasp <- sapply(strsplit(subspecies, " "), function(x) {paste(x[1], x[2], infra_cat, x[3])})
  return(data.frame(verbatimSpecies = subspecies,
                    subspecies = infrasp))
}
