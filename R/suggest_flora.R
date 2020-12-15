#' Function suggest a valid species name for Brazilian plants
#'
#' @param species Character. A species scientific name without authors, ideally already passed by the string check in `Rocc::check_status()`
#'
#' @return
#' A data frame with the name provided by the user and a new suggested name
#'
#' @importFrom flora trim suggest.names
#'
#' @author Sara Mortara & Andrea Sánchez-Tapia
#'
#' @export
#'
suggest_flora <- function(species) {
  # cleaning spaces
  trim_sp <- flora::trim(species)
  # suggesting a name based using flora pkg
  suggest_sp <- sapply(trim_sp, flora::suggest.names)
  df <- data.frame(verbatimSpecies = species,
                   species = as.character(suggest_sp))
  return(df)
}
