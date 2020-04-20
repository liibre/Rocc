#' Function suggest a valid species name for Brazilian plants
#'
#' @param scientificName Character. A species scientific name without authors, ideally already passed by the string check in `rocc::check_status`
#'
#' @return
#' A data frame with the name provided by the user and a new suggested name
#'
#' @importFrom flora trim suggest.names
#'
#' @export
#'
suggest_bfg <- function(scientificName) {
  # cleaning spaces
  trim_sp <- flora::trim(scientificName)
  # suggesting a name based using flora pkg
  suggest_sp <- sapply(trim_sp, flora::suggest.names)
  df <- data.frame(scientificName = scientificName,
                   scientificName_suggest = as.character(suggest_sp))
  return(df)
}
