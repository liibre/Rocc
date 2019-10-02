#' Gets occurrence data from GBIF
#'
#' This function uses the rgbif package to get returns occurrence data of species.
#'
#' @param dir Path to directory where the file will be saved. Default is to create a "results/" directory
#' @param filename Name of the output file
# @param basisofrecord Character. Any in 'PreservedSpecimen', 'LivingSpecimen', 'FossilSpecimen',
# 'HumanObservation', 'MachineObservation' or 'MaterialSample'. Default is 'PreservedSpecimen' for museum and herbarium search
# @param country
# @param stateProvince
# @param collectionCode
# @param basisOfRecord
# @param hasCoordinate
#' @param scientificname Genus and epithet separated by space
#' @param ... any arguments from occ_search in rgbif package
#' @return A data.frame with the search result. Also saves the output on disk.
#' @author Sara Mortara
#' @examples
#'
#' @importFrom rgbif name_backbone
#' @importFrom rgbif occ_search
#' @importFrom utils write.table
#'
#' @export
rgbif2 <- function(scientificname) {
  key <- rgbif::name_backbone(name = scientificname)$speciesKey
  if (!is.null(key)) {
    gbif_data <- rgbif::occ_search(
      hasCoordinate = TRUE,
      hasGeospatialIssue = F,
      taxonKey = key,
      return = "data", ...
    )
    gbif_data <- subset(gbif_data, !is.na(decimalLongitude) & !is.na(decimalLatitude))
    #occur.data <- data.frame(gbif_data$name, gbif_data$decimalLongitude, gbif_data$decimalLatitude)
    #colnames(occur.data) <- c("name", "lon", "lat")
    return(gbif_data)
  } else {
    showModal(modalDialog(
      title = "No results!",
      paste0("Please insert a valid species scientific name."),
      easyClose = TRUE
  ))
}
}
