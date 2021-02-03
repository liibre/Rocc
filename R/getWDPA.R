#' Function to download WDPA data for a country
#'
#' This function downloads the WDPA shapefile for a country (www.protectedplanet.net).
#' If the file is already on disk it will not perform the download.
#'
#' @param cod Three letter ISO code for the country.
#' @param destfolder The destination folder
#' @param unzip Unzip the downloaded files? Defaults to TRUE
#' @param ... Options for download.file()
#'
#' @details https://www.protectedplanet.net
#' @importFrom utils download.file
#' @export
#'
#' @author Andrea Sánchez-Tapia & Sara Mortara
#'
#' @examples
#' \dontrun{
#' getWDPA(cod = "BRA", unzip = TRUE)
#' }
#'
getWDPA <- function(cod,
                    destfolder = "WDPA",
                    unzip = TRUE,
                    ...) {
  out <- tryCatch(
    {
      message(paste("Downloading", cod, "protected areas data"))
      file <- paste0(cod, "_WDPA.zip")
      if (!file.exists(destfolder)) dir.create(destfolder)
      this <- fs::path(destfolder, file)
      if (file.exists(this)) {
        message("File already exists")
      } else {
        url <- paste0("http://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_", cod, "_shp.zip")
        download.file(url,
                      destfile = this, ...)
        message(paste("Downloading", cod, "WDPA data", "OK"))
      }
      if (unzip) utils::unzip(this, exdir = destfolder)
    },
    error = function(e) {
      message(paste("URL does not seem to exist:", url))
      message("Original error message:")
      message(e)
    },
    warning = function(w) {
      message(paste("URL caused a warning:", url))
      message("Original warning message:")
      message(w)
    },
    finally = {
      message("deleting empty files") #check and delete empty files
      file.inf <- list.files(destfolder, full.names = TRUE)
      empty <- file.inf[file.info(file.inf)[["size"]] == 0]
      unlink(empty)
    }
  )
  return(out)
}

