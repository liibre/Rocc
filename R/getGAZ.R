#' Function to download DIVA-GIS gazetteer data for a country
#'
#' This function downloads the DIVA-GIS gazetteer data for a country.
#' If the file is already on disk it will not perform the download.
#'
#' @param cod Three letter ISO code for the country.
#' @param ... Options for download.file()
#' @param destfolder The destination folder
#' @param unzip Unzip the downloaded files? Defaults to TRUE
#' @details https://gadm.org/data.html
#' @importFrom utils download.file unzip
#' @export
#'
#' @author Andrea Sánchez-Tapia & Sara Mortara
#'
#' @examples
#' \dontrun{
#' getGAZ(cod = "COL", unzip = TRUE)
#' countries <- c("COL", "BRA", "ECU", "PER", "VEN", "BOL")
#' lapply(countries, getGAZ)
#' }
#'
getGAZ <- function(cod,
                   destfolder = "GAZ",
                   unzip = TRUE,
                    ...) {
  out <- tryCatch(
    {
      message(paste("Downloading", cod, "gazetteer data"))
      file <- paste0(cod, "_gaz.zip")
      if (!file.exists(destfolder)) dir.create(destfolder)
      this <- fs::path(destfolder, file)
      if (file.exists(this)) {
        message("File already exists")
        } else {
          url <- paste0("https://biogeo.ucdavis.edu/data/diva/gaz/", file)
            download.file(url,
                          destfile = this, ...)
            message(paste("Downloading", cod, "gazetteer data", "OK"))
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
      file.inf <- list.files(destfolder, full.names = T)
      empty <- file.inf[file.info(file.inf)[["size"]] == 0]
      unlink(empty)
    }
  )
  return(out)
}
