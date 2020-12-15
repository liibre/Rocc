#' Function to download GADM data for a country at a specific level
#'
#' This function downloads the GADM shapefile for a country (https://gadm.org/data.html).
#' If the file is already on disk it will not perform the download. The function
#' can check if better resolutions are available in disk.
#'
#' @param cod Three letter ISO code for the country.
#' @param level Administrative level. 0 stands for country and 1 to 4 are the
#'  successive successive nested levels. Not every country has the same depth and
#'  the nestedness is sequential. This means a country with two levels has levels
#'  0 and 1, not 0 and 4, and that units such as states or municipalities can
#'  correspond to different levels depending on the country
#' @param type The class of spatial object to be downloaded. Defaults to "sf"
#'  but can be set to "sp"
#' @param best TRUE Checks if finer resolutions are available and avoids
#'  downloading data at coarser resolutions. Defaults to TRUE (will not download
#'  coarser resolution files)
#' @param ... Options for download.file()
#' @param destfolder The destination folder
#' @details https://gadm.org/data.html
#' @importFrom utils download.file
#' @importFrom fs path
#' @export
#'
#' @author Andrea Sánchez-Tapia & Sara Mortara
#'
#' @examples
#' \dontrun{
#' getGADM(cod = "BRA", level = 0)
#' getGADM(cod = "BRA", level = 1, best = TRUE) # will not download if lower levels exist
#' getGADM(cod = "BRA", level = 3, type = "sp")
#' }
#'
getGADM <- function(cod,
                    level = 4,
                    type = "sf",
                    best = FALSE,
                    destfolder = "GADM",
                    ...) {
  out <- tryCatch(
    {
      message(paste("Downloading", cod, level))
      file <- paste0(cod, "_", level, "_", type, ".rds")
      up   <- paste0(cod, "_", level + 1:4, "_", type, ".rds")
      if (!file.exists(destfolder)) dir.create(destfolder)
      this <- fs::path(destfolder, file)
      upper <- fs::path(destfolder, up)
      if (any(file.exists(upper)) && best) {
        message("A better resolution file already exists")
        } else {
          if (file.exists(this)) {
            message("File already exists")
          } else {
            url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/R",type,"/gadm36_", file)
            download.file(url,
                          destfile = this, ...)
            message(paste("Downloading", cod, level, "..."))
          }
        }
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
      file.inf <- list.files(destfolder, full.names = T)
      empty <- file.inf[file.info(file.inf)[["size"]] == 0]
      if (length(empty) > 0) message("Downloading failed") #check and delete empty files
      unlink(empty)
    }
  )
  return(out)
}
