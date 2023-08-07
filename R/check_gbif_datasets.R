#' Access 'gbif' dataset information
#'
#' \code{check_gbif_datasets} searches for dataset information from \href{https://www.gbif.org}{gbif}.
#' @usage check_gbif_datasets()
#' @param path Character that defines the location for the downloaded file. By default is downloaded to a temporary directory (\code{tempdir()}).
#' @return returns dataset csv from gbif
#' @export
#' @name check_gbif_datasets
#' @details This function queries for dataset info in the open-access
#' online repository \href{https://www.gbif.org}{gbif}. It saves a csv file in a temporary directory (~42 MB). The file contains some 89000 rows of datasets to explore.
#' @seealso \code{\link{check_gbif_datasets}}, \code{\link{query_observation}}, \code{\link{query_wikiaves}}, \code{\link{query_inaturalist}},
#' @examples
#' \dontrun{
#' # download dataset information
#' check_gbif_datasets()
#' }
#'
#' @references {
#'
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

check_gbif_datasets <- function(path = tempdir()) {
  # check internet connection
  a <- try(RCurl::getURL("https://www.gbif.org"), silent = TRUE)
  if (is(a, "try-error")) {
    stop2("No connection to gbif (check your internet connection!)")
  }

  if (a == "Could not connect to the database") {
    stop2("GBIF website is apparently down")
  }

  # Read the CSV data
  message("Downloading csv file ...")
  csv_data <- try(read.csv("https://api.gbif.org/v1/dataset/search/export?format=CSV&"), silent = TRUE)

  on.exit(rm(csv_data))

  if (is(csv_data, "try-error")) {
    stop2("Failed to download CSV.")
  } else {
    write.csv(csv_data, file.path(path, paste0("gbif_datasets-", Sys.time(), ".csv")))
  }

  # tell users were is the file and allow them to open it
  # cli::cli_text("... edit your {.file ~/.Rprofile} file.}")
  cli::cli_text(paste0("The csv file (", paste0("gbif_datasets-", Sys.time(), ".csv"), ") with the gbif datasets info can be found at ", path))
}
