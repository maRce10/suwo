#' Access 'gbif' dataset information
#'
#' \code{check_gbif_datasets} searches for dataset information from \href{https://www.gbif.org}{gbif}.
#' @param path Directory path where the output .csv file will be saved. By default is downloaded to the current working directory (\code{"."}).
#' @return returns dataset csv from gbif
#' @export
#' @name check_gbif_datasets
#' @details This function queries for dataset info in the open-access
#' online repository \href{https://www.gbif.org}{gbif}. It saves a csv file in a the supplied `path` directory (~42 MB). The file contains some 89000 rows of datasets to explore.
#' @seealso \code{\link{check_gbif_datasets}}, \code{\link{query_observation}}, \code{\link{query_wikiaves}}, \code{\link{query_inaturalist}},
#' @examples
#' \dontrun{
#' # download dataset information
#' check_gbif_datasets()
#' }
#'
#' @references {
#' GBIF.org (2024), GBIF Home Page. Available from: https://www.gbif.org [13 January 2020].
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

check_gbif_datasets <- function(path = ".") {

  # Use the unified connection checker
  if (!.checkconnection("gbif")) {
    return(invisible(NULL))
  }

  # Download the CSV data
  message("Downloading GBIF dataset metadata ...")
  csv_url <- "https://api.gbif.org/v1/dataset/search/export?format=CSV&"
  # csv_response <- try(httr::GET(csv_url), silent = TRUE)
  file_name <- paste0("gbif_datasets_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
  file_name <- file.path(normalizePath(path), file_name)

  download.file("https://api.gbif.org/v1/dataset/search/export?format=CSV&",destfile = file_name)

  if (!file.exists(file_name)) {
    stop("Failed to download CSV.")
  } else {
    # Read CSV data into R
    csv_data <- try(read.csv(file_name), silent = TRUE)
    if (inherits(csv_data, "try-error")) {
      stop("Failed to GBIF dataset metadata")
    }
  }

  # Inform the user about the file
  cli::cli_text(paste0("The GBIF dataset metadata was save in ", file_name))


  return(csv_data)
}
