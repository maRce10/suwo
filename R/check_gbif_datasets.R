#' Access 'gbif' dataset information
#'
#' \code{check_gbif_datasets} searches for dataset information from \href{https://www.gbif.org}{gbif}.
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
#' GBIF: The Global Biodiversity Information Facility (year) What is GBIF?. Available from https://www.gbif.org/what-is-gbif [13 January 2020].
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

check_gbif_datasets <- function(path = tempdir()) {
  # Check internet connection
  response <- try(httr::GET("https://www.gbif.org"), silent = TRUE)
  if (inherits(response, "try-error") ||
      httr::http_error(response)) {
    stop("No connection to GBIF (check your internet connection!)")
  }

  # Check if GBIF website is down
  content_text <- httr::content(response, "text", encoding = "UTF-8")
  if (grepl("Could not connect to the database", content_text)) {
    stop("GBIF website is apparently down")
  }

  # Download the CSV data
  message("Downloading CSV file ...")
  csv_url <- "https://api.gbif.org/v1/dataset/search/export?format=CSV&"
  csv_response <- try(httr::GET(csv_url), silent = TRUE)

  if (inherits(csv_response, "try-error") ||
      httr::http_error(csv_response)) {
    stop("Failed to download CSV.")
  } else {
    # Read CSV data into R
    csv_data <- try(read.csv(text = httr::content(csv_response, "text", encoding = "UTF-8")), silent = TRUE)
    if (inherits(csv_data, "try-error")) {
      stop("Failed to parse CSV.")
    }

    # Save the CSV file
    file_name <- paste0("gbif_datasets-",
                        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                        ".csv")
    file_path <- file.path(path, file_name)
    write.csv(csv_data, file_path)
  }

  # Inform the user about the file
  cli::cli_text("... edit your {.file ~/.Rprofile} file.}")
  cli::cli_text(paste0(
    "The CSV file (",
    file_name,
    ") with the GBIF datasets info can be found at ",
    path
  ))
}
