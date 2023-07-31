#' Access 'gbif' dataset information
#'
#' \code{gbif_dataset_update} searches for dataset information from \href{https://www.gbif.org}{gbif}.
#' @usage gbif_dataset_update()
#' @return returns dataset csv from gbif
#' @export
#' @name gbif_dataset_update
#' @details This function queries for dataset info in the open-access
#' online repository \href{https://www.gbif.org}{gbif}.
#' @examples
#' \dontrun{
#' # update dataset information
# df1 <- gbif_dataset_update()
#' View(df1)
#'
#' }
#'
#' @references {
#'
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

update_gbif_datasets <- function(gbif_datasets) {

  #check internet connection
  a <- try(RCurl::getURL("https://www.gbif.org"), silent = TRUE)
  if (is(a, "try-error"))
    stop2("No connection to gbif (check your internet connection!)")

  if (a == "Could not connect to the database")

  # Download the CSV file
  response <- GET("https://api.gbif.org/v1/dataset/search/export?format=CSV&")

  # Check if the download was successful
  if (http_type(response) == "text/csv") {
    # Read the CSV data
    csv_data <- read_csv(text = content(response, "text"))

    # Update the dataset object with the new data
    gbif_datasets <- csv_data

    # Return the updated object
    return(gbif_datasets)
  } else {
    stop("Failed to download CSV.")
  }
}

