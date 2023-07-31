#install.packages("httr")
#install.packages("readr")
library(httr)
library(readr)

update_gbif_datasets <- function(gbif_datasets) {
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

