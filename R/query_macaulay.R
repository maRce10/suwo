#' Access 'observation' recordings and metadata
#'
#' \code{query_macaulay} searches for metadata from \href{https://https://www.macaulaylibrary.org/}{macaulay}.
#' @usage query_macaulay(term = NULL, type = c("sound", "still image"),
#' cores = 1, pb = TRUE, verbose = TRUE, token = NULL)
#' @param term Character vector of length one indicating the
#'  species, to query 'observation' database. For example \emph{Phaethornis longirostris}.
#' @param type Character vector with media type to query for. Currently 'still image' and 'sound' are available.
#' @param cores Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param verbose Logical argument that determines if text is shown in console. Default is \code{TRUE}.
#' @param token Character refering to the token assigned by Observation.org as authorization for searches.
#' @return If all_data is not provided the function returns a data frame with the following media
#' information: id, scientific_name, name, group, group_name, status, rarity, photo,
#' info_text, permalink, determination_requirements, file_url, repository
#' @export
#' @name query_macaulay
#' @details This function queries for species observation info in the open-access
#' online repository \href{https://https://www.macaulaylibrary.org/}{macaulay}. It can return media metadata.
#' @examples
#' \dontrun{
#' # search without downloading
# df1 <- query_macaulay(term = 'Turdus iliacus', type = "Sound", cores = 4)
#' View(df1)
#' }
#'
#' @references {
#'
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
query_macaulay <-
  function(term = NULL,
           type = c("audio", "photo", "video"),
           cores = 1,
           pb = TRUE,
           verbose = TRUE,
           token = NULL,
           all_data = TRUE) {
    # check arguments
    arguments <- as.list(base::match.call())[-1]

    # add objects to argument names
    for (i in names(arguments)) {
      arguments[[i]] <- get(i)
    }

    # check each arguments
    check_results <- check_arguments(args = arguments)

    # report errors
    checkmate::reportAssertions(check_results)

    # term must be supplied
    if (is.null(term)) {
      stop2("'term' must be supplied")
    }

    # type must be supplied
    if (is.null(type)) {
      stop2("'type' must be supplied")
    }
    org_type <- match.arg(type)

    type <- switch(type,
                   sound = "audio",
                   `still image` = "photo",
                   `video` = "video"
    )

    if (tolower(Sys.info()[["sysname"]]) != "windows"){
      # check internet connection
      a <- try(RCurl::getURL("https://www.macaulaylibrary.org/"), silent = TRUE)
      if (is(a, "try-error")) {
        stop2("No connection to macaulaylibrary.org (check your internet connection!)")
      }

      if (a == "Could not connect to the database") {
        stop2("macaulaylibrary.org website is apparently down")
      }
    }



    search_url <- paste0("https://search.macaulaylibrary.org/catalog?view=list&mediaType=", type)

    browseURL(search_url)

    #Obtain file snapshot
    snapshot <- fileSnapshot()

    #Ask if user has downloaded csv file from Macaulay library
    user_input <- readline("Is the data table csv downloaded? (y/n)  ")
    if(user_input != 'y') stop('Exiting since you did not press y')

    #Obtain file path from added files
    changed_files <- changedFiles(snapshot)

    file_path <- changed_files[["added"]][grep("\\.csv$",changed_files[["added"]])]

    if(file_path == "") stop('file not found')

    # find csv in files
    query_output_df <- read.csv(file_path)

    # Change column name for media download function
    colnames(query_output_df)[colnames(query_output_df) == "ML.Catalog.Number"] <- "key"
    colnames(query_output_df)[colnames(query_output_df) == "eBird.Species.Code"] <- "species_code"
    colnames(query_output_df)[colnames(query_output_df) == "Scientific.Name"] <- "species"

    # Change column names to standard metadata used in query functions
    colnames(query_output_df)[colnames(query_output_df) == "Date"] <- "date"
    colnames(query_output_df)[colnames(query_output_df) == "Country"] <- "country"
    colnames(query_output_df)[colnames(query_output_df) == "Locality"] <- "location"
    colnames(query_output_df)[colnames(query_output_df) == "Longitude"] <- "longitude"
    colnames(query_output_df)[colnames(query_output_df) == "Latitude"] <- "latitude"

    query_output_df$file_url <- sapply(seq_len(nrow(query_output_df)), function(x){
      paste0("https://cdn.download.ams.birds.cornell.edu/api/v1/asset/", query_output_df$key[x], "/", type)}
                                       )
    # Add repository ID
    query_output_df$repository <- "Macaulay"

    if (!all_data) {
      query_output_df <- query_output_df[, c("key", "species", "date", "country", "location", "latitude", "longitude", "file_url", "repository")]
    }

    return(query_output_df)
}
