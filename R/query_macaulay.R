#' Searches for media files in the Macaulay Library
#'
#' \code{query_macaulay} searches for metadata from \href{https://https://www.macaulaylibrary.org/}{macaulay}.
#' @inheritParams template_params
#' @param path Character that defines the location where the .csv file will be downloaded. By default is downloaded to the current working directory (\code{"."}).
#' @return If all_data is not provided the function returns a data frame with the following media
#' information: id, scientific_name, name, group, group_name, status, rarity, photo,
#' info_text, permalink, determination_requirements, file_url, repository
#' @export
#' @name query_macaulay
#' @details This function queries for species observation info in the open-access
#' online repository \href{https://https://www.macaulaylibrary.org/}{macaulay} and returns media metadata. This is an interactive function which opens a browser window to the search results page, where the user must download a .csv file with the metadata. After the .csv file is downloaded, the user must confirm that the file has been downloaded. The function then reads the .csv file and returns a data frame with the metadata. Term must be a species name.
#' @examples
#' \dontrun{
#' # search without downloading
# df1 <- query_macaulay(term = 'Turdus iliacus', type = "sound", cores = 4, path = tempdir())
#' View(df1)
#' }
#'
#' @references {
#' Scholes III, Ph.D. E (2015). Macaulay Library Audio and Video Collection. Cornell Lab of Ornithology. Occurrence dataset https://doi.org/10.15468/ckcdpy accessed via GBIF.org on 2024-05-09.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
query_macaulay <-
  function(term,
           type = c("sound", "photo", "video"),
           cores = getOption("mc.cores", 1),
           pb = getOption("pb", TRUE),
           verbose = getOption("verbose", TRUE),
           all_data = getOption("all_data", FALSE),
           path = ".") {
    # check arguments
    arguments <- as.list(base::match.call())[-1]

    # add objects to argument names
    for (i in names(arguments)) {
      arguments[[i]] <- get(i)
    }

    # check each arguments
    check_results <- .check_arguments(args = arguments)

    # report errors
    checkmate::reportAssertions(check_results)

    # assign a value to type
    org_type <- type <- rlang::arg_match(type)

    type <- switch(type,
                   sound = "audio",
                   `still image` = "photo",
                   `video` = "video")


    # Check internet connection using httr and error handling
    response <- try(httr::GET("https://www.macaulaylibrary.org/"), silent = TRUE)
    if (inherits(response, "try-error") ||
        httr::http_error(response)) {
      return("No connection to macaulaylibrary.org (check your internet connection!)")
    }

    content <- httr::content(response, as = "text")
    if (grepl("Could not connect to the database", content)) {
      return("macaulaylibrary.org website is apparently down")
    }

    user_input_species <- term

    taxon_code <- taxon_code_search(user_input_species)

    if (!is.null(taxon_code)) {
      # cat(
      #   paste(
      #     "The species code for '",
      #     user_input_species,
      #     "' is '",
      #     taxon_code,
      #     "'.\n",
      #     sep = ""
      #   )
      # )

      cat(
        paste(
          "A browser will open the macaulay website. Save the csv file ('export' button) in this directory: '",
          path,
          "'.\n",
          sep = ""
        )
      )
      # pause 1 s so users can read message
      Sys.sleep(3)

    } else {
      cat(paste(
        "No matching species found for '",
        user_input_species,
        "'.\n",
        sep = ""
      ))
      return(NULL)
    }

    # Take snapshot before asking for download confirmation
    snapshot <- utils::fileSnapshot(path = path)

    # construct the search URL
    search_url <- paste0(
      "https://search.macaulaylibrary.org/catalog?view=list&mediaType=",
      type,
      "&taxonCode=",
      taxon_code
    )

    # open the search URL in the default web browser
    utils::browseURL(search_url)

    #Ask if user has downloaded csv file from Macaulay library
    user_input <- readline("Is the data table csv downloaded? (y/n)  ")
    if (user_input != 'y') {
      stop('Exiting since you did not press y')
    }

    # Obtain updated file path after the user confirms the download
    changed_files <- utils::changedFiles(snapshot)

    # Filter for CSV files, ignoring case
    csv_files <- changed_files[["added"]][grep("\\.csv$", changed_files[["added"]], ignore.case = TRUE)]

    # Check if any CSV file is found
    if (length(csv_files) == 0)
      stop('No CSV file found')

    # Check the csv file for file path
    file_path <- csv_files[1]

    # Read the CSV file
    query_output_df <- read.csv(file.path(path, file_path), stringsAsFactors = FALSE)


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

    query_output_df$file_url <- sapply(seq_len(nrow(query_output_df)), function(x) {
      paste0(
        "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/",
        query_output_df$key[x],
        "/",
        type
      )
    })
    # Add repository ID
    query_output_df$repository <- "Macaulay Library"

    if (!all_data) {
      query_output_df <- query_output_df[, c(
        "key",
        "species",
        "date",
        "country",
        "location",
        "latitude",
        "longitude",
        "file_url",
        "repository"
      )]
    }

    # Add a timestamp attribute
    search_time <- Sys.time()
    attr(query_output_df, "search_time") <- search_time
    attr(query_output_df, "query_term") <- term
    attr(query_output_df, "query_type") <- org_type
    attr(query_output_df, "query_all_data") <- all_data
    attr(query_output_df, "input_file") <- file.path(path, csv_files[1])

    # let users know the name of the csv file that was read
    cat(paste("The data was read from the file:", csv_files, "\n"))

    # # Generate a file path by combining tempdir() with a file name
    # file_path <- file.path(tempdir(), paste0(term, ".rds"))
    #
    # # Save the object to the file
    # saveRDS(query_output_df, file = file_path)
    return(query_output_df)
  }
