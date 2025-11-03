#' Searches for media files in the Macaulay Library
#'
#' \code{query_macaulay} searches for metadata from
#' \href{https://https://www.macaulaylibrary.org/}{macaulay}.
#' @inheritParams template_params
#' @param format Character vector with the media format to query for. Options
#' are 'sound', 'image' of 'video'. Required.
#' @param path Directory path where the .csv file will be saved. By default it
#' is saved into the current working directory (\code{"."}).
#' @param files Optional character vector with the name(s) of the .csv file(s)
#' to read. If not provided, the function will open a browser window to the
#' search results page, where the user must download a .csv file with the
#' metadata.
#' @param dates Optional numeric vector with years to split the search. If
#' provided, the function will perform separate queries for each date range
#' (between consecutive date values) and combine the results. Useful for
#' queries that return large number of results (i.e. > 10000 results limit).
#' For example, to search for the species between 2010 to 2020 and between 2021
#' to 2025 use \code{dates = c(2010, 2020, 2025)}. If years contain decimals
#' searches will be split by months within years as well.
#' @param taxon_code_info Data frame containing the taxon code information.
#' By default the function will use the internal data frame
#' \code{"ml_taxon_code"} included as example data in the package. This object
#' contains the data from the Clement list from october 2024
#' (downloaded from \url{https://www.birds.cornell.edu/clementschecklist/introduction/updateindex/october-2024/2024-citation-checklist-downloads/}). If new versions of the list become available it will be updated in new package versions. However, if users need to update it they can download the new list version, read it in R as a data frame and provide it to the function through this argument.
#' @export
#' @name query_macaulay
#' @return The function returns a data frame with the metadata of the media
#' files matching the search criteria. If \code{all_data = TRUE}, all metadata
#' fields (columns) are returned. If \code{raw_data = TRUE}, the raw data as
#' obtained from the repository is returned (without any formatting).
#' @details This function queries for species observation info in the
#' \href{https://https://www.macaulaylibrary.org/}{Macaulay Library} online
#' repository and returns the metadata of media files matching the query. The
#' Macaulay Library is the world’s largest repository of digital media
#' (audio, photo, and video) of wildlife, and their habitats. The archive
#' hosts more than 77 million images, 3 million sound recordings, and
#' 350k videos, from more than 80k contributors, and is integrated with
#' eBird, the world’s largest biodiversity dataset. This is an interactive
#' function which opens a browser window to the Macaulay Library's search page,
#' where the user must download a .csv file with the metadata. The function
#' then reads the .csv file and returns a data frame with the metadata.
#'
#' Here are some instructions for using this function properly:
#' \itemize{
#'    \item Users must save the save the .csv file manually
#'    \item \emph{If the file is saved overwritting a pre-existing file
#'    (i.e. same file name) the function will not detect it}
#'    \item A maximum of 10000 records per query can be returned,
#'    but this can be bypassed by using the \code{dates} argument to split
#'    the search into smaller date ranges
#'    \item Users must log in to the Macaulay Library/eBird account in order
#'    to access large batches of observations
#'    }
#' @examples
#' if (interactive()){
#' # query sounds
#' tur_ili <- query_macaulay(species = "Turdus iliacus", format = "sound",
#' path = tempdir())
#'
#' # test a query with more than 10000 results paging by date
#' cal_cos <- query_macaulay(species = "Calypte costae", format = "image",
#' path = tempdir(), dates = c(1976, 2019, 2022, 2024, 2025, 2026))
#'
#' # this is how the internal function that splits the search by year intervals
#' works
#' # it can split by entire year intervals
#' suwo:::.date_ranges(x = c(1976, 2020, 2022, 2024, 2025, 2026))
#'
#' # or by year-month intervals if dates have decimals
#' # (note that it cannot split across years)
#' suwo:::.date_ranges(x = seq(2020, 2026, length.out = 10))
#'
#' ## update clement list (note that this is actually the same list used in the
#' # current 'suwo' version, just for the sake of the example)
#'
#' # url to the clements list version october 2024
#' # (split so it is not truncaded by CRAN)
#' clements_url <- paste0(
#' "https://www.birds.cornell.edu/clementschecklist/wp-content/uploads/2024/10",
#' "/Clements-v2024-October-2024-rev.csv"
#' )
#'
#' # read list from url
#' new_clements <- read.csv(clements_url)
#'
#' # provide "updated" clements list to query_macaulay()
#' tur_ili2 <- query_macaulay(species = "Turdus iliacus", format = "sound",
#'  taxon_code_info = new_clements, path = tempdir())
#' }
#'
#' @references {
#' Scholes III, Ph.D. E (2015). Macaulay Library Audio and Video Collection.
#' Cornell Lab of Ornithology. Occurrence dataset
#' https://doi.org/10.15468/ckcdpy accessed via GBIF.org on 2024-05-09.
#' Clements, J. F., P. C. Rasmussen, T. S. Schulenberg, M. J. Iliff,
#' T. A. Fredericks, J. A. Gerbracht, D. Lepage, A. Spencer, S. M. Billerman,
#' B. L. Sullivan, M. Smith, and C. L. Wood. 2024. The eBird/Clements
#' checklist of Birds of the World: v2024.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

query_macaulay <-
  function(species = getOption("species"),
           format = c("sound", "image", "video"),
           verbose = getOption("verbose", TRUE),
           all_data = getOption("all_data", FALSE),
           raw_data = getOption("raw_data", FALSE),
           path = ".",
           files = NULL,
           dates = NULL,
           taxon_code_info = ml_taxon_code) {
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

    # assign a value to format
    format <- rlang::arg_match(format)

    ml_format <- switch(format,
                        sound = "audio",
                        image = "photo",
                        `video` = "video")

    # get species ML taxon code
    taxon_code <- .taxon_code_search(species, ml_taxon_code = taxon_code_info)

    # function will stop here
    if (is.null(taxon_code)) {
      .message(text = paste(
        "No matching species found for ",
        species,
        sep = ""
      ),as = "failure")

      return(invisible(NULL))
    }

    # Use the unified connection checker
    if (!.checkconnection("macaulay")) {
      return(invisible(NULL))
    }

    new_csv_file_list <- list()

    if (is.null(files)) {

      # Apply to all elements
      if (!is.null(dates)) {
        date_ranges_df <- .date_ranges(x = dates)
      } else {
        date_ranges_df <- data.frame(start_year = NA)
      }

      for (i in seq_len(nrow(date_ranges_df))) {
        if (!is.null(dates)) {
          # extract date range to let users know while batching
          date_range <- if (date_ranges_df$start_month[i] == 1 & date_ranges_df$end_month[i] == 12) {
            if (date_ranges_df$start_year[i] !=  date_ranges_df$end_year[i]) {
              paste0(date_ranges_df$start_year[i],
                     "-",
                     date_ranges_df$end_year[i])
            } else {
              date_ranges_df$start_year[i]
            }
          } else {
            paste0(
              month.abb[date_ranges_df$start_month[i]],
              "-",
              date_ranges_df$start_year[i],
              " - ",
              month.abb[date_ranges_df$end_month[i]],
              "-",
              date_ranges_df$end_year[1]
            )
          }

          if (nrow(date_ranges_df) > 1) {
            cli_bullets(c("*" = paste0(
              "Query ",
              i,
              " of ",
              nrow(date_ranges_df),
              " (",
              date_range,
              "):"
            )))
          }
        }
        # let users know where to save the file
        cat("A browser will open the macaulay library website. Save the .csv file ('export' button) to this directory:")
        cat(normalizePath(path), "/", sep = "")
        cat("   (R is monitoring for new .csv files. Press ESC to stop the function)")

        # pause 3 s so users can read message but only in the first query in a batch
        if (i == 1)
          Sys.sleep(3)

        # construct the search URL
        search_url <- paste0(
          "https://search.macaulaylibrary.org/catalog?view=list&mediaType=",
          ml_format,
          "&taxonCode=",
          taxon_code
        )

        # if (!is.na(date_ranges_df$start_year[i])) {
        if (!is.null(dates)) {
          search_url <- paste0(
            search_url,
            "&beginYear=",
            date_ranges_df$start_year[i],
            "&endYear=",
            date_ranges_df$end_year[i]
          )

          if (date_ranges_df$start_month[i] != 1 | date_ranges_df$end_month[i] != 12) {
            search_url <- paste0(
              search_url,
              "&beginMonth=",
              date_ranges_df$start_month[i],
              "&endMonth=",
              date_ranges_df$end_month[i]
            )
          }
        }

        # open the search URL in the default web browser
        utils::browseURL(search_url)

        # monitor for new files
        new_csv_file_list[[length(new_csv_file_list) + 1]] <- .monitor_new_files(path  = path)

        # let users know the name of the csv file that was read
        cat("\nThe data will be read from the file:", suffix = " ")

        cat(paste(new_csv_file_list[[length(new_csv_file_list)]], "\n"))
      }
    } else {
      new_csv_file_list <- as.list(files)
    }

    # Read the CSV file
    query_output_list <- lapply(new_csv_file_list, function(x)
      read.csv(file.path(path, x), stringsAsFactors = FALSE))

    # combine into a single data frame
    query_output_df <- .merge_data_frames(query_output_list)

    query_output_df$file_url <- vapply(seq_len(nrow(query_output_df)), function(x) {
      paste0(
        "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/",
        query_output_df$ML.Catalog.Number[x],
        "/"
      )
    }, FUN.VALUE = character(1))

    # rename output columns
    query_output_df <- .format_query_output(
      X = query_output_df,
      call = base::match.call(),
      column_names = c(
        "ML.Catalog.Number" = "key",
        "eBird.Species.Code" = "species_code",
        "Scientific.Name" = "species",
        "Format" = "file_extension",
        "behaviors" = "behavior",
        "state" = "state_province",
        "recordist" = "user_name"
      ),
      all_data = all_data,
      format = format,
      input_file = file.path(normalizePath(path), unlist(new_csv_file_list)),
      raw_data = raw_data
    )

    if (verbose) {
      .message(text = "{n} matching record{?s} found", n = nrow(query_output_df), as = "success", suffix = "\n")
    }


    if (nrow(query_output_df) == 10000) {
      .message(text =
        paste("The query returned 10,000 records, which is the maximum allowed. It is likely that more", format, "files that matched the query exists but were not retrieved."),
        as = "warning"
      )
    }

    return(query_output_df)
  }
