#' Access 'gbif' media file metadata
#'
#' `query_gbif` searches for metadata from
#' \href{https://www.gbif.org/}{gbif}.
#' @inheritParams template_params
#' @param format Character vector with the media format to query for.
#' Options are 'sound', 'image', 'video' and 'interactive resource'.
#' Can be set globally for the current R session via the "suwo_format"
#' option (e.g. `options(suwo_format = "image")`). Required.
#' @param dataset The name of a specific dataset in which to focus the query
#' (by default it searchs across all available datasets).
#' Users can check available dataset names by downloading this csv file
#'  \url{https://api.gbif.org/v1/dataset/search/export?format=CSV&}.
#'  See \url{https://www.gbif.org/dataset/search?q=} for more details.
#' @export
#' @name query_gbif
#' @return The function returns a data frame with the metadata of the media
#' files matching the search criteria. If `all_data = TRUE`, all metadata
#' fields (columns) are returned. If `raw_data = TRUE`, the raw data as
#' obtained from the repository is returned (without any formatting).
#' @details This function queries for species observation info in the
#' open-access
#' online repository \href{https://www.gbif.org/}{GBIF}.
#' GBIF (the Global Biodiversity Information Facility) is an international
#' network and data infrastructure funded by the world's governments and
#' aimed at providing open access to data about all types of life on Earth.
#' Note that some of the records returned by this function could be duplicates
#' of records returned by other suwo functions
#' (e.g., [query_inaturalist()]).
#' @seealso [query_inaturalist()]
#' @examples
#' # search dink frog sound files
# d_diastema <- query_gbif(species = "Diasporus diastema", format = "sound")
#'
#' @references
#' GBIF.org (2024), GBIF Home Page. Available from: https://www.gbif.org/
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
query_gbif <-
  function(species = getOption("suwo_species"), format =
    getOption("suwo_format", c("image", "sound", "video", "interactive resource")),
           cores = getOption("mc.cores", 1),
           pb = getOption("suwo_pb", TRUE),
           verbose = getOption("suwo_verbose", TRUE),
           dataset = NULL,
           all_data = getOption("suwo_all_data", FALSE),
           raw_data = getOption("suwo_raw_data", FALSE)) {
    # check arguments
    arguments <- as.list(base::match.call())

    # add objects to argument names
   for (i in names(arguments)[-1]) {
      arguments[[i]] <- get(i)
    }

    # check each arguments
    check_results <- .check_arguments(fun = arguments[[1]], args = arguments)

    # report errors
    checkmate::reportAssertions(check_results)

    # Use the unified connection checker
    if (!.checkconnection(verb = verbose, service = "gbif")) {
      return(invisible(NULL))
    }

    # assign a value to format
    format <- rlang::arg_match(format)

    gbif_format <- switch(
      format,
      sound = "Sound",
      `image` = "StillImage",
      `video` = "MovingImage",
      `interactive resource` = "InteractiveResource"
    )

    srch_trm <- paste0(
      "https://api.gbif.org/v1/occurrence/search?limit=300&",
      if (is.null(dataset))
        ""
      else
        "datasetKey=",
      dataset,
      "&scientificName=",
      gsub(" ", "%20", species),
      "&media_type=",
      gbif_format
    )

    base.srch.pth <- try(jsonlite::fromJSON(srch_trm), silent = TRUE)

    # let user know error when downloading metadata
    if (.is_error(base.srch.pth)) {
      if (verbose) {
        .message(text = "Metadata could not be downloaded", as = "failure")
      }
      return(invisible(NULL))
    }

    # message if nothing found
    if (base.srch.pth$count == 0) {
      if (verbose) {
        .message(text = "No matching records found", as = "failure")
      }
      return(invisible(NULL))
    }

    # message number of results
    if (verbose) {
      .message(n = base.srch.pth$count, as = "success")
    }

    # get total number of pages
    offsets <- (seq_len(ceiling(
      base.srch.pth$count / base.srch.pth$limit
    )) - 1) * 300

    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" && cores > 1) {
      cl <- parallel::makePSOCKcluster(cores)
    } else {
      cl <- cores
    }

    query_output_list <- .pbapply_sw(offsets, cl = cl, pbar = pb,
                                         function(i) {
      query_output <-
        try(jsonlite::fromJSON(paste0(srch_trm, "&offset=", i)),
                          silent = TRUE)

      # if error then just return it and stop here
      if (.is_error(query_output)){
        return(query_output)
      }

      # format as list of data frame
      query_output$results <- lapply(seq_len(nrow(query_output$results)),
                                     function(u) {
        x <- query_output$results[u, ]

        media_df <- do.call(rbind, x$media)

        # select format
        media_df <- media_df[media_df$type == gbif_format, ]

        # fix identifier column name
        names(media_df)[names(media_df) == "identifier"] <- "URL"
        names(media_df) <- paste0("media-", names(media_df))

        # remove lists
        x <- x[!vapply(x, is.list, logical(1))]

        # make it data frame
        X_df <- data.frame(t(unlist(x)))

        # add media details
        X_df <- cbind(X_df, media_df)

        return(X_df)
      })

      output_df <- .merge_data_frames(query_output$results)
      output_df$page <- i

      return(output_df)
    })


    # let user know error when downloading metadata
    if (any(vapply(query_output_list, .is_error, FUN.VALUE = logical(1)))) {
      if (verbose) {
        .message(text = "Metadata could not be downloaded", as = "failure")
      }
      return(invisible(NULL))
    }

    # combine into a single data frame
    query_output_df <- .merge_data_frames(query_output_list)

    # stop here if nothing found
    if (is.null(query_output_df))
      return(query_output_df)

    # remove everything after the second parenthesis
    query_output_df$species <- vapply(strsplit(query_output_df$species, " "),
                                      function(x)
      paste(x[1], x[2]), character(1))

    # remove duplicated info
    query_output_df$gbifid <- query_output_df$scientificName <- NULL

    # format output data frame column names
    query_output_df <- .format_query_output(
      X = query_output_df,
      call = base::match.call(),
      column_names = c(
        "media-URL" = "file_url",
        "eventDate" = "date",
        "decimalLatitude" = "latitude",
        "decimalLongitude" = "longitude",
        "specificepithet" = "specific_epithet",
        "recordedby" = "recordist",
        "stateprovince" = "state_province",
        "specieskey" = "species_code",
        "genuskey" = "genus_code",
        "kingdomkey" = "kingdom_code",
        "phylumkey" = "phylum_code",
        "classkey" = "class_code",
        "orderkey" = "order_code",
        "familykey" = "family_key",
        "fieldnotes" = "comments",
        "eventtime" = "time",
        "media-creator" = "user_name",
        "media-format" = "file_extension"
      ),
      all_data = all_data,
      format = format,
      raw_data = raw_data
    )

    return(query_output_df)
  }
