#' Access 'gbif' recordings and metadata
#'
#' \code{query_gbif} searches for metadata from \href{https://www.gbif.org/}{gbif}.
#' @inheritParams template_params
#' @param type Character vector with media type to query for. Options are 'sound', 'stillimage', 'movingimage' and 'interactiveresource'. Required.
#' @param dataset see \url{https://www.gbif.org/dataset/search?q=}.
#' @return If all_data is \code{FALSE} the function returns a data frame with the following media information: id, species, date, country, location, latitude, longitude, file_url, repository. If all_data is \code{TRUE} the function returns a data frame with the following media
#' information: key, datasetKey, publishingOrgKey, installationKey, hostingOrganizationKey,
#' publishingCountry, protocol, lastCrawled, lastParsed, crawlId, basisOfRecord, occurrenceStatus,
#' taxonKey, kingdomKey, phylumKey, classKey, orderKey, familyKey, genusKey, speciesKey,
#' acceptedTaxonKey, scientificName, acceptedScientificName, kingdom, phylum, order, family, genus,
#' species, genericName, specificEpithet, taxonRank, taxonomicStatus, iucnRedListCategory,
#' dateIdentified, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, continent,
#' stateProvince, year, month, day, eventDate, modified, lastInterpreted, references, license,
#' isInCluster, datasetName, recordedBy, identifiedBy, geodeticDatum, class, countryCode, country,
#' rightsHolder, identifier, http...unknown.org.nick, verbatimEventDate, verbatimLocality,
#' collectionCode, gbifID, occurrenceID, taxonID, catalogNumber, institutionCode, eventTime,
#' occurrenceRemarks, http...unknown.org.captive, identificationID, identificationRemarks,
#' distanceFromCentroidInMeters, informationWithheld, sex, lifeStage, preparations,
#' nomenclaturalCode, dynamicProperties, locality, vernacularName, fieldNotes, verbatimElevation,
#' behavior, higherClassification, associatedTaxa, infraspecificEpithet, media-type, media-format,
#' media-references, media-created, media-creator, media-publisher, media-license,
#' media-rightsHolder, file_url, media-description, page, elevation, elevationAccuracy,
#' organismQuantity, organismQuantityType, georeferenceProtocol, verbatimSRS, county,
#' verbatimCoordinateSystem, type, collectionID, individualCount, samplingProtocol,
#' scientificNameID, georeferenceRemarks, language, georeferenceSources, media-title, repository.
#' @export
#' @name query_gbif
#' @details This function queries for species observation info in the open-access
#' online repository \href{https://www.gbif.org/}{gbif}. It can return media metadata.
#' @seealso \code{\link{query_gbif}}
#' @examples
#' \dontrun{
#' # search without downloading
# df1 <- query_gbif(term = 'Turdus iliacus', type = "Sound", cores = 4)
#' View(df1)
#' }
#'
#' @references {
#' GBIF.org (2024), GBIF Home Page. Available from: https://www.gbif.org [13 January 2020].
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
query_gbif <-
  function(term,
           type = c("sound", "still image", "moving image", "interactive resource"),
           cores = getOption("mc.cores", 1),
           pb = getOption("pb", TRUE),
           verbose = getOption("verbose", TRUE),
           dataset = NULL,
           all_data = getOption("all_data", FALSE)) {
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

    type <- switch(
      type,
      sound = "Sound",
      `still image` = "StillImage",
      `moving image` = "MovingImage",
      `interactive resource` = "InteractiveResource"
    )

    # Check internet connection using httr and error handling
    response <- try(httr::GET("https://api.gbif.org/"), silent = TRUE)
    if (inherits(response, "try-error") ||
        httr::http_error(response)) {
      .stop("No connection to GBIF API (check your internet connection!)")
    }

    content <- httr::content(response, as = "text", encoding = "UTF-8")
    if (grepl("Could not connect to the database", content)) {
      .stop("GBIF website is apparently down")
    }


    # If cores is not numeric
    if (!is.numeric(cores)) {
      .stop("'cores' must be a numeric vector of length 1")
    }
    if (any(!(cores %% 1 == 0), cores < 1)) {
      .stop("'cores' should be a positive integer")
    }

    # fix term for html
    term <- gsub(" ", "%20", term)

    srch_trm <- paste0(
      "https://api.gbif.org/v1/occurrence/search?limit=300&",
      if (is.null(dataset))
        ""
      else
        "datasetKey=",
      dataset,
      "&scientificName=",
      term,
      "&media_type=",
      type
    )

    base.srch.pth <- jsonlite::fromJSON(srch_trm)

    # message if nothing found
    if (base.srch.pth$count == 0) {
      if (verbose) {
        cat(paste(.color_text(
          paste0("No ", tolower(org_type), "s were found"), "failure"
        ), .add_emoji("sad")))
      }
    } else {
      # message number of results
      if (pb & verbose) {
        cat(paste(
          .color_text(
            paste0(
              "Obtaining metadata (",
              base.srch.pth$count,
              " matching observation(s) found)"
            ),
            "success"
          ),
          .add_emoji("happy"),
          ":\n"
        ))
      }


      # get total number of pages
      offsets <- (seq_len(ceiling(
        base.srch.pth$count / base.srch.pth$limit
      )) - 1) * 300

      # set clusters for windows OS
      if (Sys.info()[1] == "Windows" & cores > 1) {
        cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores))
      } else {
        cl <- cores
      }

      query_output_list <- pblapply_sw_int(offsets, cl = 1, pbar = pb, function(i) {
        query_output <- jsonlite::fromJSON(paste0(srch_trm, "&offset=", i))

        # format as list of data frame
        query_output$results <- lapply(seq_len(nrow(query_output$results)), function(u) {
          x <- query_output$results[u, ]

          # media_df <- do.call(rbind, media_list)
          media_df <- do.call(rbind, x$media)

          # select type
          media_df <- media_df[media_df$type == type, ]

          # fix identifier column name
          names(media_df)[names(media_df) == "identifier"] <- "URL"
          names(media_df) <- paste0("media-", names(media_df))


          # remove lists
          x <- x[!sapply(x, is.list)]

          # make it data frame
          X_df <- data.frame(t(unlist(x)))

          # add media details
          X_df <- cbind(X_df, media_df)

          return(X_df)
        })

        # get common names to all data frames in X
        common_names <- unique(unlist(lapply(query_output$results, names)))

        # add missing columns to all data frames in X
        query_output$results <- lapply(query_output$results, function(e) {
          nms <- names(e)
          if (length(nms) != length(common_names)) {
            for (o in common_names[!common_names %in% nms]) {
              e <-
                data.frame(e,
                           NA,
                           stringsAsFactors = FALSE,
                           check.names = FALSE)
              names(e)[ncol(e)] <- o
            }
          }
          return(e)
        })

        # all results in a single data frame
        output_df <- do.call(rbind, query_output$results)

        output_df$page <- i

        return(output_df)
      })

      # get common names to all data frames in X
      common_names <- unique(unlist(lapply(query_output_list, names)))

      # add missing columns to all data frames in X
      query_output_list <- lapply(query_output_list, function(e) {
        nms <- names(e)
        if (length(nms) != length(common_names)) {
          for (o in common_names[!common_names %in% nms]) {
            e <-
              data.frame(e,
                         NA,
                         stringsAsFactors = FALSE,
                         check.names = FALSE)
            names(e)[ncol(e)] <- o
          }
        }
        return(e)
      })

      # all results in a single data frame
      query_output_df <- do.call(rbind, query_output_list)

      # Change column name for media download function
      names(query_output_df)[names(query_output_df) == "media-URL"] <- "file_url"

      # Add repository ID
      query_output_df$repository <- "GBIF"

      if (!all_data) {
        names(query_output_df)[names(query_output_df) == "decimalLatitude"] <- "latitude"

        names(query_output_df)[names(query_output_df) == "decimalLongitude"] <- "longitude"

        names(query_output_df)[names(query_output_df) == "scientificName"] <- "species"

        # remove everything after the first parenthesis
        query_output_df$species <- gsub("\\s*\\(.*?\\)", "", query_output_df$species)

        names(query_output_df)[names(query_output_df) == "key"] <- "key"

        names(query_output_df)[names(query_output_df) == "eventDate"] <- "date"

        names(query_output_df)[names(query_output_df) == "locality"] <- "location"

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

      # Generate a file path by combining tempdir() with a file name
      # file_path <- file.path(tempdir(), paste0(term, ".rds"))

      # Save the object to the file
      # saveRDS(query_output_df, file = file_path)

      return(query_output_df)
    }
  }
