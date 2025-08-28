#' Access 'gbif' recordings and metadata
#'
#' \code{query_gbif} searches for metadata from \href{https://www.gbif.org/}{gbif}.
#' @inheritParams template_params
#' @param format Character vector with the media format to query for. Options are 'sound', 'image', 'video' and 'interactive resource'. Required.
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
# df1 <- query_gbif(term = 'Turdus iliacus', format = "Sound", cores = 4)
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
           format = c("sound", "image", "video", "interactive resource"),
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

    # assign a value to format
    org_format <- format <- rlang::arg_match(format)

    format <- switch(
      format,
      sound = "Sound",
      `image` = "StillImage",
      `video` = "MovingImage",
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

    # fix term for html but keep original for attributes
    org_term <- term
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
      format
    )

    base.srch.pth <- jsonlite::fromJSON(srch_trm)

    # message if nothing found
    if (base.srch.pth$count == 0) {
      if (verbose) {
        cat(paste(.color_text(
          paste0("No ", tolower(org_format), "s were found"), "failure"
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

          # select format
          media_df <- media_df[media_df$type == format, ]

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

      # add file format
      query_output_df$file_extension <- sub(".*\\.", "", query_output_df$`media-URL`)

      # fix formatting
      query_output_df$file_extension <- .fix_extension(query_output_df$file_extension)

      # remove everything after the first parenthesis
      query_output_df$species <- gsub("\\s*\\(.*?\\)", "", query_output_df$scientificName)

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
          "eventtime" = "time"
        ),
        all_data = all_data,
        format = org_format
      )

      return(query_output_df)
    }
  }
