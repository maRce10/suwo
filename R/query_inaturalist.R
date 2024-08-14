#' Access 'inaturalist' recordings and metadata
#'
#' \code{query_inaturalist} searches for metadata from \href{https://www.inaturalist.org/}{inaturalist}.
#' @param term Character vector of length one indicating species, to query 'inaturalist' database. For example, \emph{Phaethornis longirostris}.
#' @param type Character vector with media type to query for. Options are 'sound', 'stillimage'. Required.
#' @param verbose Logical argument that determines if text is shown in console. Default is \code{TRUE}.
#' @param cores Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param identified Logical argument to define if search results are categorized as identified by inaturalist.
#' @param verifiable Logical argument to define if search results are categorized as verifiable by inaturalist.
#' @param all_data Logical argument that determines if all data available from database is shown in the results of search. Default is \code{TRUE}.
#' @return If all_data is not provided the function returns a data frame with the following media
#' information: quality_grade, time_observed_at, taxon_geoprivacy, uuid, id, cached_votes_total,
#' identifications_most_agree, species_guess, identifications_most_disagree, positional_accuracy,
#' comments_count, site_id, created_time_zone, license_code, observed_time_zone,
#' public_positional_accuracy, oauth_application_id, created_at, description, time_zone_offset,
#' observed_on, observed_on_string, updated_at, captive, faves_count, num_identification_agreements,
#' map_scale, uri, community_taxon_id, owners_identification_from_vision, identifications_count,
#' obscured, num_identification_disagreements, geoprivacy, location, spam, mappable,
#' identifications_some_agree, place_guess, id, license_code, file_url, attribution, page, repository
#' @export
#' @name query_inaturalist
#' @details This function queries for species observation info in the open-access
#' online repository \href{https://www.inaturalist.org/}{inaturalist}. It can return media metadata.
#' @examples
#' \dontrun{
#' # search without downloading
# df1 <- query_inaturalist(term = 'Turdus iliacus', type = "sound", cores = 4)
#' View(df1)
#' }
#'
#' @references {
#' iNaturalist. Available from https://www.inaturalist.org. Accessed [date]
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

query_inaturalist <- function(term = NULL,
                              cores = 1,
                              pb = TRUE,
                              verbose = TRUE,
                              type = c("sound", "still image"),
                              identified = FALSE,
                              verifiable = FALSE,
                              all_data = TRUE,
                              save_path = paste0(term,".csv")) {
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

  # type must be supplied
  if (is.null(type)) {
    .stop("'type' must be supplied")
  }

  org_type <- match.arg(type)

  type <- switch(type,
                 sound = "sounds",
                 "still image" = "photos"
  )

  # term must be supplied
  if (is.null(term)) {
    .stop("'term' must be supplied")
  }

  # check internet connection
  a <- try(RCurl::getURL("https://www.inaturalist.org/"), silent = TRUE)
  if (is(a, "try-error")) {
    .stop("No connection to INaturalist (check your internet connection!)")
  }

  if (a == "Could not connect to the database") {
    .stop("observation website is apparently down")
  }

  # Save species name
  species <- term

  # format JSON
  term <- gsub(" ", "%20", term)

  srch_trm <- paste0(
    "https://api.inaturalist.org/v1/observations?per_page=200&",
    "taxon_name=", term, "&",
    type, "=true", "&", "identified=",
    identified, "&", "verifiable=", verifiable
  )

  base.srch.pth <- jsonlite::fromJSON(srch_trm)

  #### org_type in tolower

  # message if nothing found
  if (base.srch.pth$total_results == 0) {
    if (verbose) {
      cat(paste(.color_text(paste0("No ", tolower(org_type), "s were found"), "failure"), .add_emoji("sad")))
    }
  } else {
    # message number of results
    if (pb & verbose) {
      cat(paste(.color_text(paste0("Obtaining metadata (", base.srch.pth$total_results, " matching observation(s) found)"), "success"), .add_emoji("happy"), ":\n"))
    }

    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & cores > 1) {
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores))
    } else {
      cl <- cores
    }


    # Determine the last processed id from the csv
    last_id <- if (file.exists(save_path)) {
      save_path_data <- read.csv(save_path)
      min(save_path_data$id)
    } else {
      0
    }

    # Determine the last processed page from the csv
    last_processed_page <- if (file.exists(save_path)) {
      save_path_data <- read.csv(save_path)
      min(save_path_data$result_number)
    } else {
      0
    }
    if (last_processed_page != 0) {number_results <-  list()} else {number_results <- list(last_id)}
#-------------------------------------------------------------------------------------------------------------------
    query_output_list <- pblapply_sw_int(number_results, cl = cl, pbar = pb, function(i) {
      query_output <- if (last_id != 0){jsonlite::fromJSON(paste0(srch_trm, "&id_below=", last_id))} else {jsonlite::fromJSON(paste0(srch_trm, "&id_above=0"))}

      # format as list of data frame
      query_output$results <- lapply(seq_len(nrow(query_output$results)), function(u) {
        x <- as.data.frame(query_output$results[u, ])

        if (type == "sounds") {
          media_df <- do.call(rbind, x$sounds)
        } else {
          media_df <- do.call(rbind, x$photos)
        }

        # media data frame with image details
        media_df <- media_df[!sapply(media_df, is.list)]
        media_df <- data.frame(media_df)
        names(media_df)[names(media_df) == "url"] <- "media-URL"

        # remove lists
        x <- x[!sapply(x, is.list)]

        # make it data frame
        X_df <- data.frame(t(unlist(x)))
        X_df$unitnumber <- u
        # add media details
        X_df <- cbind(X_df, media_df)

        if (file.exists(save_path)) {
          write.table(X_df, file = save_path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
          last_id <- max(query_output$results$id)
        } else {
          write.table(X_df, file = save_path, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
          last_id <- max(query_output$results$id)
        }

        if (u == 200) {
          number_results <- append(number_results, x$id)
        }
        return(X_df)
      })
#-------------------------------------------------------------------------------------------------------------------
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
                         check.names = FALSE
              )
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
                       check.names = FALSE
            )
          names(e)[ncol(e)] <- o
        }
      }
      return(e)
    })

    # all results in a single data frame
    query_output_df <- do.call(rbind, query_output_list)

    # Change column name for media download function
    colnames(query_output_df)[colnames(query_output_df) == "media-URL"] <- "file_url"

    # Add repository ID
    query_output_df$repository <- "INAT"

    split_location <- do.call(rbind, strsplit(as.character(query_output_df$location), ","))

    # Convert the split values to numeric
    latitude <- split_location[, 1]
    longitude <- split_location[, 2]

    # Add the latitude and longitude columns to the data frame
    query_output_df$latitude <- latitude
    query_output_df$longitude <- longitude

    # Find the index of the first occurrence of "id" in the old column names
    first_id_index <- which(names(query_output_df) == "id")[1]

    # Check if a match was found before proceeding
    if (!is.na(first_id_index)) {
      # Replace the first occurrence of "id" with "key" in the new column names
      names(query_output_df)[first_id_index] <- "key"
    }

    # rename output columns
    names_df <- data.frame(old = c("quality_grade", "time_observed_at", "taxon_geoprivacy", "uuid", "key", "cached_votes_total", "identifications_most_agree", "species_guess", "identifications_most_disagree", "positional_accuracy", "comments_count", "site_id", "created_time_zone", "license_code", "observed_time_zone", "public_positional_accuracy", "oauth_application_id", "created_at", "description", "time_zone_offset", "observed_on", "observed_on_string", "updated_at", "captive", "faves_count", "num_identification_agreements", "map_scale", "uri", "community_taxon_id", "owners_identification_from_vision", "identifications_count", "obscured", "num_identification_disagreements", "geoprivacy", "location", "spam", "mappable", "identifications_some_agree", "place_guess", "file_url", "subtype", "play_local", "native_sound_id", "attribution", "id", "file_content_type", "license_code", "secret_token", "hidden", "page", "repository", "latitude", "longitude"), new = c("quality_grade", "time_observed_at", "taxon_geoprivacy", "uuid", "key", "cached_votes_total", "identifications_most_agree", "species_guess", "identifications_most_disagree", "positional_accuracy", "comments_count", "site_id", "created_time_zone", "license_code", "observed_time_zone", "public_positional_accuracy", "oauth_application_id", "created_at", "description", "time_zone_offset", "observed_on", "observed_on_string", "updated_at", "captive", "faves_count", "num_identification_agreements", "map_scale", "uri", "community_taxon_id", "owners_identification_from_vision", "identifications_count", "obscured", "num_identification_disagreements", "geoprivacy", "location", "spam", "mappable", "identifications_some_agree", "place_guess", "file_url", "subtype", "play_local", "native_sound_id", "attribution", "file_id", "media_extension", "license_code", "secret_token", "hidden", "page", "repository", "latitude", "longitude"))

    for (i in 1:nrow(names_df)) {
      names(query_output_df)[names(query_output_df) == names_df$old[i]] <- names_df$new[i]
    }

    # Add species
    query_output_df$species <- species

    if (!all_data) {
      query_output_df$country <- NA
      query_output_df$date <- query_output_df$time_observed_at
      query_output_df <- query_output_df[, c("key", "species", "date", "country", "location", "latitude", "longitude", "file_url", "repository")]
    }

    # Replace square image size to original in file_url
    replace_image_size <- function(file_url) {
      gsub("square", "original", file_url)
    }
    for (i in 1:nrow(query_output_df)) {
      query_output_df$file_url[i] <- replace_image_size(query_output_df$file_url[i])
    }

    # Remove files that have no download link
    query_output_df <- query_output_df[!is.na(query_output_df$file_url), ]

    # Add a timestamp attribute
    search_time <- Sys.time()
    attr(query_output_df, "search_time") <- search_time
    attr(query_output_df, "query_term") <- term
    attr(query_output_df, "query_type") <- org_type
    attr(query_output_df, "query_all_data") <- all_data

    return(query_output_df)
  }
}
