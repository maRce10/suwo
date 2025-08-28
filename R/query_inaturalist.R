#' Access 'inaturalist' recordings and metadata
#'
#' \code{query_inaturalist} searches for metadata from \href{https://www.inaturalist.org/}{inaturalist}.
#' @param term Character vector of length one indicating species, to query 'inaturalist' database. For example, \emph{Phaethornis longirostris}.
#' @param format Character vector with the media format to query for. Options are 'sound', 'image'. Required.
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
#' obscured, num_identification_disagreements, geoprivacy, locality, spam, mappable,
#' identifications_some_agree, place_guess, file_url, attribution, page, repository
#' @export
#' @name query_inaturalist
#' @details This function queries for species observation info in the open-access
#' online repository \href{https://www.inaturalist.org/}{inaturalist}. It can return media metadata.
#' @examples
#' \dontrun{
#' # search without downloading
# df1 <- query_inaturalist(term = 'Turdus iliacus', format = "sound", cores = 4)
#' View(df1)
#' }
#'
#' @references {
#' iNaturalist. Available from https://www.inaturalist.org. Accessed [date]
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

query_inaturalist <- function(term,
                              cores = getOption("mc.cores", 1),
                              pb = getOption("pb", TRUE),
                              verbose = getOption("verbose", TRUE),
                              format = c("sound", "image"),
                              identified = FALSE,
                              verifiable = FALSE,
                              all_data = getOption("all_data", FALSE)) {
  arguments <- as.list(base::match.call())[-1]

  for (i in names(arguments)) {
    arguments[[i]] <- get(i)
  }

  check_results <- .check_arguments(args = arguments)
  checkmate::reportAssertions(check_results)

  org_format <- format <- rlang::arg_match(format)
  format <- switch(format, sound = "sounds", image = "photos")

  response <- try(httr::GET("https://www.inaturalist.org/"), silent = TRUE)
  if (inherits(response, "try-error") ||
      httr::http_error(response)) {
    .stop("No connection to INaturalist (check your internet connection!)")
  }

  content <- httr::content(response, as = "text")
  if (grepl("Could not connect to the database", content)) {
    .stop("INaturalist website is apparently down")
  }

  species <- term
  term <- gsub(" ", "%20", term)

  base_url <- paste0(
    "https://api.inaturalist.org/v1/observations?per_page=200&",
    "taxon_name=",
    term,
    "&",
    format,
    "=true",
    "&",
    "identified=",
    identified,
    "&",
    "verifiable=",
    verifiable
  )

  first_query <- jsonlite::fromJSON(base_url)
  total_results <- first_query$total_results
  if (total_results == 0) {
    if (verbose) {
      cat(paste(.color_text(
        paste0("No ", tolower(org_format), "s were found"), "failure"
      ), .add_emoji("sad")))
    }
    return(data.frame())
  } else {
    if (pb & verbose) {
      cat(paste(
        .color_text(
          paste0(
            "Obtaining metadata (",
            total_results,
            " matching observation(s) found)"
          ),
          "success"
        ),
        .add_emoji("happy"),
        ":\n"
      ))
    }

    offsets <- seq(0, total_results, by = 200)

    if (Sys.info()[1] == "Windows" & cores > 1) {
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores))
    } else {
      cl <- cores
    }

    query_output_list <- pblapply_sw_int(offsets, cl = cl, pbar = pb, function(offset) {
      query_output <- jsonlite::fromJSON(paste0(base_url, "&offset=", offset))

      if (is.null(query_output$results)) {
        return(NULL)
      }

      query_output$results <- lapply(seq_len(nrow(query_output$results)), function(u) {
        x <- as.data.frame(query_output$results[u, ])
        media_df <- if (format == "sounds")
          do.call(rbind, x$sounds)
        else
          do.call(rbind, x$photos)
        media_df <- media_df[!sapply(media_df, is.list)]
        media_df <- data.frame(media_df)
        names(media_df)[names(media_df) == "url"] <- "media-URL"

        x <- x[!sapply(x, is.list)]
        X_df <- data.frame(t(unlist(x)))
        X_df <- cbind(X_df, media_df)
        return(X_df)
      })

      common_names <- unique(unlist(lapply(query_output$results, names)))
      query_output$results <- lapply(query_output$results, function(e) {
        nms <- names(e)
        if (length(nms) != length(common_names)) {
          for (o in common_names[!common_names %in% nms]) {
            e <- data.frame(e,
                            NA,
                            stringsAsFactors = FALSE,
                            check.names = FALSE)
            names(e)[ncol(e)] <- o
          }
        }
        return(e)
      })

      output_df <- do.call(rbind, query_output$results)
      output_df$offset <- offset
      return(output_df)
    })

    common_names <- unique(unlist(lapply(query_output_list, names)))
    query_output_list <- lapply(query_output_list, function(e) {
      nms <- names(e)
      if (length(nms) != length(common_names)) {
        for (o in common_names[!common_names %in% nms]) {
          e <- data.frame(e,
                          NA,
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
          names(e)[ncol(e)] <- o
        }
      }
      return(e)
    })

    query_output_df <- do.call(rbind, query_output_list)
    colnames(query_output_df)[colnames(query_output_df) == "media-URL"] <- "file_url"

    split_location <- do.call(rbind, strsplit(as.character(query_output_df$location), ","))
    latitude <- split_location[, 1]
    longitude <- split_location[, 2]
    query_output_df$latitude <- latitude
    query_output_df$longitude <- longitude

    first_id_index <- which(names(query_output_df) == "id")[1]
    if (!is.na(first_id_index)) {
      names(query_output_df)[first_id_index] <- "key"
    }

    query_output_df$species <- species

    # add format
    query_output_df$file_extension <- sub(".*\\.", "", sub("\\?.*", "", query_output_df$file_url))

    # fix formatting
    query_output_df$file_extension <- .fix_extension(query_output_df$file_extension)

    # fix column names
    query_output_df$country <- NA
    query_output_df$date <- substr(x = query_output_df$time_observed_at,
                                   start = 1,
                                   stop = 10)

    # format output data frame column names
    query_output_df <- .format_query_output(
      X = query_output_df,
      call = base::match.call(),
      column_names = c(
        "location" = "locality",
        "time_observed_at" = "time"
                     ),
      all_data = all_data,
      format = org_format
    )

    replace_image_size <- function(file_url) {
      gsub("square", "original", file_url)
    }
    for (i in seq_len(nrow(query_output_df))) {
      query_output_df$file_url[i] <- replace_image_size(query_output_df$file_url[i])
    }

    query_output_df <- query_output_df[!is.na(query_output_df$file_url), ]

    # file_path <- file.path(tempdir(), paste0(term, ".rds"))
    return(query_output_df)
  }
}
