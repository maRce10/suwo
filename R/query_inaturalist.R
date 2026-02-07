#' Access 'iNaturalist' media file metadata
#'
#' `query_inaturalist` searches for metadata from
#' \href{https://www.inaturalist.org/}{iNaturalist}.
#' @inheritParams template_params
#' @param format Character vector with the media format to query for.
#' Currently 'image' and 'sound' are available. Can be set globally for
#' the current R session via the "suwo_format" option
#' (e.g. `options(suwo_format = "image")`). Required.
#' @param all_data Logical argument that determines if all data available
#' from database is shown in the results of search. Default is `TRUE`.
#' @param identified Logical argument to define if search results are
#' categorized as identified by inaturalist.
#' @param verifiable Logical argument to define if search results are
#' categorized as verifiable by inaturalist.
#' @export
#' @name query_inaturalist
#' @return The function returns a data frame with the metadata of the media
#' files matching the search criteria. If `all_data = TRUE`, all metadata
#' fields (columns) are returned. If `raw_data = TRUE`, the raw data as
#' obtained from the repository is returned (without any formatting).
#' @details This function queries for species observation info in the
#' open-access
#' online repository \href{https://www.inaturalist.org/}{iNaturalist}.
#' iNaturalist is a free, crowdsourced online platform for nature enthusiasts
#' to document and identify plants, animals, fungi, and other organisms in
#' the wild. Note that Inaturalist observations do not include a 'country'
#' field.
#' @examples
#' # search Bleeding Tooth mushroom images
# h_peckii <- query_inaturalist(species = 'Hydnellum peckii', format = "image")
#'
#' @references
#' iNaturalist. Available from https://www.inaturalist.org. Accessed [date]
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

query_inaturalist <- function(species = getOption("suwo_species"),
                      format = getOption("suwo_format", c("image", "sound")),
                      cores = getOption("mc.cores", 1),
                      pb = getOption("suwo_pb", TRUE),
                      verbose = getOption("suwo_verbose", TRUE),
                      all_data = getOption("suwo_all_data", FALSE),
                      raw_data = getOption("suwo_raw_data", FALSE),
                      identified = FALSE,
                      verifiable = FALSE) {

  arguments <- as.list(base::match.call())
 for (i in names(arguments)[-1]) arguments[[i]] <- get(i)

  check_results <- .check_arguments(fun = arguments[[1]], args = arguments)
  checkmate::reportAssertions(check_results)

  format <- rlang::arg_match(format)
  inat_format <- switch(format, sound = "sounds", image = "photos")

  if (!.checkconnection(verb = verbose, service = "inat")) {
    return(invisible(NULL))
  }

  base_url <- paste0(
    "https://api.inaturalist.org/v1/observations?",
    "per_page=200&",
    "taxon_name=", gsub(" ", "%20", species), "&",
    inat_format, "=true&",
    "identified=", identified, "&",
    "verifiable=", verifiable
  )

  first_query <- try(jsonlite::fromJSON(base_url), silent = TRUE)

  if (.is_error(first_query) || is.null(first_query$total_results)) {
    if (verbose) {
      .message(text = "Metadata could not be downloaded", as = "failure")
    }
    return(invisible(NULL))
  }

  total_results <- first_query$total_results

  if (total_results == 0) {
    if (verbose) {
      .message(text = "No matching records found", as = "failure")
    }
    return(invisible(NULL))
  }

  if (verbose) {
    .message(n = total_results, as = "success")
  }

  pages <- seq_len(ceiling(total_results / 200))

  if (Sys.info()[1] == "Windows" && cores > 1) {
    cl <- parallel::makePSOCKcluster(cores)
  } else {
    cl <- cores
  }

  query_output_list <- .pbapply_sw(
    X = pages,
    cl = cl,
    pbar = pb,
    FUN = function(x, Y = pages) {

      # set index to get the right page
      page <- Y[x]

      query_output <- try(
        jsonlite::fromJSON(paste0(base_url, "&page=", page)),
        silent = TRUE
      )

      if (.is_error(query_output) || is.null(query_output$results)) {
        return(NULL)
      }

      query_output$results <- lapply(
        seq_len(nrow(query_output$results)),
        function(u) {

          x <- as.data.frame(query_output$results[u, ])

          media_df <- if (format == "sound"){
            do.call(rbind, x$sounds)
            } else {
            do.call(rbind, x$photos)
              }

          media_df <- media_df[!vapply(media_df, is.list, logical(1))]
          media_df <- data.frame(media_df)

          x <- x[!vapply(x, is.list, logical(1))]
          X_df <- data.frame(t(unlist(x)))
          X_df <- cbind(X_df, media_df)

          # modify URL so it refers to original file
          if (format == "image") {
            X_df$url <-  gsub("square", "original", X_df$url)
          }

          return(X_df)
        }
      )

      .merge_data_frames(query_output$results)
    }
  )

  if (any(vapply(query_output_list, .is_error, logical(1)))) {
    if (verbose) {
      .message(text = "Metadata could not be downloaded", as = "failure")
    }
    return(invisible(NULL))
  }

  query_output_df <- .merge_data_frames(query_output_list)

  split_location <- do.call(
    rbind,
    strsplit(as.character(query_output_df$location), ",")
  )

  query_output_df$latitude  <- split_location[, 1]
  query_output_df$longitude <- split_location[, 2]

  first_id_index <- which(names(query_output_df) == "id")[1]
  if (!is.na(first_id_index)) {
    names(query_output_df)[first_id_index] <- "key"
  }

  query_output_df$key <- as.character(query_output_df$key)
  query_output_df$species <- species

  query_output_df$file_extension <- sub(
    ".*\\.",
    "",
    sub("\\?.*", "",
        query_output_df[, grep("url", names(query_output_df), value = TRUE)])
  )

  query_output_df$country <- NA

  query_output_df$date <- substr(
    x = query_output_df$time_observed_at,
    start = 1,
    stop = 10
  )

  query_output_df$user_name <- vapply(
    strsplit(query_output_df$attribution, ","),
    "[[",
    1,
    FUN.VALUE = character(1)
  )

  query_output_df$user_name[
    query_output_df$user_name != "no rights reserved"
  ] <- vapply(
    strsplit(
      query_output_df$user_name[
        query_output_df$user_name != "no rights reserved"
      ],
      ") "
    ),
    "[[",
    2,
    FUN.VALUE = character(1)
  )

  query_output_df <- .format_query_output(
    X = query_output_df,
    call = base::match.call(),
    column_names = c(
      "location" = "locality",
      "time_observed_at" = "time",
      "url" = "file_url"
    ),
    all_data = all_data,
    format = format,
    raw_data = raw_data
  )

  query_output_df <- query_output_df[!is.na(query_output_df$file_url), ]

  query_output_df
}
