#' Access 'inaturalist' recordings and metadata
#'
#' \code{query_inaturalist} searches for metadata from \href{https://www.inaturalist.org/}{inaturalist}.
#' @inheritParams template_params
#' @param format Character vector with the media format to query for. Currently 'image' and 'sound' are available.
#' @param identified Logical argument to define if search results are categorized as identified by inaturalist.
#' @param verifiable Logical argument to define if search results are categorized as verifiable by inaturalist.
#' @param all_data Logical argument that determines if all data available from database is shown in the results of search. Default is \code{TRUE}.
#' @export
#' @name query_inaturalist
#' @details This function queries for species observation info in the open-access
#' online repository \href{https://www.inaturalist.org/}{inaturalist}. iNaturalist is a free, crowdsourced online platform for nature enthusiasts to document and identify plants, animals, fungi, and other organisms in the wild.
#' @examples
#' \dontrun{
#' # search without downloading
# df1 <- query_inaturalist(species = 'Turdus plebejus', format = "sound")
#' View(df1)
#' }
#'
#' @references {
#' iNaturalist. Available from https://www.inaturalist.org. Accessed [date]
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

query_inaturalist <- function(species = getOption("species"),
                              cores = getOption("mc.cores", 1),
                              pb = getOption("pb", TRUE),
                              verbose = getOption("verbose", TRUE),
                              format = c("sound", "image"),
                              identified = FALSE,
                              verifiable = FALSE,
                              all_data = getOption("all_data", FALSE),
                              raw_data = getOption("raw_data", FALSE)) {
  arguments <- as.list(base::match.call())[-1]

  for (i in names(arguments)) {
    arguments[[i]] <- get(i)
  }

  check_results <- .check_arguments(args = arguments)
  checkmate::reportAssertions(check_results)

  format <- rlang::arg_match(format)
  inat_format <- switch(format, sound = "sounds", image = "photos")

  # Use the unified connection checker
  if (!.checkconnection("inat")) {
    return(invisible(NULL))
  }


  base_url <- paste0(
    "https://api.inaturalist.org/v1/observations?per_page=200&",
    "taxon_name=",
    gsub(" ", "%20", species),
    "&",
    inat_format,
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
      .failure_message(format = format)
    }
    return(invisible(NULL))
  }

  if (pb & verbose) {
    .success_message(n = total_results, format = format)
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
      media_df <- if (format == "sound")
        do.call(rbind, x$sounds)
      else
        do.call(rbind, x$photos)
      media_df <- media_df[!sapply(media_df, is.list)]
      media_df <- data.frame(media_df)

      x <- x[!sapply(x, is.list)]
      X_df <- data.frame(t(unlist(x)))
      X_df <- cbind(X_df, media_df)
      return(X_df)
    })

    # combine into a single data frame
    output_df <- .merge_data_frames(query_output$results)

    output_df$offset <- offset
    return(output_df)
  })

  # combine into a single data frame
  query_output_df <- .merge_data_frames(query_output_list)

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
  query_output_df$file_extension <- sub(".*\\.", "", sub("\\?.*", "", query_output_df[, grep("url", names(query_output_df), value = TRUE)]))

  # fix column names
  query_output_df$country <- NA
  query_output_df$date <- substr(x = query_output_df$time_observed_at,
                                 start = 1,
                                 stop = 10)

  # fix recordist name
  query_output_df$user_name <- sapply(strsplit(query_output_df$attribution, ","), "[[", 1)

  query_output_df$user_name[query_output_df$user_name != "no rights reserved"] <- sapply(strsplit(query_output_df$user_name[query_output_df$user_name != "no rights reserved"], ") "), "[[", 2)

  # format output data frame column names
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

  # file_path <- file.path(tempdir(), paste0(species, ".rds"))
  return(query_output_df)

}
