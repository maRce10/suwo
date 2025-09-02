#' Access 'observation' recordings and metadata
#'
#' \code{query_observation} searches for metadata from \href{https://www.observation.org/}{observation}.
#' @inheritParams template_params
#' @param format Character vector with the media format to query for. Currently 'image' and 'sound' are available.
#' @param token Character refering to the token assigned by Observation.org as authorization for searches.
#' @return If all_data is not provided the function returns a data frame with the following media
#' information: id, scientific_name, name, group, group_name, status, rarity, photo,
#' info_text, permalink, determination_requirements, file_url, repository
#' @export
#' @name query_observation
#' @details This function queries for species observation info in the open-access
#' online repository \href{https://www.observation.org/}{observation}. It can return media metadata.
#' @examples
#' \dontrun{
#' # search without downloading
# df1 <- query_observation(term = 'Turdus iliacus', format = "Sound", cores = 4, token = ".....")
#' View(df1)
#' }
#'
#' @references {
#' Observation.org, Observation International and local partners. Available at https://observation.org. Retrieved June 30, 2022
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
query_observation <-
  function(term,
           format = c("sound", "image"),
           cores = getOption("mc.cores", 1),
           pb = getOption("pb", TRUE),
           verbose = getOption("verbose", TRUE),
           token = NULL,
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
    format <- rlang::arg_match(format)

    obs_format <- switch(
      format,
      sound = "Sound",
      `image` = "StillImage")


    # Check internet connection using httr and error handling
    response <- try(httr::GET("https://observation.org"), silent = TRUE)
    if (inherits(response, "try-error") ||
        httr::http_error(response)) {
      cat("No connection to observation.org (check your internet connection!)")
      return(invisible(NULL))
    }

    content <- httr::content(response, as = "text")
    if (grepl("Could not connect to the database", content)) {
      cat("observation.org website is apparently down")
      return(invisible(NULL))
    }
    # format JSON
    srch_trm <- paste0("https://observation.org/api/v1/species/search/?",
                       "q=",
                       gsub(" ", "%20", term))

    base.srch.pth <- jsonlite::fromJSON(srch_trm)

    # If species not found in repository
    if (base.srch.pth$count == 0) {
      cat("Species was not found in database")
      return(invisible(NULL))
    }

    # Check if token is available
    if (is.null(token)) {
      cat("Invalid token for observation.org")
      return(invisible(NULL))
      }

    # Set the species ID and API endpoint URL
    species_id <- base.srch.pth$results$id
    url_inquiry <- paste0(
      "https://observation.org/api/v1/species/",
      species_id,
      "/observations/?limit=100"
    )

    # Set the authorization header with your bearer token
    bearer_token <- token
    headers <- c("Authorization" = paste("Bearer", bearer_token))

    # Make the GET request and retrieve the response content as text
    response <- httr::GET(url_inquiry, httr::add_headers(.headers = headers))
    dataURL <- httr::content(response, "text", encoding = "UTF-8")


    # JSON format
    data <- jsonlite::fromJSON(dataURL)

    data_org <- data

    if (data$count == 0) {
      if (verbose) {
        .failure_message(format = format)
      }
      return(invisible(NULL))
    } else {
      # message number of results
      if (pb & verbose) {
        .success_message(n = data$count, format = format)
      }
    }

    # get total number of pages
    offsets <- (seq_len(ceiling(data$count / 100)) - 1) * 100

    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & cores > 1) {
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores))
    } else {
      cl <- cores
    }


    query_output_list <- pblapply_sw_int(offsets, cl = 1, pbar = pb, function(i) {
      # print()
      #
      srch_trm <- paste0(
        "https://observation.org/api/v1/species/",
        species_id,
        "/observations/?limit=100"
      )

      dataURL <- getURL(paste0(srch_trm, "&offset=", i), httpheader = headers)

      # JSON format
      data <- fromJSON(dataURL)

      # format as list of data frame
      data$results <- lapply(seq_len(nrow(data$results)), function(u) {
        x <- data$results[u, ]

        if (obs_format == "StillImage") {
          media_URL <- if (length(x$photos[[1]]) > 0) {
            unlist(x$photos)
          } else {
            NA
          }
        }

        if (obs_format == "Sound") {
          media_URL <- if (length(x$sounds[[1]]) > 0) {
            unlist(x$sounds)
          } else {
            NA
          }
        }

        # remove lists
        x <- x[!sapply(x, is.list)]

        # add media details
        X_df <- data.frame(x, media_URL, row.names = seq_len(length(media_URL)))

        # remove NAs
        X_df <- X_df[!is.na(X_df$media_URL), ]


        X_df$species_name <- if (nrow(X_df) > 0)
          data_org$results$species_detail$scientific_name[u]
        else
          vector(mode = "character")

        return(X_df)
      })

      # combine into a single data frame
      output_df <- .merge_data_frames(data$results)

      return(output_df)
    })

    # combine into a single data frame
    query_output_df <- .merge_data_frames(query_output_list)

    # format output data frame column names
    query_output_df <- .format_query_output(
      X = query_output_df,
      call = base::match.call(),
      column_names = c(
        "media-URL" = "file_url",
        "id" = "key",
        "species" = "species_code",
        "species_name" = "species"
      ),
      all_data = all_data,
      format = format
    )

    return(query_output_df)
  }
