#' Access 'observation' recordings and metadata
#'
#' \code{query_observation} searches for metadata from \href{https://www.observation.org/}{observation}.
#' @usage query_observation(term = NULL, type = c("sound", "still image"),
#' cores = 1, pb = TRUE, verbose = TRUE, token = NULL)
#' @param term Character vector of length one indicating the
#'  species, to query 'observation' database. For example \emph{Phaethornis longirostris}.
#' @param type Character vector with media type to query for. Currently 'still image' and 'sound' are available.
#' @param cores Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param verbose Logical argument that determines if text is shown in console. Default is \code{TRUE}.
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
# df1 <- query_observation(term = 'Turdus iliacus', type = "Sound", cores = 4)
#' View(df1)
#' }
#'
#' @references {
#'
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
query_observation <-
  function(term = NULL,
           type = c("sound", "still image"),
           cores = 1,
           pb = TRUE,
           verbose = TRUE,
           token = NULL) {
    # check arguments
    arguments <- as.list(base::match.call())[-1]

    # add objects to argument names
    for (i in names(arguments)) {
      arguments[[i]] <- get(i)
    }

    # check each arguments
    check_results <- check_arguments(args = arguments)

    # report errors
    checkmate::reportAssertions(check_results)

    # term must be supplied
    if (is.null(term)) {
      stop2("'term' must be supplied")
    }

  #Check if token is available
    if (is.null(token)){
      stop2("Invalid token for observation.org")
    }

    # type must be supplied
    if (is.null(type)) {
      stop2("'type' must be supplied")
    }
    org_type <- match.arg(type)

    type <- switch(type,
      sound = "Sound",
      `still image` = "StillImage",
      `moving image` = "MovingImage",
      `interactive resource` = "InteractiveResource"
    )

    if (tolower(Sys.info()[["sysname"]]) != "windows"){
      # check internet connection
      a <- try(RCurl::getURL("https://observation.org"), silent = TRUE)
      if (is(a, "try-error")) {
        stop2("No connection to observation.org (check your internet connection!)")
      }

      if (a == "Could not connect to the database") {
        stop2("observation.org website is apparently down")
      }
    }


    # format JSON
    term <- gsub(" ", "%20", term)

    srch_trm <- paste0("https://observation.org/api/v1/species/search/?", "q=", term)

    base.srch.pth <- jsonlite::fromJSON(srch_trm)

    # If species not found in repository
    if (base.srch.pth$count == 0) {
      stop2("Species was not found in database")
    }

    # Set the species ID and API endpoint URL
    species_id <- base.srch.pth$results$id
    url_inquiry <- paste0("https://observation.org/api/v1/species/", species_id, "/observations/?limit=100")

    # Set the authorization header with your bearer token
    bearer_token <- token
    headers <- c("Authorization" = paste("Bearer", bearer_token))

    # Make the GET request and retrieve the response
    dataURL <- RCurl::getURL(url_inquiry, httpheader = headers)

    # JSON format
    data <- jsonlite::fromJSON(dataURL)

    data_org <- data

    if (data$count == 0) {
      if (verbose) {
        cat(paste(colortext(paste0("No ", tolower(org_type), "s were found"), "failure"), add_emoji("sad")))
      }
    } else {
      # message number of results
      if (pb & verbose) {
        cat(paste(colortext(paste0("Obtaining metadata (", data$count, " candidate observation(s) found)"), "success"), add_emoji("happy"), ":\n"))
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
      srch_trm <- paste0("https://observation.org/api/v1/species/", species_id, "/observations/?limit=100")

      dataURL <- getURL(paste0(srch_trm, "&offset=", i), httpheader = headers)

      # JSON format
      data <- fromJSON(dataURL)

      # format as list of data frame
      data$results <- lapply(seq_len(nrow(data$results)), function(u) {
        x <- data$results[u, ]

        if (type == "StillImage") {
          media_URL <- if (length(x$photos[[1]]) > 0) {
            unlist(x$photos)
          } else {
            NA
          }
        }

        if (type == "Sound") {
          media_URL <- if (length(x$sounds[[1]]) > 0) {
            unlist(x$sounds)
          } else {
            NA
          }
        }

        # remove lists
        x <- x[!sapply(x, is.list)]

        # make it data frame
        # X_df <- data.frame(t(unlist(x)))

        # add media details
        X_df <- data.frame(x, media_URL, row.names = seq_len(length(media_URL)))

        # remove NAs
        X_df <- X_df[!is.na(X_df$media_URL), ]


        X_df$species_name <- if (nrow(X_df) > 0) data_org$results$species_detail$scientific_name[u] else vector(mode = "character")

        return(X_df)
      })

      # get common names to all data frames in X
      common_names <- unique(unlist(lapply(data$results, names)))

      # add missing columns to all data frames in X
      data$results <- lapply(data$results, function(e) {
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
      output_df <- do.call(rbind, data$results)

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
    colnames(query_output_df)[colnames(query_output_df) == "media_URL"] <- "file_url"
    colnames(query_output_df)[colnames(query_output_df) == "id"] <- "key"
    colnames(query_output_df)[colnames(query_output_df) == "species"] <- "species_code"
    colnames(query_output_df)[colnames(query_output_df) == "species_name"] <- "species"
    # Add repository ID
    query_output_df$repository <- "Observation"

    return(query_output_df)
  }
