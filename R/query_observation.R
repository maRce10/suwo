#' Access 'observation' recordings and metadata
#'
#' \code{query_observation} searches for metadata from \href{https://www.observation.org/}{observation}.
#' @usage query_observation(term, type = c("sound", "still image", "moving image", "interactive resource"), cores = 1, pb = TRUE)
#' @param term Character vector of length one indicating the genus, or genus and
#'  species, to query 'observation' database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}.
#'  @param type Character vector with media type to query for. Options are #######. Required.
#' @param cores Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param dataset see \url{https://observation.org/api/v1/species/search/?q=}
#' @return If X is not provided the function returns a data frame with the following media information: id, scientific_name, name, group, group_name, status, rarity, photo, info_text, permalink, determination_requirements, file_url, page, repository
#' @export
#' @name query_observation
#' @details This function queries for species observation info in the open-access
#' online repository \href{https://www.observation.org/}{observation}. It can return media metadata.
#' @examples
#' \dontrun{
#' # search without downloading
# df1 <- query_observation(term = 'Turdus iliacus', type = "Sound", cores = 4)
#' View(df1)
#'
#' }
#'
#' @references {
#'
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
query_observation <-
  function(term = NULL,
           cores = 1,
           pb = TRUE,
           verbose = TRUE
  ) {

    # # type must be supplied
    # if (is.null(type))
    #   stop("'type' must be supplied")

    # term must be supplied
    if (is.null(term))
      stop("'term' must be supplied")

    #check internet connection
    # a <- try(RCurl::getURL("https://www.observation.org/"), silent = TRUE)
    # if (is(a, "try-error"))
    #   stop("No connection to observation (check your internet connection!)")
    #
    # if (a == "Could not connect to the database")
    #   stop("observation website is apparently down")

    # If cores is not numeric
    if (!is.numeric(cores))
      stop("'cores' must be a numeric vector of length 1")
    if (any(!(cores %% 1 == 0), cores < 1))
      stop("'cores' should be a positive integer")

    #format JSON
    term <- gsub(" ", "%20", term)


    srch_trm <- paste0("https://observation.org/api/v1/species/search/?limit=100&", "q=", term)

    base.srch.pth <- jsonlite::fromJSON(srch_trm)

    ####org_type in tolower

    # message if nothing found
    if (base.srch.pth$count == 0 & verbose)
      cat(paste(colortext(paste0("No ", tolower("photo"), "s were found"), "failure"), add_emoji("sad"))) else {

        # message number of results
        if (pb & verbose)
          cat(paste(colortext(paste0("Obtaining metadata (", base.srch.pth$total_results, " matching observation(s) found)"), "success"), add_emoji("happy"), ":\n"))

        # get total number of pages
        offsets <- (seq_len(ceiling(base.srch.pth$count / 100)) - 1) * 100



        # set clusters for windows OS
        if (Sys.info()[1] == "Windows" & cores > 1)
          cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores)) else cl <- cores


          query_output_list <- pblapply_sw_int(offsets, cl = 1, pbar = pb, function(i)
          {
            query_output <- jsonlite::fromJSON(paste0(srch_trm, "&offset=", i))

            # format as list of data frame
            query_output$results <- lapply(seq_len(nrow(query_output$results)), function(u) {

              x <- query_output$results[u, ]

              # media_df <- do.call(rbind, media_list)
              media_URL <- x$photo

              # remove lists
              x <- x[!sapply(x, is.list)]

              # make it data frame
              X_df <- data.frame(t(unlist(x)))

              # add media details
              X_df <- cbind(X_df, media_URL)

              return(X_df)
            })

            # get common names to all data frames in X
            common_names <- unique(unlist(lapply(query_output$results, names)))

            # add missing columns to all data frames in X
            query_output$results <- lapply(query_output$results, function(e){

              nms <- names(e)
              if (length(nms) != length(common_names))
                for (o in common_names[!common_names %in% nms]) {
                  e <-
                    data.frame(e,
                               NA,
                               stringsAsFactors = FALSE,
                               check.names = FALSE)
                  names(e)[ncol(e)] <- o
                }
              return(e)
            })

            # all results in a single data frame
            output_df <- do.call(rbind, query_output$results)

            output_df$page <- i/100

            return(output_df)
          })

          # get common names to all data frames in X
          common_names <- unique(unlist(lapply(query_output_list, names)))

          # add missing columns to all data frames in X
          query_output_list<- lapply(query_output_list, function(e){

            nms <- names(e)
            if (length(nms) != length(common_names))
              for (o in common_names[!common_names %in% nms]) {
                e <-
                  data.frame(e,
                             NA,
                             stringsAsFactors = FALSE,
                             check.names = FALSE)
                names(e)[ncol(e)] <- o
              }
            return(e)
          })

          # all results in a single data frame
          query_output_df <- do.call(rbind, query_output_list)

          #Change column name for media download function
          colnames(query_output_df)[colnames(query_output_df) == "media_URL"] ="file_url"

          #Add repository ID
          query_output_df$repository <- "Observation"


          return(query_output_df)
      }
  }
