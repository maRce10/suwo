#' Access 'inaturalist' recordings and metadata
#'
#' \code{query_inaturalist} searches for metadata from \href{https://www.inaturalist.org/}{inaturalist}.
#' @usage query_inat(term, type = c("sound", "still image", "moving image", "interactive resource"), cores = 1, pb = TRUE)
#' @param term Character vector of length one indicating the genus, or genus and
#'  species, to query 'inaturalist' database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}.
#'  @param type Character vector with media type to query for. Options are 'sound', 'stillimage', 'movingimage' and 'interactiveresource'. Required.
#' @param cores Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param dataset see \url{https://api.inaturalist.org/v1/Search?q=}
#' @return If X is not provided the function returns a data frame with the following media information: ############### A, B, C
#' @export
#' @name query_inaturalist
#' @details This function queries for species observation info in the open-access
#' online repository \href{https://www.inaturalist.org/}{inaturalist}. It can return media metadata.
#' @examples
#' \dontrun{
#' # search without downloading
# df1 <- query_inaturalist(term = 'Turdus iliacus', type = "Sound", cores = 4)
#' View(df1)
#'
#' }
#'
#' @references {
#'
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

query_inaturalist <-
  function(term = NULL,
           cores = 1,
           pb = TRUE,
           verbose = TRUE
  ) {

    # # type must be supplied
    if (is.null(type))
      stop2("'type' must be supplied")

    org_type <- match.arg(type)

    type <- switch(type,
                   sound = "sounds",
                   `still image` = "photos")

    # term must be supplied
    if (is.null(term))
      stop2("'term' must be supplied")

    #check internet connection
    a <- try(RCurl::getURL("https://www.inaturalist.org/"), silent = TRUE)
    if (is(a, "try-error"))
      stop2("No connection to INaturalist (check your internet connection!)")

    if (a == "Could not connect to the database")
    # If cores is not numeric
    if (!is.numeric(cores))
      stop2("'cores' must be a numeric vector of length 1")
    if (any(!(cores %% 1 == 0), cores < 1))
      stop2("'cores' should be a positive integer")

    #format JSON
    term <- gsub(" ", "%20", term)


    # srch_trm <- paste0("https://api.inaturalist.org/v1/observations?per_page=200&", "taxon_name=", term)

    srch_trm <- paste0(
      "https://api.inaturalist.org/v1/observations?per_page=200&",
      "taxon_name==", term, "&",
      type, "=true"
    )

    base.srch.pth <- jsonlite::fromJSON(srch_trm)

    ####org_type in tolower

    # message if nothing found
    if (base.srch.pth$total_results == 0 & verbose)
      cat(paste(colortext(paste0("No ", tolower("photos"), "s were found"), "failure"), add_emoji("sad"))) else {

        # message number of results
        if (pb & verbose)
          cat(paste(colortext(paste0("Obtaining metadata (", base.srch.pth$total_results, "matching observation(s) found)"), "success"), add_emoji("happy"), ":\n"))


        # set clusters for windows OS
        if (Sys.info()[1] == "Windows" & cores > 1)
          cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores)) else cl <- cores


          query_output_list <- pblapply_sw_int(cl = 1, pbar = pb, function(i)
          {
            query_output <- jsonlite::fromJSON(paste0(srch_trm, "&page=", i))

            # format as list of data frame
            query_output$results <- lapply(seq_len(nrow(query_output$results)), function(u) {

              x <- query_output$results[u, ]

              # media_df <- do.call(rbind, media_list)
              media_df <- do.call(rbind, x$record$taxon_photos)

              # fix identifier column name
              # names(media_df)[names(media_df) == "photo$original_url"] <- "URL"
              # names(media_df) <- paste0("media-", names(media_df))

              # media data frame with image details
              media_df <- data.frame(media_df$photo)

              # remove lists
              x <- x$record
              media_df <- media_df[!sapply(media_df, is.list)]

              # make it data frame
              X_df <- data.frame(t(unlist(x)))

              # add media details
              X_df <- cbind(X_df, media_df)



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

            output_df$page <- i

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
          colnames(query_output_df)[colnames(query_output_df) == "media-URL"] ="file_url"

          #Add repository ID
          query_output_df$repository <- "INAT"


          return(query_output_df)
      }
}


