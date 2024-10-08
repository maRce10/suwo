#' Access 'Xeno-Canto' recordings and metadata
#'
#' \code{query_xenocanto} searches for metadata from \href{https://www.xeno-canto.org/}{Xeno-Canto}.
#' @inheritParams template_params
#' @param term Character vector of length one indicating the scientific of the taxonomic group (species, genus, or family)
#'  to query for in the 'Xeno-Canto' database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}.
#'  More complex queries can be done by using search terms that follow the
#'  xeno-canto advance query syntax. This syntax uses tags to search within a particular aspect of the recordings
#'  (e.g. country, location, sound type). Tags are of the form tag:searchterm'. For instance, 'type:song'
#'  will search for all recordings in which the sound type description contains the word 'song'.
#'  Several tags can be included in the same query. The query "phaethornis cnt:belize' will only return
#'  results for birds in the genus \emph{Phaethornis} that were recorded in Belize. Queries are case insensitive. Make sure taxonomy related tags (Genus or scientific name) are found first in multi-tag queries. See \href{https://www.xeno-canto.org/help/search}{Xeno-Canto's search help} for a full description and see examples below
#'  for queries using terms with more than one word.
#' @return The function returns a data frame with the following recording information: recording ID,
#' Genus, Specific epithet, Subspecies, English name, Recordist, Country, Locality, Latitude,
#' Longitude, Vocalization type, Audio file, License, URL, Quality, Time, Date. Sound files in .mp3
#' format are downloaded into the working directory if download = \code{TRUE}.
#' @export
#' @name query_xenocanto
#' @details This function queries for avian vocalization recordings in the open-access
#' online repository \href{https://www.xeno-canto.org/}{Xeno-Canto}. It can return recordings metadata
#' or download the associated sound files. Complex queries can be done by using search terms that follow the
#'  xeno-canto advance query syntax (check "term" argument description).
#'  Files are double-checked after downloading and "empty" files are re-downloaded.
#'  File downloading process can be interrupted and resume later as long as the working directory is the same.
#' @seealso \code{\link{query_gbif}}, \code{\link{query_wikiaves}}, \code{\link{query_inaturalist}}, \code{\link{query_observation}}
#' \href{https://marce10.github.io/2016/12/22/Download_a_single_recording_for_each_species_in_a_site_from_Xeno-Canto.html}{blog post on accessing Xeno-Canto recordings}
#' @examples
#' \dontrun{
#' # search without downloading
# df1 <- query_xenocanto(term = "Phaethornis anthophilus")
#'
#' ## search using xeno-canto advance query ###
#' orth.pap <- query_xenocanto(term = "gen:orthonyx cnt:papua loc:tari")
#'
#' # use quotes for queries with more than 1 word (e.g. Costa Rica),note that the
#' # single quotes are used for the whole 'term' and double quotes for the 2-word term inside
#' # Phaeochroa genus in Costa Rica
#' phae.cr <- query_xenocanto(term = 'gen:phaeochroa cnt:"costa rica"')
#'
#' # several terms can be searched for in the same field
#' # search for all female songs in sound type
#' femsong <- query_xenocanto(term = "type:song type:female")
#' }
#'
#' @references {
#' Planqué, Bob, & Willem-Pier Vellinga. 2008. Xeno-canto: a 21st-century way to appreciate Neotropical bird song. Neotrop. Birding 3: 17-23.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on nov-16-2016 (MAS)

query_xenocanto <-
  function(term,
           cores = getOption("mc.cores", 1),
           pb = getOption("pb", TRUE),
           verbose = getOption("verbose", TRUE),
          all_data = getOption("all_data", TRUE)) {
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

    # check internet connection
    a <- try(RCurl::getURL("www.xeno-canto.org"), silent = TRUE)
    if (is(a, "try-error")) {
      .stop("No connection to xeno-canto.org (check your internet connection!)")
    }

    if (a == "Could not connect to the database") {
      .stop("xeno-canto.org website is apparently down")
    }

    # search recs in xeno-canto (results are returned in pages with 500 recordings each)
    if (pb & verbose) {
      cat(paste(.color_text(paste0("Obtaining metadata (matching observation(s) found)"), "success"), .add_emoji("happy"), ":\n"))
    }

    # format query term
    if (grepl("\\:", term)) { # if using advanced search

      # replace first space with %20 when using full species name
      first_colon_pos <- gregexpr(":", term)[[1]][1]
      spaces_pos <- gregexpr(" ", term)[[1]]

      if (length(spaces_pos) > 1) {
        if (all(spaces_pos[1:2] < first_colon_pos)) {
          term <- paste0(substr(term, start = 0, stop = spaces_pos - 1), "%20", substr(term, start = spaces_pos + 1, stop = nchar(term)))
        }
      }

      # replace remaining spaces with "&"
      term <- gsub(" ", "+", term)
    } else {
      term <- gsub(" ", "+", term)
    }

    # initialize search
    q <-
      jsonlite::fromJSON(paste0(
        "https://www.xeno-canto.org/api/2/recordings?query=",
        term
      ))

    if (as.numeric(q$numRecordings) == 0) {
      if (verbose) {
        cat(paste(
          .color_text("No audios were found", "failure"),
          .add_emoji("sad"),
          "\n"
        ))
      }
    } else {
      nms <-
        c(
          "id",
          "gen",
          "sp",
          "ssp",
          "en",
          "rec",
          "cnt",
          "loc",
          "lat",
          "lng",
          "type",
          "file",
          "lic",
          "url",
          "q",
          "time",
          "date"
        )

      ### loop over pages
      # set clusters for windows OS
      if (Sys.info()[1] == "Windows" & cores > 1) {
        cl <-
          parallel::makePSOCKcluster(getOption("cl.cores", cores))
      } else {
        cl <- cores
      }

      records_list <-
        pblapply_sw_int(
          pbar = pb,
          X = 1:q$numPages,
          cl = cl,
          FUN = function(y) {
            # search for each page
            a <-
              rjson::fromJSON(
                file = paste0(
                  "https://www.xeno-canto.org/api/2/recordings?query=",
                  term,
                  "&page=",
                  y
                )
              )

            # put together as data frame
            d <-
              lapply(1:length(a$recordings), function(z) {
                data.frame(t(unlist(
                  a$recordings[[z]]
                )))
              })

            d2 <- lapply(d, function(x) {
              if (!all(nms %in% names(x))) {
                dif <- setdiff(nms, names(x))
                mis <- rep(NA, length(dif))
                names(mis) <- dif
                return(cbind(x, t(mis)))
              }
              return(x)
            })

            # determine all column names in all pages
            cnms <- unique(unlist(lapply(d2, names)))

            # add columns that are missing to each selection table
            d3 <- lapply(d2, function(X) {
              nms <- names(X)
              if (length(nms) != length(cnms)) {
                for (i in cnms[!cnms %in% nms]) {
                  X <-
                    data.frame(X,
                      NA,
                      stringsAsFactors = FALSE,
                      check.names = FALSE
                    )
                  names(X)[ncol(X)] <- i
                }
              }
              return(X)
            })

            e <- do.call(rbind, d3)

            return(e)
          }
        )

      # determine all column names in all pages
      cnms <- unique(unlist(lapply(records_list, names)))

      # add columns that are missing to each selection table
      records_list2 <- lapply(records_list, function(X) {
        nms <- names(X)
        if (length(nms) != length(cnms)) {
          for (i in cnms[!cnms %in% nms]) {
            X <-
              data.frame(X,
                NA,
                stringsAsFactors = FALSE,
                check.names = FALSE
              )
            names(X)[ncol(X)] <- i
          }
        }
        return(X)
      })

      # save results in a single data frame
      results <- do.call(rbind, records_list2)

      # convert factors to characters
      indx <- sapply(results, is.factor)
      results[indx] <- lapply(results[indx], as.character)

      # order columns
      results <- results[, order(match(names(results), nms))]

      names(results)[match(
        c(
          "id",
          "gen",
          "sp",
          "ssp",
          "en",
          "rec",
          "cnt",
          "loc",
          "lat",
          "lng",
          "alt",
          "type",
          "file",
          "lic",
          "url",
          "q",
          "length",
          "time",
          "date",
          "uploaded",
          "rmk",
          "bird.seen",
          "playback.used"
        ),
        names(results)
      )] <-
        c(
          "id",
          "genus",
          "specific.epithet",
          "subspecies",
          "english.name",
          "recordist",
          "country",
          "location",
          "latitude",
          "longitude",
          "altitude",
          "vocalization.type",
          "audio_file",
          "license",
          "file_url",
          "quality",
          "length",
          "time",
          "date",
          "uploaded",
          "remarks",
          "bird.seen",
          "playback.used"
        )[which(
          c(
            "id",
            "gen",
            "sp",
            "ssp",
            "en",
            "rec",
            "cnt",
            "loc",
            "lat",
            "lng",
            "alt",
            "type",
            "file",
            "lic",
            "url",
            "q",
            "length",
            "time",
            "date",
            "uploaded",
            "rmk",
            "bird.seen",
            "playback.used"
          ) %in% names(results)
        )]

      # rename also columns
      names(results) <- gsub("also", "other.species", names(results))
      # rename
      names(results) <- gsub("sono.", "spectrogram.", names(results))
      # Add repository ID
      results$repository <- "XC"
      # remove duplicates
      results <- results[!duplicated(results$id), ]


      if (pb & verbose) {
        cat(.color_text(paste(nrow(results), "audio(s) found"), "success"), .add_emoji("happy"), "\n")
      }
    }

    if (as.numeric(q$numRecordings) > 0) {
      # convert lat long to numbers
      results$latitude <- as.numeric(results$latitude)
      results$longitude <- as.numeric(results$longitude)

      # create species name column
      results$species <- paste(results$genus, results$specific.epithet, sep = "_")
      # rename id
      names(results)[names(results) == "id"] <- "key"
      # fix url
      results$file_url <- paste0("https:", results$file_url, "/download")

      if (!all_data) {
        results <- results[, c("location", "latitude", "longitude", "file_url", "repository", "key", "species", "date", "country")]
      }

      # Add a timestamp and search query attribute
      search_time <- Sys.time()
      attr(query_output_df, "search_time") <- search_time
      attr(query_output_df, "query_term") <- term
      attr(query_output_df, "query_all_data") <- all_data

      return(droplevels(results))
    }
  }
