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

query_xenocanto <-
  function(term,
           cores = getOption("mc.cores", 1),
           pb = getOption("pb", TRUE),
           verbose = getOption("verbose", TRUE),
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

    # Check internet connection
    response <- try(httr::GET("www.xeno-canto.org"), silent = TRUE)
    if (inherits(response, "try-error") ||
        httr::http_error(response)) {
      .stop("No connection to xeno-canto.org (check your internet connection!)")
    }

    content <- httr::content(response, as = "text")
    if (grepl("Could not connect to the database", content)) {
      .stop("xeno-canto.org website is apparently down")
    }

    # save original term
    org_term <- term

    # format query term
    if (grepl("\\:", term)) {
      first_colon_pos <- gregexpr(":", term)[[1]][1]
      spaces_pos <- gregexpr(" ", term)[[1]]

      if (length(spaces_pos) > 1) {
        if (all(spaces_pos[1:2] < first_colon_pos)) {
          term <- paste0(
            substr(term, start = 0, stop = spaces_pos - 1),
            "%20",
            substr(term, start = spaces_pos + 1, stop = nchar(term))
          )
        }
      }

      term <- gsub(" ", "+", term)
    } else {
      term <- gsub(" ", "+", term)
    }

    # initialize search
    query <-
      jsonlite::fromJSON(paste0(
        "https://www.xeno-canto.org/api/2/recordings?query=",
        term
      ))

    if (as.numeric(query$numRecordings) == 0) {
      if (verbose) {
        cat(paste(
          .color_text("No audios were found", "failure"),
          .add_emoji("sad"),
          "\n"
        ))
      }
      return(data.frame()) # empty df
    } else {
      ### loop over pages
      if (Sys.info()[1] == "Windows" & cores > 1) {
        cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores))
      } else {
        cl <- cores
      }

      records_list <-
        pblapply_sw_int(
          pbar = pb,
          X = seq_len(query$numPages),
          cl = cl,
          FUN = function(y) {
            query_output <-
              jsonlite::fromJSON(
                txt = paste0(
                  "https://www.xeno-canto.org/api/2/recordings?query=",
                  term,
                  "&page=",
                  y
                )
              )

            # Handle 'also'
            if (!is.null(query_output$recordings$also)) {
              query_output$recordings$also <- sapply(query_output$recordings$also, function(x) {
                if (is.null(x) || all(is.na(x))) {
                  return(NA_character_)
                } else {
                  paste(x, collapse = "-")
                }
              })
            }

            # sonograms
            if (!is.null(query_output$recordings$sono)) {
              sono_df <- as.data.frame(query_output$recordings$sono)
              names(sono_df) <- paste("sonogram", names(sono_df), sep = "_")
            } else {
              sono_df <- data.frame(sonogram_full = NA_character_,
                                    sonogram_small = NA_character_,
                                    sonogram_med = NA_character_)
            }

            # oscillograms
            if (!is.null(query_output$recordings$osci)) {
              osci_df <- as.data.frame(query_output$recordings$osci)
              names(osci_df) <- paste("oscillogram", names(osci_df), sep = "_")
            } else {
              osci_df <- data.frame(oscillogram_full = NA_character_,
                                    oscillogram_small = NA_character_,
                                    oscillogram_med = NA_character_)
            }

            query_output$recordings$sono <- query_output$recordings$osci <- NULL
            query_output <- cbind(query_output$recordings, sono_df, osci_df)

            return(query_output)
          }
        )

      # unify colnames
      pooled_column_names <- unique(unlist(lapply(records_list, names)))
      records_list2 <- lapply(records_list, function(X) {
        nms <- names(X)
        if (length(nms) != length(pooled_column_names)) {
          for (i in pooled_column_names[!pooled_column_names %in% nms]) {
            X[[i]] <- NA
          }
        }
        return(X)
      })

      query_output_df <- do.call(rbind, records_list2)

      if (as.numeric(query$numRecordings) > 0) {
        indx <- sapply(query_output_df, is.factor)
        query_output_df[indx] <- lapply(query_output_df[indx], as.character)

        query_output_df$species <- paste(
          ifelse(is.na(query_output_df$gen), "", query_output_df$gen),
          ifelse(is.na(query_output_df$sp), "", query_output_df$sp),
          sep = " "
        )
        query_output_df$species <- trimws(query_output_df$species)
        query_output_df$species[query_output_df$species == ""] <- NA_character_

        query_output_df$file_extension <- sub(".*\\.", "", query_output_df$`file-name`)
        query_output_df$file_extension <- .fix_extension(query_output_df$file_extension)

        query_output_df$repository <- "Xeno-Canto"

        # === SAFE RENAMING FIX ===
        old_names <- c("id", "gen", "sp", "ssp", "en", "rec", "cnt", "loc", "lat", "lng", "alt",
                       "type", "file", "lic", "url", "q", "length", "time", "date", "uploaded",
                       "rmk", "bird.seen", "playback.used")

        new_names <- c("id", "genus", "specific.epithet", "subspecies",
                       "english.name", "recordist", "country", "location", "latitude", "longitude", "altitude",
                       "vocalization.type", "audio_file", "license", "file_url", "quality",
                       "length", "time", "date", "uploaded", "remarks", "bird.seen", "playback.used")

        rename_map <- setNames(new_names, old_names)
        rename_map <- rename_map[names(rename_map) %in% names(query_output_df)]

        names(query_output_df)[match(names(rename_map), names(query_output_df))] <- rename_map
        # === END FIX ===

        query_output_df <- query_output_df[!duplicated(query_output_df$id), ]

        if (pb & verbose) {
          cat(.color_text(paste(
            nrow(query_output_df), "audio(s) found"
          ), "success"),
          .add_emoji("happy"),
          "\n")
        }
      }

      return(droplevels(query_output_df))
    }
  }

