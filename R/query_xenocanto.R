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

    # Check internet connection using httr and error handling
    response <- try(httr::GET("www.xeno-canto.org"), silent = TRUE)
    if (inherits(response, "try-error") ||
        httr::http_error(response)) {
      .stop("No connection to xeno-canto.org (check your internet connection!)")
    }

    content <- httr::content(response, as = "text")
    if (grepl("Could not connect to the database", content)) {
      .stop("xeno-canto.org website is apparently down")
    }

    # search recs in xeno-canto (results are returned in pages with 500 recordings each)
    if (pb & verbose) {
      cat(paste(
        .color_text(
          paste0("Obtaining metadata (matching observation(s) found)"),
          "success"
        ),
        .add_emoji("happy"),
        ":\n"
      ))
    }

    # save original term for query metadata
    org_term <- term

    # format query term
    if (grepl("\\:", term)) {
      # if using advanced searc
      # replace first space with %20 when using full species name
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

      # replace remaining spaces with "&"
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
    } else {
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
          X = seq_len(query$numPages),
          cl = cl,
          FUN = function(y) {
            # search for each page
            query_output <-
              jsonlite::fromJSON(
                txt = paste0(
                  "https://www.xeno-canto.org/api/2/recordings?query=",
                  term,
                  "&page=",
                  y
                )
              )

            query_output$recordings$also <- sapply(query_output$recordings$also, paste, collapse = "-")


            # split sonogram in 3 columns
            sono_df <- as.data.frame(query_output$recordings$sono)

            names(sono_df) <- paste("sonogram", names(sono_df), sep = "_")

            # split oscillograms in 3 columns
            osci_df <- as.data.frame(query_output$recordings$osci)

            names(osci_df) <- paste("oscillogram", names(osci_df), sep = "_")

            # remove raw columns
            query_output$recordings$sono <- query_output$recordings$osci <- NULL

            query_output <- cbind(query_output$recordings, sono_df, osci_df)

            return(query_output)
          }
        )

      # determine all column names in all pages
      pooled_column_names <- unique(unlist(lapply(records_list, names)))

      # add columns that are missing to each data set
      records_list2 <- lapply(records_list, function(X) {
        nms <- names(X)
        if (length(nms) != length(pooled_column_names)) {
          for (i in pooled_column_names) {
            X <-
              data.frame(X,
                         NA,
                         stringsAsFactors = FALSE,
                         check.names = FALSE)
            names(X)[ncol(X)] <- i
          }
        }
        return(X)
      })

      # save results in a single data frame
      query_output_df <- do.call(rbind, records_list2)

      if (as.numeric(query$numRecordings) > 0) {
        # convert factors to characters
        indx <- sapply(query_output_df, is.factor)
        query_output_df[indx] <- lapply(query_output_df[indx], as.character)

        # create species column and other missing columns
        query_output_df$species <- paste(query_output_df$gen, query_output_df$sp, sep = " ")

        query_output_df$file_extension <- sub(".*\\.", "", query_output_df$`file-name`)

        # fix formatting
        query_output_df$file_extension <- .fix_extension(query_output_df$file_extension)

        # Add repository ID
        query_output_df$repository <- "Xeno-Canto"

        # format output data frame column names
        query_output_df <- .format_query_output(
          X = query_output_df,
          call = base::match.call(),
          colm_names = c(
            "id" = "key",
            "gen" = "genus",
            "sp" = "specific_epithet",
            "ssp" = "subspecies",
            "en" = "english_name",
            "rec" = "recordist",
            "cnt" = "country",
            "loc" = "locality",
            "lat" = "latitude",
            "lon" = "longitude",
            "alt" = "altitude",
            "type" = "vocalization_type",
            "file" = "file_url",
            "lic" = "license",
            "url" = "url",
            "q" = "quality",
            "grp" = "taxonomic_group",
            "file-name" = "uploaded_file",
            "sono" = "sonogram",
            "also" = "other_species",
            "smp" = "sampling_rate",
            "dvc" = "recorder",
            "mic" = "microphone",
            "uploaded" = "upload_date",
            "rmk" = "comments",
            "animal.seen" = "animal_seen",
            "playback.used" = "playback_used"
          ), all_data = all_data,
          format = "sound"
        )

        # remove duplicates
        query_output_df <- query_output_df[!duplicated(query_output_df$key), ]


        if (pb & verbose) {
          cat(.color_text(paste(
            nrow(query_output_df), "audio(s) found"
          ), "success"),
          .add_emoji("happy"),
          "\n")
        }
      }

      # Generate a file path by combining tempdir() with a file name
      # file_path <- file.path(tempdir(), paste0(term, ".rds"))

      # Save the object to the file
      # MARCELO: I added a try function to avoid errors when saving the file, double check (why do we need to save it and how people can find the RDS)
      # try(saveRDS(droplevels(query_output_df), file = file_path), silent = TRUE)
      return(droplevels(query_output_df))
    }
  }
