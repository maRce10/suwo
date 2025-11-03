#' Access 'Xeno-Canto' recordings and metadata
#'
#' \code{query_xenocanto} searches for metadata from
#' \href{https://www.xeno-canto.org/}{Xeno-Canto}.
#' @inheritParams template_params
#' @param species Character string with the scientific name of a species in
#' the format: "Genus epithet". Required. Can be set globally for the current
#' R session via the "term" option (e.g.
#' \code{options(term = "Hypsiboas rufitelus")}). Alternatively, a character
#' string containing additional tags that follows the Xeno-Canto advanced query
#' syntax can be provided. Tags are of the form 'tag:searchterm'. For
#' instance, \code{'type:"song"'} will search for recordings where the sound
#' type contains 'song'. Multiple tags can be provided
#' (e.g., \code{'"cnt:"belize" type:"song"'}).
#'  See examples down below and check
#'  \href{https://www.xeno-canto.org/help/search}{Xeno-Canto's search help}
#'  for a full description.
#' @param api_key Character string refering to the key assigned by Xeno-Canto
#' as authorization for searches. Get yours at
#' \href{https://xeno-canto.org/account}{https://xeno-canto.org/account}.
#' Required.
#' @export
#' @name query_xenocanto
#' @return The function returns a data frame with the metadata of the media
#' files matching the search criteria. If \code{all_data = TRUE}, all metadata
#' fields (columns) are returned. If \code{raw_data = TRUE}, the raw data as
#' obtained from the repository is returned (without any formatting).
#' @details This function queries metadata for animal sound recordings in
#' the open-access
#'  online repository \href{https://www.xeno-canto.org/}{Xeno-Canto}. Complex
#'  queries can be constructed usin the Xeno-Canto advanced query syntax.
#' @seealso \code{\link{query_gbif}}, \code{\link{query_wikiaves}},
#' \code{\link{query_inaturalist}}, \code{\link{query_observation}}
#' @examples
#' \dontrun{
#' # An API key is required. Get yours at https://xeno-canto.org/account.
#' XC_API_KEY <- "YOUR_API_KEY_HERE"
#'
#' # Simple search for a species (will be converted to sp:"Phaethornis anthophilus")
#' p_anth <- query_xenocanto(species = "Phaethornis anthophilus", api_key = XC_API_KEY)
#'
#' # Search for same species and add specify country
#' p_anth_cr <- query_xenocanto(species = 'sp:"Phaethornis anthophilus" cnt:"Panama"',
#' raw_data = TRUE, api_key = XC_API_KEY)
#'
#' # Search for female songs of a species
#' femsong <-  query_xenocanto(
#' species = 'sp:"Thryothorus ludovicianus" type:"song" type:"female"',
#' api_key = XC_API_KEY)
#' }
#'
#' @references {
#' Planqué, Bob, & Willem-Pier Vellinga. 2008. Xeno-canto: a 21st-century way
#' to appreciate Neotropical bird song. Neotrop. Birding 3: 17-23.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

query_xenocanto <-
  function(species = getOption("species"),
           api_key = getOption("xc_api_key"),
           cores = getOption("mc.cores", 1),
           pb = getOption("pb", TRUE),
           verbose = getOption("verbose", TRUE),
           all_data = getOption("all_data", FALSE),
           raw_data = getOption("raw_data", FALSE)) {
    # Check for API key
    if (is.null(api_key) || !nzchar(api_key)) {
      .stop(
        "An API key is required for Xeno-Canto API v3. Get yours at https://xeno-canto.org/account."
      )
    }

    # --- build query from tags ---
    # Handle species names with spaces by wrapping them in quotes for the query
    if (!grepl(":", species)){
    species_name <- ifelse(grepl("\\s", species), paste0('"', species, '"'), species)

    # Prepend the required 'sp:' tag to the species name
    query_str <- paste0("sp:", species_name)
    } else {
      # Collapse into a single query string
      query_str <- paste(species, collapse = " ")
    }

    # URL encode (spaces -> %20, quotes -> %22, etc.)
    query_str <- utils::URLencode(query_str, reserved = TRUE)

    if (verbose) {
      .message("Obtaining metadata:\n", as = "success")
    }

    # --- API request ---
    query <- jsonlite::fromJSON(paste0(
      "https://www.xeno-canto.org/api/3/recordings?query=",
      query_str,
      "&key=",
      api_key
    ))

    if (as.numeric(query$numRecordings) == 0) {
      if (verbose)
        .message(text = "No matching records found", as = "failure")
      return(invisible(NULL))
    }

    if (Sys.info()[1] == "Windows" & cores > 1) {
      cl <-
        parallel::makePSOCKcluster(getOption("cl.cores", cores))
    } else {
      cl <- cores
    }

    records_list <- pblapply_sw_int(
      pbar = pb,
      X = seq_len(query$numPages),
      cl = cl,
      FUN = function(y) {
        query_output <- jsonlite::fromJSON(paste0(
          "https://www.xeno-canto.org/api/3/recordings?query=",
          query_str,
          "&page=",
          y,
          "&key=",
          api_key
        ))

        query_output$recordings$also <-
          vapply(query_output$recordings$also,
                 paste,
                 collapse = "-",
                 FUN.VALUE = character(1))


        sono_df <- as.data.frame(query_output$recordings$sono)
        names(sono_df) <-
          paste("sonogram", names(sono_df), sep = "_")

        osci_df <- as.data.frame(query_output$recordings$osci)
        names(osci_df) <-
          paste("oscillogram", names(osci_df), sep = "_")

        query_output$recordings$sono <-
          query_output$recordings$osci <- NULL
        query_output <-
          cbind(query_output$recordings, sono_df, osci_df)
        return(query_output)
      }
    )

    pooled_column_names <-
      unique(unlist(lapply(records_list, names)))
    records_list2 <- lapply(records_list, function(X) {
      nms <- names(X)
      if (length(nms) != length(pooled_column_names)) {
        for (i in pooled_column_names) {
          X <- data.frame(
            X,
            NA,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          names(X)[ncol(X)] <- i
        }
      }
      return(X)
    })
    query_output_df <- do.call(rbind, records_list2)

    if (as.numeric(query$numRecordings) > 0) {
      indx <- vapply(query_output_df, is.factor, logical(1))

      query_output_df[indx] <-
        lapply(query_output_df[indx], as.character)

      query_output_df$species <-
        paste(query_output_df$gen, query_output_df$sp, sep = " ")
      query_output_df$file_extension <-
        sub(".*\\.", "", query_output_df$`file-name`)
      query_output_df$repository <- "Xeno-Canto"
      query_output_df$file <-
        paste0("https://xeno-canto.org/", query_output_df$id, "/download")
      query_output_df$date <-
        gsub("-", "/", query_output_df$date)

      query_output_df <- .format_query_output(
        X = query_output_df,
        call = base::match.call(),
        column_names = c(
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
          "playback.used" = "playback_used",
          "recordist" = "user_name"
        ),
        all_data = all_data,
        format = "sound",
        raw_data = raw_data
      )

      if (verbose) {
        .message(
          paste0("{n} matching sound file{?s} found"),
          as = "success",
          n = nrow(query_output_df)
        )
      }
      return(droplevels(query_output_df))
    }
  }
