#' Access 'Wikiaves' recordings and metadata
#'
#' \code{query_wikiaves} searches for metadata from
#' \href{https://www.wikiaves.com.br/}{wikiaves}.
#' @inheritParams template_params
#' @param format Character vector with the media format to query for.
#' Options are 'sound' or 'image'. Required.
#' @export
#' @name query_wikiaves
#' @return The function returns a data frame with the metadata of the media
#' files matching the search criteria. If \code{all_data = TRUE}, all metadata
#' fields (columns) are returned. If \code{raw_data = TRUE}, the raw data as
#' obtained from the repository is returned (without any formatting).
#' @details This function queries for avian digital media in the open-access
#' online repository \href{https://www.wikiaves.com.br/}{wikiaves} and returns
#' its metadata. WikiAves is a Brazilian online platform and citizen science
#' project that serves as the largest community for birdwatchers in Brazil.
#' It functions as a collaborative, interactive encyclopedia of Brazilian
#' birds, where users contribute georeferenced photographs and sound
#' recordings, which are then used to build a vast database for research
#' and conservation.
#' @examples
#' if (interactive()){
#' # search
#' p_nattereri <- query_wikiaves(species = "Phaethornis nattereri",
#'     format = "image")
#' }
#'
#' @references {
#' Schubert, Stephanie Caroline, Lilian Tonelli Manica, and André De Camargo
#' Guaraldo. 2019. Revealing the potential of a huge citizen-science platform
#' to study bird migration. Emu-Austral Ornithology 119.4: 364-373.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

query_wikiaves <-
  function(species = getOption("species"),
           format = c("sound", "image"),
           cores = getOption("mc.cores", 1),
           pb = getOption("pb", TRUE),
           verbose = getOption("verbose", TRUE),
           all_data = getOption("all_data", FALSE),
           raw_data = getOption("raw_data", FALSE)) {
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

    # Use the unified connection checker
    if (!.checkconnection(verb = verbose, service = "wikiaves")) {
      return(invisible(NULL))
    }

    # assign a value to format
    format <- rlang::arg_match(format)

    wiki_format <- switch(format, sound = "s", image = "f")

    # initialize search with user agent
    response <- httr::GET(
      url = paste0(
        "https://www.wikiaves.com.br/getTaxonsJSON.php?term=",
        gsub(" ", "%20", species)
      ),
      httr::user_agent("suwo (https://github.com/maRce10/suwo)")
    )

    # check if request succeeded
     if (httr::http_status(response)$category != "Success") {
      if (verbose) {
        .message(
          text = paste0(
            "Wikiaves query request failed: ",
            httr::http_status(response)$message
          ),
          as = "failure"
        )
      }
      return(invisible(NULL))
     }

    get_ids <- httr::content(response, as = "parsed", type = "application/json")


    if (length(get_ids) == 0) {
      if (verbose) {
        .message("Search species not found", as = "failure")
      }
      return(invisible(NULL))
    }

    # make it a data frame
    get_ids <- as.data.frame(t(vapply(get_ids, unlist, character(8))))

    get_ids$total_registers <- vapply(seq_len(nrow(get_ids)), function(u) {
      response <- httr::GET(
        url = paste0(
          "https://www.wikiaves.com.br/getRegistrosJSON.php?tm=",
          wiki_format,
          "&t=s&s=",
          get_ids$id[u],
          "&o=mp&p=1"
        ),
        httr::user_agent("suwo (https://github.com/maRce10/suwo)")
      )

      # check if request succeeded
      if (httr::http_status(response)$category != "Success") {
        if (verbose) {
          .message(
            text = paste0(
              "Wikiaves query request failed: ",
              httr::http_status(response)$message
            ),
            as = "failure"
          )
        }
        return(invisible(NULL))
      }


      as.numeric(httr::content(response, as = "parsed")$registros$total)
    }, numeric(1))

    if (sum(get_ids$total_registers) == 0) {
      if (verbose) {
        .message(text = "No matching records found", as = "failure")
      }
      return(invisible(NULL))
    }

    # get number of pages (20 is the default number of registers per page)
    get_ids$pages <- ceiling(get_ids$total_registers / 20)

    # remove those rows with no pages
    # (only needed when many species are returned)
    get_ids <- get_ids[get_ids$pages > 0, ]

    id_by_page_list <- lapply(seq_len(nrow(get_ids)), function(x) {
      X <- get_ids[x, ]
      data.frame(id = X$id, page = 1:X$pages)
    })

    id_by_page_df <- do.call(rbind, id_by_page_list)

    # search recs in wikiaves (results are returned in pages with 500
    # recordings each)
    if (verbose) {
      .message(n = get_ids$total_registers, as = "success")
    }

    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" && cores > 1) {
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores))
    } else {
      cl <- cores
    }

    # loop over pages
    query_output_list <- pblapply_sw_int(seq_len(nrow(id_by_page_df)), cl = cl,
                                         pbar = pb, function(i) {
      # print(i)
      # Sys.sleep(1)

      query_output <-
        try(jsonlite::fromJSON(
          paste0(
            "https://www.wikiaves.com.br/getRegistrosJSON.php?tm=",
            wiki_format,
            "&t=",
            "s",
            "&s=",
            id_by_page_df$id[i],
            "&o=mp&p=",
            id_by_page_df$page[i]
          )
        ), silent = TRUE)

      # retry if an error occurs waiting 1 s
      if (is(query_output, "try-error")) {
        Sys.sleep(1)

        query_output <- try(jsonlite::fromJSON(jsonlite::fromJSON(
          paste0(
            "https://www.wikiaves.com.br/getRegistrosJSON.php?tm=",
            wiki_format,
            "&t=",
            "s",
            "&s=",
            id_by_page_df$id[i],
            "&o=mp&p=",
            id_by_page_df$page[i]
          )
        ), silent = TRUE)
      )
      }

      # if error then just return the error
      if (is(query_output, "try-error")){
        return(query_output)
      }

      # make it a data frame
      output_df <-
        as.data.frame(do.call(rbind, lapply(
          query_output$registros$itens, unlist
        )))

      # fix link
      output_df$link <- gsub("#", "", as.character(output_df$link))

      return(output_df)
    })

    # let user know error when downloading metadata
    if (any(vapply(query_output_list, .is_error, FUN.VALUE = logical(1)))) {
      if (verbose) {
        .message(text = "Metadata could not be dowloaded", as = "failure")
      }
      return(invisible(NULL))
    }

    # combine into a single data frame
    query_output_df <- .merge_data_frames(query_output_list)

    # rename rows
    rownames(query_output_df) <- seq_len(nrow(query_output_df))

    # change jpg to mp3 in links
    if (format == "sound") {
      query_output_df$link <-
        gsub(".jpg$", ".mp3", query_output_df$link)
    }

    # remove weird columns
    query_output_df$por <- query_output_df$grande <-
      query_output_df$enviado <- NULL

    # flip verified
    query_output_df$is_questionada <-
      !as.logical(query_output_df$is_questionada)

    # add file format
    query_output_df$file_extension <- sub(".*\\.", "", query_output_df$link)

    # add missing basic columns
    query_output_df$format <- format
    query_output_df$country <- "Brazil"

    # rename output columns
    query_output_df <- .format_query_output(
      X = query_output_df,
      call = base::match.call(),
      column_names = c(
        "id" = "key",
        "tipo" = "format",
        "id_usuario" = "user.id",
        "sp.id" = "species_id",
        "sp.nome" = "species",
        "sp.nvt" = "common.name",
        "sp.idwiki" = "repository.id",
        "autor" = "author",
        "perfil" = "user_name",
        "data" = "date",
        "is_questionada" = "verified",
        "local" = "locality",
        "idMunicipio" = "locality.id",
        "coms" = "number_of_comments",
        "likes" = "likes",
        "vis" = "visualizations",
        "link" = "file_url",
        "dura" = "duration",
        "scientific.name" = "species",
        "record.id" = "key",
        "species_id" = "species_code",
        "author" = "user_name"
      ),
      all_data = all_data,
      format = format,
      raw_data = raw_data
    )
    # Generate a file path by combining tempdir() with a file name
    # file_path <- file.path(tempdir(), paste0(species, ".rds"))
    #
    # # Save the object to the file
    # saveRDS(query_output_df, file = file_path)
    return(query_output_df)

  }
