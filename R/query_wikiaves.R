#' Access 'Wikiaves' recordings and metadata
#'
#' \code{query_wikiaves} searches for metadata from \href{https://www.wikiaves.com/}{wikiaves}.
#' @inheritParams template_params
#' @param format Character vector with the media format to query for. Options are 'sound' or 'image'. Required.
#' @return A data frame with the metadata of the observations matching the query.  If all_data is \code{FALSE} (default) the data frame contains the following columns: key, species, date, country, locality, latitude, longitude, file_url, repository, format. If all_data is \code{TRUE} the data frame contains the following information: recording ID, media type, user ID, species ID, scientific name, common name, repository ID, author, user name, date, verified condition, locality, locality ID, comments, likes, visualizations, url, duration and repository.
#' @export
#' @name query_wikiaves
#' @details This function queries for avian vocalization recordings in the open-access
#' online repository \href{https://www.wikiaves.com/}{wikiaves}. It can return recordings metadata.
#' @examples
#' \dontrun{
#' # search
#' df1 <- query_wikiaves(term = "Phaethornis nattereri", format = "image")
#' View(df1)
#' }
#'
#' @references {
#' Schubert, Stephanie Caroline, Lilian Tonelli Manica, and André De Camargo Guaraldo. 2019. Revealing the potential of a huge citizen-science platform to study bird migration. Emu-Austral Ornithology 119.4: 364-373.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

query_wikiaves <-
  function(term,
           format = c("sound", "image"),
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

    url_check <- .check_internet_resource(url = "https://www.wikiaves.com.br", skip.error = TRUE)

    # Check internet connection using httr and error handling
    # response <- try(httr::GET("https://www.wikiaves.com.br/wiki/aves"), silent = TRUE)
    # if (inherits(response, "try-error") ||
    #     httr::http_error(response)) {
    #   .stop("No connection to wikiaves (check your internet connection!)")
    # }
    #
    # content <- httr::content(response, as = "text")
    # if (grepl("Could not connect to the database", content)) {
    #   .stop("wikiaves.com website is apparently down")
    # }

    if (url_check == "OK") {
    # assign a value to format
    org_format <- format <- rlang::arg_match(format)

    format <- switch(format, sound = "Sound", image = "photo")

    # format JSON
    org_term <- term
    term <- gsub(" ", "%20", term)

    # initialize search with user agent
    response <- httr::GET(
      url = paste0(
        "https://www.wikiaves.com.br/getTaxonsJSON.php?term=",
        term
      ),
      httr::user_agent("suwo (https://github.com/maRce10/suwo)")
    )

    # check if request succeeded
    httr::stop_for_status(response)

    get_ids <- httr::content(response, as = "parsed", type = "application/json")


    if (length(get_ids) == 0) {
      if (verbose) {
        cat(paste(
          .color_text("Search term not found", "failure"),
          .add_emoji("sad")
        ))
      }
    } else {
      # make it a data frame
      get_ids <- as.data.frame(t(sapply(get_ids, unlist)))

      get_ids$total_registers <- sapply(seq_len(nrow(get_ids)), function(u) {
        response <- httr::GET(
          url = paste0(
            "https://www.wikiaves.com.br/getRegistrosJSON.php?tm=",
            if (format == "photo") {
              "f"
            } else {
              "s"
            },
            "&t=s&s=",
            get_ids$id[u],
            "&o=mp&p=1"
          ),
          httr::user_agent("suwo (https://github.com/maRce10/suwo)")
        )
        httr::stop_for_status(response)
        as.numeric(httr::content(response, as = "parsed")$registros$total)
      })

      if (sum(get_ids$total_registers) == 0) {
        cat(paste(.color_text(
          paste0("No ", format, "s were found"), "failure"
        ), .add_emoji("sad")))
      } else {
        # get number of pages (20 is the default number of registers per page)
        get_ids$pages <- ceiling(get_ids$total_registers / 20)

        # remove those rows with no pages (only needed when many species are returned)
        get_ids <- get_ids[get_ids$pages > 0, ]

        id_by_page_list <- lapply(seq_len(nrow(get_ids)), function(x) {
          X <- get_ids[x, ]
          out_df <- data.frame(id = X$id, page = 1:X$pages)
        })

        id_by_page_df <- do.call(rbind, id_by_page_list)

        # search recs in wikiaves (results are returned in pages with 500 recordings each)
        if (pb & verbose) {
          cat(paste(
            .color_text(
              paste0(
                "Obtaining metadata (",
                sum(get_ids$total_registers),
                " ",
                format,
                "(s) found)"
              ),
              "success"
            ),
            .add_emoji("happy"),
            ":\n"
          ))
        }

        # set clusters for windows OS
        if (Sys.info()[1] == "Windows" & cores > 1) {
          cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores))
        } else {
          cl <- cores
        }

        # loop over pages
        query_output_list <- pblapply_sw_int(seq_len(nrow(id_by_page_df)), cl = cl, pbar = pb, function(i) {
          # print(i)
          Sys.sleep(1)

          query_output <-
            try(jsonlite::fromJSON(
              paste0(
                "https://www.wikiaves.com.br/getRegistrosJSON.php?tm=",
                if (format == "photo") {
                  "f"
                } else {
                  "s"
                },
                "&t=",
                "s",
                "&s=",
                id_by_page_df$id[i],
                "&o=mp&p=",
                id_by_page_df$page[i]
              )
            ),
            silent = TRUE)

          # retry if an error occurs waiting 1 s
          if (is(query_output, "try-error")) {
            Sys.sleep(1)

            query_output <-jsonlite::fromJSON(
              paste0(
                "https://www.wikiaves.com.br/getRegistrosJSON.php?tm=",
                if (format == "photo") {
                  "f"
                } else {
                  "s"
                },
                "&t=",
                "s",
                "&s=",
                id_by_page_df$id[i],
                "&o=mp&p=",
                id_by_page_df$page[i]
              ))
            }


          # make it a data frame
          output_df <-
            as.data.frame(t(sapply(
              query_output$registros$itens, unlist
            )))

          # fix link
          output_df$link <- gsub("#", "", as.character(output_df$link))

          return(output_df)
        })

        # put in a data frame
        query_output_df <- do.call(rbind, query_output_list)

        # rename rows
        rownames(query_output_df) <- seq_len(nrow(query_output_df))

        # change jpg to mp3 in links
        if (format == "Sound") {
          query_output_df$link <-
            gsub(".jpg$", ".mp3", query_output_df$link)
        }

        # remove weird columns
        query_output_df$por <- query_output_df$grande <- query_output_df$enviado <- NULL

        # flip verified
        query_output_df$is_questionada <- !as.logical(query_output_df$is_questionada)

        # add file format
        query_output_df$file_extension <- sub(".*\\.", "", query_output_df$link)

        # fix formatting
        query_output_df$file_extension <- .fix_extension(query_output_df$file_extension)

        # add missing basic columns
        query_output_df$format <- org_format
        query_output_df$country <- "Brazil"

        # rename output columns
        query_output_df <- .format_query_output(
          X = query_output_df,
          call = base::match.call(),
          colm_names = c(
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
            "species_id" = "species_code"
          ),
          all_data = all_data,
          format = org_format
        )

        # Generate a file path by combining tempdir() with a file name
        # file_path <- file.path(tempdir(), paste0(term, ".rds"))
        #
        # # Save the object to the file
        # saveRDS(query_output_df, file = file_path)
        return(query_output_df)
      }
    }
  }
}
