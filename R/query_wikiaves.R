#' Access 'wikiaves' recordings and metadata
#'
#' \code{query_wikiaves} searches for metadata from \href{https://www.wikiaves.com/}{wikiaves}.
#' @inheritParams template_params
#' @param type Character vector with media type to query for. Options are 'still image' or 'sound'. Required.
#' @return If all_data is not provided the function returns a data frame with the following
#' recording information: recording ID, media type, user ID, species ID, scientific name, common
#' name, repository ID, author, user name, date, verified condition, location, location ID,
#' comments, likes, visualizations, url, duration and repository
#' @export
#' @name query_wikiaves
#' @details This function queries for avian vocalization recordings in the open-access
#' online repository \href{https://www.wikiaves.com/}{wikiaves}. It can return recordings metadata.
#' @examples
#' \dontrun{
#' # search
#' df1 <- query_wikiaves(term = "Phaethornis nattereri", type = "still image")
#' View(df1)
#' }
#'
#' @references {
#' Schubert, Stephanie Caroline, Lilian Tonelli Manica, and André De Camargo Guaraldo. 2019. Revealing the potential of a huge citizen-science platform to study bird migration. Emu-Austral Ornithology 119.4: 364-373.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
query_wikiaves <-
  function(term,
           type = c("sound", "still image"),
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


    # Check internet connection using httr and error handling
    response <- try(httr::GET("https://www.wikiaves.com"), silent = TRUE)
    if (inherits(response, "try-error") ||
        httr::http_error(response)) {
      .stop("No connection to wikiaves (check your internet connection!)")
    }

    content <- httr::content(response, as = "text")
    if (grepl("Could not connect to the database", content)) {
      .stop("wikiaves.com website is apparently down")
    }

    # assign a value to type
    org_type <- type <- rlang::arg_match(type)

    type <- switch(type, sound = "Sound", `still image` = "photo")

    # format JSON
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
            if (type == "photo") {
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
          paste0("No ", type, "s were found"), "failure"
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
                type,
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
        query_output_list <- pblapply_sw_int(1:nrow(id_by_page_df), cl = cl, pbar = pb, function(i) {
          query_output <-
            jsonlite::fromJSON(
              paste0(
                "https://www.wikiaves.com.br/getRegistrosJSON.php?tm=",
                if (type == "photo") {
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
            )

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
        rownames(query_output_df) <- 1:nrow(query_output_df)

        # change jpg to mp3 in links
        if (type == "Sound") {
          query_output_df$link <-
            gsub(".jpg$", ".mp3", query_output_df$link)
        }

        # add repository
        query_output_df$repository <- "wikiaves"

        # remove weird columns
        query_output_df$por <- query_output_df$grande <- query_output_df$enviado <- NULL

        # flip verified
        query_output_df$is_questionada <- !as.logical(query_output_df$is_questionada)

        # fix media type
        query_output_df$tipo <- type

        # rename output columns
        names_df <- data.frame(
          old = c(
            "id",
            "tipo",
            "id_usuario",
            "sp.id",
            "sp.nome",
            "sp.nvt",
            "sp.idwiki",
            "autor",
            "perfil",
            "data",
            "is_questionada",
            "local",
            "idMunicipio",
            "coms",
            "likes",
            "vis",
            "link",
            "dura",
            "repository"
          ),
          new = c(
            "key",
            "media.type",
            "user.id",
            "sp.id",
            "species",
            "common.name",
            "repository.id",
            "author",
            "user.name",
            "date",
            "verified",
            "location",
            "location.id",
            "comments",
            "likes",
            "visualizations",
            "file_url",
            "duration",
            "repository"
          )
        )

        for (i in 1:nrow(names_df)) {
          names(query_output_df)[names(query_output_df) == names_df$old[i]] <- names_df$new[i]
        }

        if (!all_data) {
          query_output_df$country <- "Brazil"
          names(query_output_df)[names(query_output_df) == "scientific.name"] <- "species"
          names(query_output_df)[names(query_output_df) == "record.id"] <- "key"
          query_output_df$latitude <- NA
          query_output_df$longitude <- NA
          query_output_df <- query_output_df[, c(
            "key",
            "species",
            "date",
            "country",
            "location",
            "latitude",
            "longitude",
            "file_url",
            "repository"
          )]
        }

        # Add a timestamp, term and type attribute
        search_time <- Sys.time()
        attr(query_output_df, "search_time") <- search_time
        attr(query_output_df, "query_term") <- term
        attr(query_output_df, "query_type") <- org_type
        attr(query_output_df, "query_all_data") <- all_data

        # Generate a file path by combining tempdir() with a file name
        file_path <- file.path(tempdir(), paste0(term, ".rds"))

        # Save the object to the file
        saveRDS(query_output_df, file = file_path)
        return(query_output_df)
      }
    }
  }
