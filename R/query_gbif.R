#' Access 'gbif' recordings and metadata
#'
#' \code{query_gbif} searches for metadata from \href{https://www.gbif.org/}{gbif}.
#' @usage query_gbif(term, type, cores = 1, pb = TRUE)
#' @param term Character vector of length one indicating the genus, or genus and
#'  species, to query 'wikiaves' database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}.
#'  @param type Character vector with media type to query for. Options are 'photo' or 'audio'. Required.
#' @param cores Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param dataset see https://www.gbif.org/dataset/search?q=
#' @return If X is not provided the function returns a data frame with the following recording information: recording ID, media type, user ID, species ID, scientific name, common name, repository ID, author, user name, date, verified condition, location, location ID, comments, likes, visualizations, url, duration and repository
#' @export
#' @name query_gbif
#' @details This function queries for avian vocalization recordings in the open-access
#' online repository \href{https://www.gbif.org/}{gbif}. It can return recordings metadata.
#' @examples
#' \dontrun{
#' # search without downloading
#' df1 <- query_gbif(term = 'Phaethornis anthophilus', download = FALSE)
#' View(df1)
#'
#' }
#'
#' @references {
#' Schubert, Stephanie Caroline, Lilian Tonelli Manica, and André De Camargo Guaraldo. 2019. Revealing the potential of a huge citizen-science platform to study bird migration. Emu-Austral Ornithology 119.4: 364-373.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
query_gbif <-
    function(term = NULL,
             type = NULL,
             cores = 1,
             pb = TRUE,
             verbose = TRUE) {

      # type must be supplied
      if (is.null(type))
        stop("'type' must be supplied")

      # type must be supplied
      if (is.null(term))
        stop("'term' must be supplied")

      #check internet connection
      a <- try(RCurl::getURL("https://api.gbif.org/v1/occurrence/search?"), silent = TRUE)
      if (is(a, "try-error"))
        stop("No connection to GBIF API (check your internet connection!)")

      if (a == "Could not connect to the database")
        stop("GBIF website is apparently down")

      # If cores is not numeric
      if (!is.numeric(cores))
        stop("'cores' must be a numeric vector of length 1")
      if (any(!(cores %% 1 == 0), cores < 1))
        stop("'cores' should be a positive integer")

      # fix term for html
      term <- gsub(" ", "%20", term)

      base.srch.pth <- rjson::fromJSON(file = paste0("https://api.gbif.org/v1/occurrence/search?media_type=Sound&limit=300&species=", term))

      # get total number of pages
      num_pages <- ceiling(base.srch.pth$count / base.srch.pth$limit)

  query_output_list <- pblapply_sw_int(seq_len(num_pages), cl = cl, pbar = pb, function(i)
  {
      query_output <-
      jsonlite::fromJSON(
        paste0(
          "https://api.gbif.org/v1/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165&media_type=Sound&limit=300",
          if (type == "photo")
            "f" else
              "s",
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

  paste0(base.srch.pth, "&offset=", i)

  #initialize search
  query_output <- as.array(rjson::fromJSON(file = paste0(base.srch.pth)))

  # format as list of data frame
  query_output$results <- lapply(query_output$results , function(x) {

    media_list <- lapply(lapply(x$media, function(y) data.frame(t(unlist(y)))), function(w) data.frame(t(unlist(w))))

    media_df <- do.call(rbind, media_list)

    ## fitler by media type
    # media_df <- media_df[grep()]


    # fix identifier column name
    names(media_df)[names(media_df) == "identifier"] <- "URL"
    # remove lists
    x <- x[!sapply(x, is.list)]

    # make it data frame
    X_df <- data.frame(t(unlist(x)))

    # add media details
    X_df <- cbind(X_df, media_df)


    return(X_df)
  })

  # get common names to all data frames in X
  cnms <- unique(unlist(lapply(query_output$results, names)))

  # add missing columns to all data frames in X
  query_output$results <- lapply(query_output$results, function(e){

    nms <- names(e)
    if (length(nms) != length(cnms))
      for (i in cnms[!cnms %in% nms]) {
        e <-
          data.frame(e,
                     NA,
                     stringsAsFactors = FALSE,
                     check.names = FALSE)
        names(e)[ncol(e)] <- i
      }
    return(e)
  })

  # allr results in a single data frame
  results <- do.call(rbind, query_output$results)

}
  #Specific search for species




sapply(query_output$results[[1]], length)

sapply(query_output$results, length)

unlist(query_output$results[[1]])


    #format JSON
    term <- gsub(" ", "%20", term)

    #initialize search
    get_ids <-
      rjson::fromJSON(file = paste0("https://api.gbif.org/v1/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165&media_type=Sound&limit=300&species=", term)
      )

    if (length(get_ids$count) == 0){
      if (verbose)
        cat(paste(colortext("Search term not found", "failure"), add_emoji("sad")))
    } else {

      #################################################################################
      # make it a data frame
      get_ids <- as.data.frame(t(sapply(get_ids, unlist)))

      get_ids$total_registers <-sapply(1:nrow(get_ids), function(u)
        as.numeric(jsonlite::fromJSON(
          paste0(
            "https://www.wikiaves.com.br/getRegistrosJSON.php?tm=",
            if (type == "photo")
              "f" else
                "s",
            "&t=s&s=",
            get_ids$id[u],
            "&o=mp&p=1"
          )
        )$registros$total))

      if (sum(get_ids$total_registers) == 0 & verbose)
        cat(paste(colortext(paste0("No ", type, "s were found"), "failure"), add_emoji("sad"))) else {

          # get number of pages (20 is the default number of registers per page)
          get_ids$pages <- ceiling(get_ids$total_registers / 20)


          # remove those rows with no pages (only needed when many species are returned)
          get_ids <- get_ids[get_ids$pages > 0, ]


          id_by_page_list <- lapply(1:nrow(get_ids), function(x){

            X <- get_ids[x, ]
            out_df <- data.frame(id = X$id, page = 1:X$pages)

          })

          id_by_page_df <- do.call(rbind, id_by_page_list)

          #search recs in wikiaves (results are returned in pages with 500 recordings each)
          if (pb & verbose)
            cat(paste(colortext(paste0("Obtaining metadata (", sum(get_ids$total_registers), " ", type, "(s) found)"), "success"), add_emoji("happy"), ":\n"))

          # set clusters for windows OS
          if (Sys.info()[1] == "Windows" & cores > 1)
            cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores)) else cl <- cores

          # loop over pages
          query_output_list <- pblapply_sw_int(1:nrow(id_by_page_df), cl = cl, pbar = pb, function(i)
          {
            query_output <-
              jsonlite::fromJSON(
                paste0(
                  "https://www.wikiaves.com.br/getRegistrosJSON.php?tm=",
                  if (type == "photo")
                    "f" else
                      "s",
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
          if (type == "audio")
            query_output_df$link <-
            gsub("\\.jpg$", ".mp3",  query_output_df$link)

          # add repository
          query_output_df$repository <- "wikiaves"

          # remove weird columns
          query_output_df$por <- query_output_df$grande <- query_output_df$enviado <- NULL

          # flip verified
          query_output_df$is_questionada <- !as.logical(query_output_df$is_questionada)

          #fix media type
          query_output_df$tipo <- type

          # rename output columns
          names_df <- data.frame(old = c("id", "tipo", "id_usuario", "sp.id", "sp.nome", "sp.nvt", "sp.idwiki", "autor", "perfil", "data", "is_questionada", "local", "idMunicipio", "coms", "likes", "vis", "link", "dura", "repository"), new = c("record.id", "media.type", "user.id", "sp.id", "scientific.name", "common.name", "repository.id", "author", "user.name", "date", "verified", "location", "location.id", "comments", "likes", "visualizations", "url", "duration", "repository"))

          for(i in 1:nrow(names_df))
            names(query_output_df)[names(query_output_df) == names_df$old[i]] <- names_df$new[i]

          return(query_output_df)
        }
    }
  }
