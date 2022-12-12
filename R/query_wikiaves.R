#' Access 'Xeno-Canto' recordings and metadata
#'
#' \code{query_wikiaves} searches for metadata from \href{https://www.xeno-canto.org/}{Xeno-Canto}.
#' @usage query_wikiaves(term, X = NULL, file.name = c("Genus", "Specific_epithet"),
#' cores = 1, path = NULL, pb = TRUE)
#' @param term Character vector of length one indicating the genus, or genus and
#'  species, to query 'Xeno-Canto' database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}.
#'  More complex queries can be done by using search terms that follow the
#'  xeno-canto advance query syntax. This syntax uses tags to search within a particular aspect of the recordings
#'  (e.g. country, location, sound type). Tags are of the form tag:searchterm'. For instance, 'type:song'
#'  will search for all recordings in which the sound type description contains the word 'song'.
#'  Several tags can be included in the same query. The query "phaethornis cnt:belize' will only return
#'  results for birds in the genus \emph{Phaethornis} that were recorded in  Belize.
#'  See \href{https://www.xeno-canto.org/help/search}{Xeno-Canto's search help} for a full description and see examples below
#'  for queries using terms with more than one word.
#' @param cores Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files will be saved.
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return If X is not provided the function returns a data frame with the following recording information: recording ID, Genus, Specific epithet, Subspecies, English name, Recordist, Country, Locality, Latitude, Longitude, Vocalization type, Audio file, License, URL, Quality, Time, Date. Sound files in .mp3 format are downloaded into the working directory if download = \code{TRUE} or if X is provided; a column indicating the  names of the downloaded files is included in the output data frame.
#' @export
#' @name query_wikiaves
#' @details This function queries for avian vocalization recordings in the open-access
#' online repository \href{https://www.xeno-canto.org/}{Xeno-Canto}. It can return recordings metadata
#' or download the associated sound files. Complex queries can be done by using search terms that follow the
#'  xeno-canto advance query syntax (check "term" argument description).
#'  Files are double-checked after downloading and "empty" files are re-downloaded.
#'  File downloading process can be interrupted and resume later as long as the working directory is the same.
#'  Maps of recording coordinates can be produced using
#' \code{\link{map_xc}}.
#' @seealso \code{\link{map_xc}},
#' \href{https://marce10.github.io/2016/12/22/Download_a_single_recording_for_each_species_in_a_site_from_Xeno-Canto.html}{blog post on accessing Xeno-Canto recordings}
#' @examples
#' \dontrun{
#' # search without downloading
#' df1 <- query_wikiaves(term = 'Phaethornis anthophilus', download = FALSE)
#' View(df1)
#'
#' # downloading files
#'query_wikiaves(term = 'Phaethornis anthophilus', download = TRUE, path = tempdir())
#'
#' # check this folder
#' tempdir()
#'
#' ## search using xeno-canto advance query ###
#' orth.pap <- query_wikiaves(term = 'gen:orthonyx cnt:papua loc:tari', download = FALSE)
#'
#' # download file using the output data frame as input
#' query_wikiaves(X = orth.pap, path = tempdir())
#'
#' # use quotes for queries with more than 1 word (e.g. Costa Rica),note that the
#' # single quotes are used for the whole 'term' and double quotes for the 2-word term inside
#' #Phaeochroa genus in Costa Rica
#' phae.cr <- query_wikiaves(term = 'gen:phaeochroa cnt:"costa rica"', download = FALSE)
#'
#' # several terms can be searched for in the same field
#' # search for all female songs in sound type
#' femsong <- query_wikiaves(term = 'type:song type:female', download = FALSE)
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on nov-16-2016 (MAS)

query_wikiaves <-
  function(term,
           type = "photo",
           cores = 1,
           path = NULL,
           pb = TRUE,
           verbose = TRUE) {

    #check path to working directory
    if (is.null(path))
      path <- getwd() else
      if (!dir.exists(path))
        stop("'path' provided does not exist") else
      path <- normalizePath(path)

    #check internet connection
    a <- try(RCurl::getURL("www.wikiaves.com"), silent = TRUE)
    if (is(a, "try-error"))
      stop("No connection to wikiaves (check your internet connection!)")

    if (a == "Could not connect to the database")
      stop("wikiaves.com website is apparently down")

    # If cores is not numeric
    if (!is.numeric(cores))
      stop("'cores' must be a numeric vector of length 1")
    if (any(!(cores %% 1 == 0), cores < 1))
      stop("'cores' should be a positive integer")

    #format JSON
    term <- gsub(" ", "%20", term)

    #initialize search
    get_ids <-
      rjson::fromJSON(file = paste0(
        "https://www.wikiaves.com.br/getTaxonsJSON.php?term=",
        term
      ))

    if (length(get_ids) == 0)
      warning2("Search term not found") else {

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

    if (sum(get_ids$total_registers) == 0)
      warning2("No registers found") else{

    # get number of pages (20 is the default number of registers per page)
    get_ids$pages <- ceiling(get_ids$total_registers / 20)

    id_by_page_list <- lapply(1:nrow(get_ids), function(x){

      X <- get_ids[x, ]
      out_df <- data.frame(id = X$id, page = 1:X$pages)

    })

    id_by_page_df <- do.call(rbind, id_by_page_list)

    #search recs in wikiaves (results are returned in pages with 500 recordings each)
    if (pb &  verbose)
      write(file = "", x = paste0("Obtaining metadata (", sum(get_ids$total_registers) , " registers found):"))

    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & cores > 1)
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores)) else cl <- cores

# loop over pages
    query_output_list <- pblapply_sw_int(1:nrow(id_by_page_df), cl = cl,  function(i)
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

    names(query_output_df) <- c("id", "media.type", "user.id", "sp.id", "scientific.name", "common.name", "repository.id", "author", "user.name", "date", "verified", "location", "location.id", "comments", "likes", "visualizations", "url", "duration", "repostory")

    return(query_output_df)
    }
  }
}
