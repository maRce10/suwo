#' Access 'Xeno-Canto' recordings and metadata
#'
#' \code{query_xenocanto} searches for metadata from \href{https://www.xeno-canto.org/}{Xeno-Canto}.
#' @usage query_xenocanto(term, X = NULL, file.name = c("Genus", "Specific_epithet"),
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
#' @name query_xenocanto
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
#' df1 <- query_xenocanto(term = 'Phaethornis anthophilus', download = FALSE)
#' View(df1)
#'
#' # downloading files
#'query_xenocanto(term = 'Phaethornis anthophilus', download = TRUE, path = tempdir())
#'
#' # check this folder
#' tempdir()
#'
#' ## search using xeno-canto advance query ###
#' orth.pap <- query_xenocanto(term = 'gen:orthonyx cnt:papua loc:tari', download = FALSE)
#'
#' # download file using the output data frame as input
#' query_xenocanto(X = orth.pap, path = tempdir())
#'
#' # use quotes for queries with more than 1 word (e.g. Costa Rica),note that the
#' # single quotes are used for the whole 'term' and double quotes for the 2-word term inside
#' #Phaeochroa genus in Costa Rica
#' phae.cr <- query_xenocanto(term = 'gen:phaeochroa cnt:"costa rica"', download = FALSE)
#'
#' # several terms can be searched for in the same field
#' # search for all female songs in sound type
#' femsong <- query_xenocanto(term = 'type:song type:female', download = FALSE)
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on nov-16-2016 (MAS)

query_xenocanto <- function(term, X = NULL, file.name = c("Genus", "Specific_epithet"),
                   cores = 1, path = NULL, pb = TRUE, verbose = TRUE) {


  #check path to working directory
  if (is.null(path)) path <- getwd() else
    if (!dir.exists(path))
      stop("'path' provided does not exist") else
        path <- normalizePath(path)

  #check internet connection
  a <- try(RCurl::getURL("www.xeno-canto.org"), silent = TRUE)
  if (is(a, "try-error")) stop("No connection to xeno-canto.org (check your internet connection!)")

  if (a == "Could not connect to the database")  stop("xeno-canto.org website is apparently down")

  # If cores is not numeric
  if (!is.numeric(cores)) stop("'cores' must be a numeric vector of length 1")
  if (any(!(cores %% 1 == 0),cores < 1)) stop("'cores' should be a positive integer")


    #search recs in xeno-canto (results are returned in pages with 500 recordings each)
    if (pb &  verbose)
      write(file = "", x = "Obtaining recording list...")

    #format JSON
    term <- gsub(" ", "%20", term)

    #initialize search
    q <- rjson::fromJSON(file = paste0("https://www.xeno-canto.org/api/2/recordings?query=", term))

    if (as.numeric(q$numRecordings) == 0 ) {
      if (verbose)
      cat("No recordings were found")
      } else {

      nms <- c("id", "gen", "sp", "ssp", "en", "rec", "cnt", "loc", "lat", "lng", "type", "file", "lic", "url", "q", "time", "date")

      ### loop over pages
      # set clusters for windows OS
      if (Sys.info()[1] == "Windows" & cores > 1)
        cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores)) else cl <- cores

        records_list <- pblapply_sw_int(pbar = pb, X = 1:q$numPages, cl = cl, FUN = function(y)
      {
        #search for each page
        a <- rjson::fromJSON(file = paste0("https://www.xeno-canto.org/api/2/recordings?query=", term, "&page=", y))

        #put together as data frame
        d <-lapply(1:length(a$recordings), function(z) data.frame(t(unlist(a$recordings[[z]]))))

        d2 <- lapply(d,  function(x)
        {
          if (!all(nms %in% names(x))){
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
        d3 <- lapply(d2, function(X)
        {
          nms <- names(X)
          if (length(nms) != length(cnms))
            for(i in cnms[!cnms %in% nms]) {
              X <- data.frame(X,  NA, stringsAsFactors = FALSE, check.names = FALSE)
              names(X)[ncol(X)] <- i
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
        records_list2 <- lapply(records_list, function(X)
        {
          nms <- names(X)
          if (length(nms) != length(cnms))
            for(i in cnms[!cnms %in% nms]) {
              X <- data.frame(X,  NA, stringsAsFactors = FALSE, check.names = FALSE)
              names(X)[ncol(X)] <- i
            }
          return(X)
        })

      # save results in a single data frame
      results <- do.call(rbind, records_list2)

      # convert factors to characters
      indx <- sapply(results, is.factor)
      results[indx] <- lapply(results[indx], as.character)

      #order columns
    results <- results[ ,order(match(names(results), nms))]

    names(results)[match(c("id", "gen", "sp", "ssp", "en", "rec", "cnt", "loc", "lat", "lng", "alt", "type", "file", "lic", "url", "q", "length", "time", "date", "uploaded", "rmk", "bird.seen", "playback.used"), names(results))] <- c("record.id", "genus", "specific.epithet", "subspecies", "english.name", "recordist",
                        "country", "locality", "latitude", "longitude", "altitude", "vocalization.type", "audio_file", "license",
                        "url", "quality", "length", "time", "date", "uploaded", "remarks", "bird.seen","playback.used")[which(c("id", "gen", "sp", "ssp", "en", "rec", "cnt", "loc", "lat", "lng", "alt", "type", "file", "lic", "url", "q", "length", "time", "date", "uploaded", "rmk", "bird.seen", "playback.used") %in% names(results))]

    # rename also columns
    names(results) <- gsub("also", "other.species", names(results))
    # rename
    names(results) <- gsub("sono.", "spectrogram.", names(results))

    #remove duplicates
    results <- results[!duplicated(results$record.id), ]

    if (pb  & verbose)
      write(file = "", x = paste0(nrow(results), " recording(s) found!"))
    }

 if (as.numeric(q$numRecordings) > 0) {

   # convert lat long to numbers
   results$latitude <- as.numeric(results$latitude)
   results$longitude <- as.numeric(results$longitude)

   return(droplevels(results))
   }

   }
