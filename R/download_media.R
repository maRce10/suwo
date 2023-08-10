#' Download media files from repositories
#'
#' \code{download_media} downloads recordings and metadata from \href{https://www.xeno-canto.org/}{Xeno-Canto}, \href{https://www.wikiaves.com/}{wikiaves} or \href{https://www.gbif.org/}{gbif}.
#' @usage download_media(metadata, path = "./", pb = TRUE, verbose = TRUE, cores = 1)
#' @param metadata Data frame with a 'file_url' column and any other column listed in the file.name argument. Only the media listed in the data frame
#' will be downloaded (\code{download} argument is automatically set to \code{TRUE}). This can be used to select
#' the recordings to be downloaded based on their attributes.
#' @param path Character that defines the location for the downloaded files.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param verbose Logical argument that determines if text is shown in console. Default is \code{TRUE}.
#' @param cores Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @return media files
#' @export
#' @name download_media
#' @details File downloading process can be interrupted and resume later as long as the working directory is the same.
#' @seealso \code{\link{query_gbif}},
#' @examples
#' \dontrun{
#'
#' }
#'
#' @references {
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

download_media <-
  function(metadata,
           path = "./",
           pb = TRUE,
           verbose = TRUE,
           cores = 1) {
    # check arguments
    arguments <- as.list(base::match.call())[-1]

    # add objects to argument names
    for (i in names(arguments)) {
      arguments[[i]] <- get(i)
    }

    # check each arguments
    check_results <- check_arguments(args = arguments)

    # report errors
    checkmate::reportAssertions(check_results)

    # Add file extension
    if (metadata$repository[1] == "XC") {
      metadata$extension <-
        vapply(
          X = metadata$file.name,
          FUN = function(x) {
            x2 <- strsplit(x, "\\?")[[1]][1]

            max_x2 <- max(gregexpr("\\.", x2)[[1]])

            extension <- substr(x = x2,
                                start = max_x2,
                                stop = nchar(x2))

            if (extension == ".mpga") {
              extension <- ".mp3"
            }

            return(extension)
          },
          FUN.VALUE = character(1),
          USE.NAMES = FALSE
        )
    }

    if (metadata$repository[1] == "INAT") {
      if (exists("media_extension", where = metadata)) {
        metadata$extension <-
          vapply(
            X = metadata$media_extension,
            FUN = function(x) {
              extension <- strsplit(x, "/")[[1]][2]

              if (extension == "mpeg") {
                extension <- ".mp3"
              }

              if (extension == "x-wav") {
                extension <- ".wav"
              }

              if (extension == "x-m4a") {
                extension <- ".m4a"
              }

              if (extension == "mp4") {
                extension <- ".mp4"
              }

              return(extension)
            },
            FUN.VALUE = character(1),
            USE.NAMES = FALSE
          )
      }
    }

    if (metadata$repository[1] == "INAT") {
      if (!exists("media_extension", where = metadata)) {
        metadata$extension <-
          vapply(
            X = metadata$file_url,
            FUN = function(x) {
              x2 <- strsplit(x, "\\?")[[1]][1]

              max_x2 <- max(gregexpr("\\.", x2)[[1]])

              extension <- ".jpeg"

              return(extension)
            },
            FUN.VALUE = character(1),
            USE.NAMES = FALSE
          )
      }
    } else if (metadata$repository[1] != "XC") {
      metadata$extension <-
        vapply(
          X = metadata$file_url,
          FUN = function(x) {
            x2 <- strsplit(x, "\\?")[[1]][1]

            max_x2 <- max(gregexpr("\\.", x2)[[1]])

            extension <- substr(x = x2,
                                start = max_x2,
                                stop = nchar(x2))

            if (extension == ".mpga") {
              extension <- ".mp3"
            }

            return(extension)
          },
          FUN.VALUE = character(1),
          USE.NAMES = FALSE
        )
    }

    # Abbreviate repository name
    repo <- metadata$repository[1]

    metadata$repository <- switch(
      repo,
      XC = "XC",
      Observation = "OBS",
      GBIF = "GBIF",
      wikiaves = "WA",
      INAT = "INAT",
      Macaulay = "ML"
    )

    # rename if any duplicated names
    metadata$non_dup_key <- unlist(lapply(unique(metadata$key),
                                          function(x) {
                                            on <- metadata$key[metadata$key == x]
                                            if (length(on) > 1) {
                                              return(paste0(on, "-", seq_len(length(on))))
                                            } else {
                                              return(x)
                                            }
                                          }))

    # create file name
    metadata$file.name <-
      paste0(
        gsub(pattern = " ", "_", x = metadata$species),
        "-",
        metadata$repository,
        metadata$non_dup_key,
        metadata$extension
      )

    # Function to download file according to repository
    downloadFUN <- function(metadata, x) {
      dl_result <- try(download.file(
        url = as.character(metadata$file_url[x]),
        destfile = file.path(path, metadata$file.name[x]),
        quiet = TRUE,
        mode = "wb",
        cacheOK = TRUE,
        extra = getOption("download.file.extra")
      ),
      silent = TRUE)


      if (is(dl_result, "try-error")) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    # set clusters for windows OS
    if (pb & verbose) {
      write(file = "", x = "Downloading files...")
    }
    if (Sys.info()[1] == "Windows" & cores > 1) {
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores))
    } else {
      cl <- cores
    }

    success_dwnld <-
      unlist(pblapply_sw_int(
        pbar = pb,
        X = 1:nrow(metadata),
        cl = cl,
        FUN = function(x) {
          downloadFUN(metadata, x)
        }
      ))

    if (any(!success_dwnld)) {
      options(suwo = c(
        .Options$suwo,
        list(failed_downloads = metadata$file.name[!success_dwnld])
      ))

      message("Some files couldn't be downloaded, check `.Options$suwo$failed_downloads`")
    }
  }
