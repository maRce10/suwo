#' Download media files from repositories
#'
#' \code{download_media} downloads recordings and metadata from \href{https://www.xeno-canto.org/}{Xeno-Canto}, \href{https://www.wikiaves.com/}{wikiaves} or \href{https://www.gbif.org/}{gbif}.
#' @inheritParams template_params
#' @param metadata Data frame with the output of any of the media query functions in this package ( \code{\link{query_gbif}}, \code{\link{query_observation}}, \code{\link{query_wikiaves}}, \code{\link{query_inaturalist}}, \code{\link{query_macaulay}}, \code{\link{query_xenocanto}}).
#' @param path Directory path where the output media files will be saved. By default files are saved into the current working directory (\code{"."}).
#' @return media files
#' @export
#' @name download_media
#' @details File downloading process can be interrupted and resume later as long as the working directory is the same.
#' @seealso \code{\link{query_gbif}}, \code{\link{query_macaulay}}
#' @examples
#' \dontrun{
#'   download_media(query_result, path = "./home")
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

download_media <-
  function(metadata,
           path = ".",
           pb = getOption("pb", TRUE),
           verbose = getOption("verbose", TRUE),
           cores = getOption("mc.cores", 1)) {
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


    # Abbreviate repository name
    metadata$repository <- vapply(metadata$repository, function(x) switch(
      x,
      `Xeno-Canto` = "XC",
      Observation = "OBS",
      GBIF = "GBIF",
      Wikiaves = "WA",
      iNaturalist = "INAT",
      `Macaulay Library` = "ML"
    ), FUN.VALUE = character(1),
    USE.NAMES = FALSE)

    # rename if any duplicated names
    metadata$non_dup_key <- unlist(lapply(unique(metadata$key), function(x) {
      on <- metadata$key[metadata$key == x]
      if (length(on) > 1) {
        return(paste0(on, "-", seq_len(length(on))))
      } else {
        return(x)
      }
    }))

    # create file name
    metadata$download_file_name <-
      paste0(
        gsub(pattern = " ", "_", x = metadata$species),
        "-",
        metadata$repository,
        metadata$non_dup_key,
        ".",
        metadata$file_extension
      )

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
        X = seq_len(nrow(metadata)),
        cl = cl,
        FUN = function(x) {
          .download(metadata, x, path)
        }
      ))

    if (any(!success_dwnld)) {
      options(suwo = c(
        .Options$suwo,
        list(failed_downloads = metadata$download_file_name[!success_dwnld])
      ))

      message("Some files couldn't be downloaded, check `.Options$suwo$failed_downloads`")
    }

    # return file names without printing them
    invisible(file.path(normalizePath(path), metadata$download_file_name[success_dwnld]))
  }
