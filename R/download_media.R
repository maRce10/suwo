#' Download media files from repositories
#'
#' \code{download_media} downloads recordings and metadata from
#' \href{https://www.xeno-canto.org/}{Xeno-Canto},
#' \href{https://www.wikiaves.com/}{wikiaves} or
#' \href{https://www.gbif.org/}{gbif}.
#' @inheritParams template_params
#' @param path Directory path where the output media files will be saved.
#' By default files are saved into the current working directory (\code{"."}).
#' @param overwrite Logical. If TRUE, existing files (in \code{"path"}) with
#' the same name will be overwritten. Default is FALSE.
#' @param folder_by Character string with the name of a character or factor
#' column in the metadata data frame. If supplied the function will use the
#' unique values in that column to create subfolders within \code{"path"} and
#' the files will be downloaded into the corresponding folder. By default no
#' subfolders are created and all files are saved in the path provided. Missing
#' values (NAs) are saved in a folder called
#' \code{paste0("unknown_", folder_by)}. Special characters that are not
#' allowed in folder names will be modified or removed. If any of the folder
#' names already exist in \code{"path"}, they will be used as is.
#' @return Downloads media files into the supplied directory path
#' (\code{"path"}) and returns (invisibly) the input data frame with
#' two additional columns: \code{downloaded_file_name} with the name of
#' the downloaded file (if downloaded or already in the directory), and
#' \code{download_status} with the result of the download process for each
#' file (either "saved", "overwritten", "already there (not downloaded)",
#' or "failed").
#' @export
#' @name download_media
#' @details This function will take the output data frame of any of the
#' "query_x()" functions and download the associated media files. The
#' function will download all files into a single directory
#' (argument \code{"path"}). File downloading process can be interrupted and
#' resume later as long as the working directory is the same. By default only
#' the missing files will be downloaded when resuming. Users only need to rerun
#' the same function call. Can also be used on a updated query output
#' (see \code{\link{update_metadata}}) to add the new media files to the
#' existing media pool.
#' @seealso \code{\link{query_gbif}}, \code{\link{query_macaulay}}
#' @examples
#' \dontrun{
#'   phae_anth <- query_xenocanto(species = 'Phaethornis anthophilus',
#'   all_data = FALSE)
#'
#'   # donwload the first to files
#'   phae_anth_downl <- download_media(metadata = phae_anth[1:2, ],
#'   path = tempdir())
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

download_media <-
  function(metadata,
           path = ".",
           pb = getOption("pb", TRUE),
           verbose = getOption("verbose", TRUE),
           cores = getOption("mc.cores", 1),
           overwrite = FALSE,
           folder_by = NULL) {
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
    metadata$downloaded_file_name <-
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
      write(file = "", x = "Downloading media files:")
    }
    if (Sys.info()[1] == "Windows" & cores > 1) {
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores))
    } else {
      cl <- cores
    }

    metadata$download_status <-
      unlist(pblapply_sw_int(
        pbar = pb,
        X = seq_len(nrow(metadata)),
        cl = cl,
        FUN = function(x) {
          .download(metadata, x, path, overwrite, folder_by)
        }
      ))

    # add folder to file name
    if (!is.null(folder_by)){
      metadata$downloaded_file_name <-
        file.path(metadata[, folder_by], metadata$downloaded_file_name)
    }

    # report results
    if (length(unique(metadata$download_status)) > 1)
      report_message <- c()


    # report failed files
    if (any(metadata$download_status == "failed")) {

      if (sum(metadata$download_status == "failed") == nrow(metadata)){
        report_message <- .color_text("All files failed to download", as = "failure")
      } else {
        report_message <- c(report_message, "x" = paste0(cli::pluralize("{sum(metadata$download_status == 'failed')} file{?s} failed to download")))
      }
      # remove file name from "downloaded_file_name"
      metadata$downloaded_file_name[metadata$download_status == "failed"] <- NA
    }

    # report not-overwritten files
    if (any(metadata$download_status == "already there (not downloaded)")) {

      if (sum(metadata$download_status == "already there (not downloaded)") == nrow(metadata)){
        report_message <- .color_text("All files were already there (overwritten = FALSE)", as = "success")
      } else {

        report_message <- c(report_message, "!" = paste0(cli::pluralize("{sum(metadata$download_status == 'already there (not downloaded)')} file{?s} w{?as/ere} already there (overwritten = FALSE)")))
      }
    }

    # report overwritten files
    if (any(metadata$download_status == "overwritten")) {

      if (sum(metadata$download_status == "overwritten") == nrow(metadata)){
        report_message <- .color_text("All files were downloaded successfully (and all were overwritten)", as = "success")
      } else {
        report_message <- c(report_message, "v" = paste0(cli::pluralize("{sum(metadata$download_status == 'overwritten')} file{?s} w{?as/ere} downloaded (and overwritten)")))
      }
    }

        # report successful files
    if (any(metadata$download_status == "saved")) {

      if (sum(metadata$download_status == "saved") == nrow(metadata)){
        report_message <- .color_text("All files were downloaded successfully", as = "success")
      } else {
        report_message <- c(report_message, "v" = paste0(cli::pluralize("{sum(metadata$download_status == 'saved')} file{?s} w{?as/ere} downloaded successfully")))
      }
    }

    if (length(unique(metadata$download_status)) > 1)
    report_message <-  c(report_message, "i" = "check  the `download_status` column in the output data frame (invisibly returned) for details ")


    # report download results
    if (verbose){
      cli::cli_bullets(report_message)
    }

    # remove extra column
    metadata$non_dup_key <- NULL

    # return data frame without printing them
    invisible(metadata)
  }
