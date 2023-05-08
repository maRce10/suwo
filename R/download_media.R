#' Download media files from repositories
#'
#' \code{download_media} downloads recordings and metadata from \href{https://www.xeno-canto.org/}{Xeno-Canto}, \href{https://www.wikiaves.com/}{wikiaves} or \href{https://www.gbif.org/}{gbif}.
#' @usage download_media(metadata)

#' @param metadata Data frame with a 'file_url' column and any other column listed in the file.name argument. Only the media listed in the data frame
#' will be downloaded (\code{download} argument is automatically set to \code{TRUE}). This can be used to select
#' the recordings to be downloaded based on their attributes.

#' @return media files
#' @export
#' @name download_media
#' @details File downloading process can be interrupted and resume later as long as the working directory is the same.
#'  Maps of recording coordinates can be produced using
#' \code{\link{map_results}}.
#' @seealso \code{\link{map_results}},
#' @examples
#' \dontrun{
#' }
#'
#' @references {
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

download_media <- function(metadata, path = "./", file.name = NULL, pb= TRUE, verbose = TRUE, cores = 1){

  #stop if metadata is not a data frame
  if (!is(metadata, "data.frame")) stop2("metadata is not a data frame")

  # Add file extension
  metadata$extension  <- vapply(X = metadata$file_url, FUN = function(x){

    x2 <- strsplit(x, "\\?")[[1]][1]

    max_x2 <- max(gregexpr("\\.", x2)[[1]])

    extension <- substr(x = x2, start = max_x2, stop = nchar(x2))

    if (extension == ".mpga")
      extension <- ".mp3"

    return(extension)

  }, FUN.VALUE = character(1), USE.NAMES = FALSE)


  # rename if any duplicated names
  metadata$non_dup_key <- unlist(lapply(
    unique(metadata$key),
    function(x) {
      on <- metadata$key[metadata$key == x]
      if (length(on) > 1) {
        return(paste0(on, "-", seq_len(length(on))))
      } else {
        return(x)
      }
    }
  ))

   # create file name
    metadata$file.name <- tolower(paste0(gsub(pattern = " ", "_", x = metadata$species), "-", metadata$non_dup_key, "-", metadata$repository, metadata$extension))


  #Function to download file according to repository
    downloadFUN <-  function(metadata, x){
      # if (!file.exists(metadata$file.name[x])){
      #   if (metadata$repository[x] == "XC"){
      #     download.file(
      #     url = paste("https://xeno-canto.org/", metadata$file_url[x], "/download", sep = ""),
      #     destfile = file.path(path, metadata$file.name[x]),
      #     quiet = TRUE,  mode = "wb", cacheOK = TRUE,
      #     extra = getOption("download.file.extra"))
      #     return (NULL)
      #   } else if (metadata$repository[x] == "wikiaves"){
      #     download.file(
      #     url = as.character(metadata$file_url[x]),
      #     destfile = file.path(path, metadata$record.id[x]),
      #     quiet = TRUE,  mode = "wb", cacheOK = TRUE,
      #     extra = getOption("download.file.extra"))
      #     return (NULL)
      #   } else if (metadata$repository[x] == "GBIF"){
          dl_result <- try(download.file(
              url = as.character(metadata$file_url[x]),
              destfile = file.path(path, metadata$file.name[x]),
              quiet = TRUE,  mode = "wb", cacheOK = TRUE,
              extra = getOption("download.file.extra")), silent = TRUE)


          if (is(dl_result, "try-error"))
            return (FALSE) else
              return (TRUE)
        }


    # set clusters for windows OS
    if (pb  & verbose)
      write(file = "", x = "Downloading files...")
    if (Sys.info()[1] == "Windows" & cores > 1)
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores)) else cl <- cores

    success_dwnld <- unlist(pblapply_sw_int(pbar = pb, X = 1:nrow(metadata), cl = cl, FUN = function(x)
    {
      downloadFUN(metadata, x)
    }))

  #   if (pb & verbose) write(file = "", x ="double-checking downloaded files")
  #
  #   #check if some files have no data
  #   fl <- list.files(path = path, pattern = ".mp3$")
  #   size0 <- fl[file.size(file.path(path, fl)) == 0]
  #
  #   #if so redo those files
  #   if (length(size0) > 0)
  #   {  Y <- metadata[metadata$file.name %in% size0, ]
  #   unlink(size0)
  #
  #   # set clusters for windows OS
  #   if (Sys.info()[1] == "Windows" & cores > 1)
  #     cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores)) else cl <- cores
  #
  #
  #   a1 <- pblapply_sw_int(pbar = pb, X = 1:nrow(Y), cl = cl, FUN = function(x)
  #   {
  #     try(downloadFUN(Y, x), silent = TRUE)
  #   })
  # }

    if (any(!success_dwnld)){
      .Options$suwo$failed_downloads  <- c(metadata$file.name[!success_dwnld])

      message("Some files couldn't be downloaded, check `.Options$suwo$failed_downloads`")

      }
}

