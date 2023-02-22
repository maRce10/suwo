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

download_media <- function(metadata, path = "./", file.name = NULL, pb= TRUE, verbose = TRUE){


  #stop if metadata is not a data frame
  if (!is(metadata, "data.frame")) stop("metadata is not a data frame")

  #stop if the basic columns are not found
  if (!is.null(file.name))
  {if (any(!c(file.name, "file_url") %in% colnames(metadata)))
    stop(paste(paste(c(file.name, "file_url")[!c(file.name, "file_url") %in% colnames(metadata)], collapse=", "), "column(s) not found in data frame"))} else
      if (!"file_url" %in% colnames(metadata))
        stop("file_url column not found in data frame")

  #download recordings

    if (any(file.name == "file_url")) file.name <- file.name[-which(file.name == "file_url")]

    if (!is.null(file.name))  {  if (length(which(tolower(names(metadata)) %in% file.name)) > 1)
      fn <- apply(metadata[,which(tolower(names(metadata)) %in% file.name)], 1 , paste , collapse = "-" ) else
        fn <- metadata[,which(tolower(names(metadata)) %in% file.name)]
      metadata$media.files <- paste(paste(fn, paste0(metadata$repository, metadata$file_url), sep = "-"), ".mp3", sep = "")
    } else
      metadata$media.files <- paste0(metadata$repository, metadata$file_url, ".mp3")

  #Function to download file according to repository

    xcFUN <-  function(metadata, x){
      if (!file.exists(metadata$media.files[x])){
        if (metadata$repository[x] == "XC"){
          download.file(
          url = paste("https://xeno-canto.org/", metadata$file_url[x], "/download", sep = ""),
          destfile = file.path(path, metadata$media.files[x]),
          quiet = TRUE,  mode = "wb", cacheOK = TRUE,
          extra = getOption("download.file.extra"))
          return (NULL)
        } else if (metadata$repository[x] == "wikiaves"){
          download.file(
          url = as.character(metadata$file_url[x]),
          destfile = file.path(path, metadata$record.id[x]),
          quiet = TRUE,  mode = "wb", cacheOK = TRUE,
          extra = getOption("download.file.extra"))
          return (NULL)
        } else if (metadata$repository[x] == "GBIF"){
            download.file(
              url = as.character(metadata$file_url[x]),
              destfile = file.path(path, metadata$species[x]),
              quiet = TRUE,  mode = "wb", cacheOK = TRUE,
              extra = getOption("download.file.extra"))
            return (NULL)

        }

      }
    }

    # set clusters for windows OS
    if (pb  & verbose)
      write(file = "", x = "Downloading files...")
    if (Sys.info()[1] == "Windows" & cores > 1)
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores)) else cl <- cores

    a1 <- pblapply_sw_int(pbar = pb, X = 1:nrow(metadata), cl = cl, FUN = function(x)
    {
      xcFUN(metadata, x)
    })

    if (pb & verbose) write(file = "", x ="double-checking downloaded files")

    #check if some files have no data
    fl <- list.files(path = path, pattern = ".mp3$")
    size0 <- fl[file.size(file.path(path, fl)) == 0]

    #if so redo those files
    if (length(size0) > 0)
    {  Y <- metadata[metadata$media.files %in% size0, ]
    unlink(size0)

    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & cores > 1)
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores)) else cl <- cores


    a1 <- pblapply_sw_int(pbar = pb, X = 1:nrow(Y), cl = cl, FUN = function(x)
    {
      try(xcFUN(Y, x), silent = TRUE)
    })
  }
}

