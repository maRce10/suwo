% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_media.R
\name{download_media}
\alias{download_media}
\title{Download media files from repositories}
\usage{
download_media(
  metadata,
  path = "./",
  pb = getOption("pb", TRUE),
  verbose = getOption("verbose", TRUE),
  cores = getOption("mc.cores", 1)
)
}
\arguments{
\item{metadata}{Data frame with a 'file_url' column and any other column listed in the file.name argument. Only the media listed in the data frame
will be downloaded (\code{download} argument is automatically set to \code{TRUE}). This can be used to select
the recordings to be downloaded based on their attributes.}

\item{path}{Character that defines the location for the downloaded files.}

\item{pb}{Logical argument to control if progress bar is shown. Default is \code{TRUE}. Can be set globally for the current R session via the "pb" option (see \code{\link[base]{options}}).}

\item{verbose}{Logical argument that determines if text is shown in console. Default is \code{TRUE}. Can be set globally for the current R session via the "verbose" option (see \code{\link[base]{options}}).}

\item{cores}{Numeric vector of length 1. Controls whether parallel computing is applied by specifying the number of cores to be used. Default is 1 (i.e. no parallel computing). Can be set globally for the current R session via the "mc.cores" option (see \code{\link[base]{options}}).}
}
\value{
media files
}
\description{
\code{download_media} downloads recordings and metadata from \href{https://www.xeno-canto.org/}{Xeno-Canto}, \href{https://www.wikiaves.com/}{wikiaves} or \href{https://www.gbif.org/}{gbif}.
}
\details{
File downloading process can be interrupted and resume later as long as the working directory is the same.
}
\examples{
\dontrun{
  download_media(query_result, path = "./home")
}

}
\seealso{
\code{\link{query_gbif}},
}
\author{
Marcelo Araya-Salas (\email{marcelo.araya@ucr.ac.cr})
}
