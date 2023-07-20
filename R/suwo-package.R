#' ohun: A package to streamline bioacoustic analysis
#'
#' warbleR is intended to facilitate the analysis of the structure of animal acoustic signals in R. Users can collect open-access avian recordings or enter their own data into a workflow that facilitates spectrographic visualization and measurement of acoustic parameters. warbleR makes use of the fundamental sound analysis tools of the seewave package, and offers new tools for acoustic structure analysis. These tools are available for batch analysis of acoustic signals.
#'
#' The main features of the package are:
#'   \itemize{
#'   \item The use of loops to apply tasks through acoustic signals referenced in a selection table
#'   \item The production of images in the working folder with spectrograms that allow to organize data and verify acoustic analyzes
#'   }
#'
#' The package offers functions to:
#'   \itemize{
#'   \item Explore and download Xeno Canto recordings
#'   \item Explore, organize and manipulate multiple sound files
#'   \item Detect signals automatically (in frequency and time)
#'   \item Create spectrograms of complete recordings or individual signals
#'   \item Run different measures of acoustic signal structure
#'   \item Evaluate the performance of measurement methods
#'   \item Catalog signals
#'   \item Characterize different structural levels in acoustic signals
#'   \item Statistical analysis of duet coordination
#'   \item Consolidate databases and annotation tables
#'   }
#'
#' Most of the functions allow the parallelization of tasks, which distributes the tasks among several processors to improve computational efficiency. Tools to evaluate the performance of the analysis at each step are also available. In addition, warbleR satisfies the need for rigorous open source bioacoustic analysis, which facilitates opportunities for use in research and innovation of additional custom analyzes.
#'
#' The warbleR package offers three overarching categories of
#'   functions:
#'
#' @section Obtaining animal vocalization data:
#'
#'   \code{\link{query_xc}}: Download recordings and/or metadata from 'Xeno-Canto'
#'
#' @import parallel
#' @import RCurl
#' @import jsonlite
#' @importFrom utils download.file
#' @importFrom methods is
#' @importFrom cli style_bold style_italic make_ansi_style num_ansi_colors
#' @author Marcelo Araya-Salas & Grace Smith Vidaurre
#'
#'   Maintainer: Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
#' @docType package
#' @name warbleR
#' @details License: GPL (>= 2)
NULL
#> NULL
#'
