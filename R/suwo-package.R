#' suwo: A package to streamline bioacoustic analysis
#'
#' suwo is intended to facilitate the search of biological observation data across multiple databases
#'
#' The main features of the package are:
#'   \itemize{
#'   \item The use of loops to apply tasks through acoustic signals referenced in a selection table
#'   \item The production of images in the working folder with spectrograms that allow to organize data and verify acoustic analyzes
#'   }
#'
#' The package offers functions to:
#'   \itemize{
#'   \item Explore and download observational data from GBIF, inaturalist, Xenocanto, Observation.org and Wikiaves
#'   \item Download different types of data across multiple databases
#'   \item Consolidate databases and annotation tables
#'   }
#'
#' Most of the functions allow the parallelization of tasks, which distributes the tasks among several processors to improve computational efficiency. Tools to evaluate the performance of the analysis at each step are also available. In addition, warbleR satisfies the need for rigorous open source bioacoustic analysis, which facilitates opportunities for use in research and innovation of additional custom analyzes.
#'
#' The suwo package offers three overarching categories of
#'   functions:
#'
#' @section Obtaining animal vocalization data:
#'
#'
#' @import parallel
#' @import RCurl
#' @import jsonlite
#' @importFrom utils download.file read.csv write.csv
#' @importFrom methods is
#' @importFrom cli style_bold style_italic make_ansi_style num_ansi_colors cli_text
#' @author Marcelo Araya-Salas & Jorge Elizondo-Calvo
#'
#'   Maintainer: Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
#' @docType _PACKAGE
#' @name suwo
#' @details License: GPL (>= 2)
NULL
#> NULL
#'
