#' suwo: A package to streamline bioacoustic analysis
#'
#' suwo is intended to facilitate the search of biological observation data
#' across multiple databases
#'
#' The main features of the package are:
#'   \itemize{
#'   \item The use of loops to apply tasks through acoustic signals referenced
#'   in a selection table
#'   \item The production of images in the working folder with spectrograms
#'   that allow to organize data and verify acoustic analyzes
#'   }
#'
#' The package offers functions to:
#'   \itemize{
#'   \item Explore and download observational data from iNaturalist, GBIF,
#'   Macaulay Library, Observation.org, Wikiaves and Xenocanto.
#'   \item Download different types of data across multiple databases
#'   \item Consolidate databases and annotation tables
#'   }
#'
#' Most of the functions allow the parallelization of tasks, which distributes
#' the tasks among several processors to improve computational efficiency.
#' Tools to evaluate the performance of the analysis at each step are also
#' available. In addition, warbleR satisfies the need for rigorous open source
#' bioacoustic analysis, which facilitates opportunities for use in research
#' and innovation of additional custom analyzes.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_user_agent req_error req_perform resp_is_error
#' resp_body_string resp_status resp_status_desc resp_body_json
#' @importFrom leaflet awesomeIcons leaflet addTiles addAwesomeMarkers
#' markerClusterOptions addMiniMap addEasyButton easyButton JS easyButtonState
#' @importFrom parallel makePSOCKcluster detectCores parLapply mclapply
#' @importFrom fs dir_tree
#' @importFrom jpeg readJPEG
#' @importFrom graphics rasterImage
#' @importFrom RecordLinkage compare.dedup
#' @importFrom kableExtra cell_spec kbl kable_styling column_spec
#' @importFrom lubridate parse_date_time
#' @importFrom tools file_ext
#' @importFrom utils packageVersion
#' @importFrom viridis viridis
#' @importFrom rlang arg_match
#' @importFrom stats setNames complete.cases
#' @importFrom utils download.file read.csv write.csv
#' @importFrom methods is
#' @importFrom cli cli_alert_info cli_alert_danger cli_alert_warning
#' cli_alert_success pluralize cli_bullets
#' @author Marcelo Araya-Salas, Jorge Elizondo-Calvo & Alejandro Rico-Guevara
#'
#'   Maintainer: Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
#' @docType package
#' @details License: GPL (>= 2)
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
