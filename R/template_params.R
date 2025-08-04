#' @param cores Numeric vector of length 1. Controls whether parallel computing is applied by specifying the number of cores to be used. Default is 1 (i.e. no parallel computing). Can be set globally for the current R session via the "mc.cores" option (see \code{\link[base]{options}}). Note that some repositories might not support parallel queries from the same IP address and errors might occur during the search.
#' @param pb Logical argument to control if progress bar is shown. Default is \code{TRUE}. Can be set globally for the current R session via the "pb" option (see \code{\link[base]{options}}).
#' @param term Character vector of length one indicating species, to query the database. For example, \emph{Phaethornis longirostris}. Required.
#' @param verbose Logical argument that determines if text is shown in console. Default is \code{TRUE}. Can be set globally for the current R session via the "verbose" option (see \code{\link[base]{options}}).
#' @param all_data Logical argument that determines if all data available from database is shown in the results of search. Default is \code{FALSE}. Can be set globally for the current R session via the "all_data" option (see \code{\link[base]{options}}).
#' @param token Character referring to the token assigned by Observation.org as authorization for searches.
#'
#' @name template_params
NULL
