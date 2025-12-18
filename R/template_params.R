#' @param cores Numeric vector of length 1. Controls whether parallel computing
#' is applied by specifying the number of cores to be used. Default is 1
#' (i.e. no parallel computing). Can be set globally for the current R session
#' via the "mc.cores" option (e.g.  \code{options(mc.cores = 2)}). Note that
#' some repositories might not support parallel queries from the same IP
#' address as it might be identified as denial-of-service cyberattack.
#' @param pb Logical argument to control if progress bar is shown. Default
#' is \code{TRUE}. Can be set globally for the current R session via the "pb"
#' option ( \code{options(pb = TRUE)}).
#' @param species Character string with the scientific name of a species in
#' the format: "Genus epithet". Required. Can be set globally for the current
#' R session via the "term" option
#' (e.g. \code{options(term = "Hypsiboas rufitelus")}).
#' @param verbose Logical argument that determines if text is shown in
#' console. Default is \code{TRUE}. Can be set globally for the current R
#' session via the "verbose" option
#' ( \code{options(verbose = TRUE)}).
#' @param all_data Logical argument that determines if all data available
#' from database is shown in the results of search. Default is \code{FALSE}.
#' Can be set globally for the current R session via the "all_data"
#' option ( \code{options(all_data = TRUE)}).
#' @param raw_data Logical argument that determines if the raw data from the
#' repository is returned (e.g. without any manipulation).
#' Default is \code{FALSE}. Can be set globally for the current R session
#' via the "raw_data" option ( \code{options(raw_data = TRUE)}). If \code{TRUE}
#' \code{all_data} is set to \code{TRUE} internally. Useful for developers,
#' or if users suspect that some data is mishandled during processing
#' (i.e. date information is lost). Note that the metadata obtained
#' when \code{raw_data = TRUE} is not standardized, so most suwo functions
#' for downstream steps will not work on them.
#' @param metadata data frame previously obtained from any suwo query
#' function (i.e. `query_reponame()`).
#' @name template_params
#' @keywords internal
#' @title template_params
NULL
