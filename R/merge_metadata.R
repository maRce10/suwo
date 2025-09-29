#' Merge query results
#'
#' \code{merge_metadata} merges the output of suwo query results.
#' @param ... two or more data frames (each one a a separate entry) referring to the metadata obtained from suwo query functions (`query_x()`).
#' @return A single data frame with the data from all input data frames combined and with an additional column named `source` indicating the original data frame from which each row originated.
#' @export
#' @name merge_metadata
#' @details This function compares two dataframes to detect repeated data.
#' @examples
#' \dontrun{
#'  # get metadata from 2 repos
#'   wa <- query_wikiaves(term = 'Glaucis dohrnii', format =  "sound")
#'   xc <- query_xenocanto(term = 'Glaucis dohrnii')
#'
#'   # combine metadata
#'   merged_mt <- merge_metadata(wa, xc)
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
merge_metadata <-
  function(...) {
    metadata_list <- list(...)

    for (i in seq_along(metadata_list)) {
      if (!is.data.frame(metadata_list[[i]])) {
        .stop("All inputs must be data frames obtained from suwo query functions (query_x()).")
      }

      if (is.null(attr(metadata_list[[i]], "query_term"))) {
        .stop(
          "The input data frame '",
          deparse(substitute(metadata_list[[i]])),
          "' does not have the required attributes. ",
          "Please provide a data frame obtained from any of the query_x() functions and make sure `raw_data = FALSE`."
        )
      }
    }

    # get names of the input data frames
    call <- match.call()
    args <- as.list(call)[-1]  # Remove function name
    deparse_args <- sapply(args, deparse)
    df_names <- as.character(deparse_args)

    # add source
    for (i in seq_along(metadata_list)) {
      metadata_list[[i]]$source <- df_names[i]
    }

    # Merge the dataframes
    merged_data <- .merge_data_frames(metadata_list)

    return(merged_data)

  }
