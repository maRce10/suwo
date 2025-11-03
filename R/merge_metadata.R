#' Merge metadata data frames
#'
#' \code{merge_metadata} merges metadata data frames from suwo queries.
#' @param ... two or more data frames (each one as a separate entry) referring
#' to the metadata obtained from suwo query functions (`query_x()`).
#' @return A single data frame with the data from all input data frames
#' combined and with an additional column named `source` indicating the
#' original data frame from which each row originated.
#' @export
#' @name merge_metadata
#' @details This function combines metadata from multiple sources
#' (e.g. WikiAves and xeno-canto) into a single data frame for easier analysis
#' and comparison. Each input data frame must be obtained from one of the
#' suwo query functions (e.g., `query_wikiaves()`, `query_xenocanto()`, etc.)
#' with `raw_data = FALSE`.
#' @examples
#' if (interactive()){
#'  # get metadata from 2 repos
#'   wa <- query_wikiaves(species = 'Glaucis dohrnii', format =  "sound")
#'   xc <- query_xenocanto(species = 'Glaucis dohrnii')
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
      .stop(paste("All inputs must be data frames obtained",
                  "from suwo query functions (query_x())."))
      }
    }

    # get names of the input data frames
    call <- match.call()
    args <- as.list(call)[-1]  # Remove function name
    df_names <- vapply(args, function(x)
      as.character(deparse(x)), character(1))

    # add source
    for (i in seq_along(metadata_list)) {
      metadata_list[[i]]$source <- df_names[i]
    }

    # Merge the dataframes
    merged_data <- .merge_data_frames(metadata_list)

    return(merged_data)

  }
