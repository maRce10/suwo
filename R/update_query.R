#' Access 'observation' recordings and metadata
#'
#' \code{update_query} detects duplicate data in data frames and updates new query results.
#' @inheritParams template_params
#' @param previous_query data frame referring to the metadata containing the multimedia information previously obtained from query functions.
#' @export
#' @name update_query
#' @return returns a data frame similar to the input 'previous_query' with new data appended.
#' @details This function updates a previous query to add new information from the corresponding database of the original search
#' @examples
#' \dontrun{
#' # compare
# df3 <- update_query(previous_query = df1)
#' View(df3)
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
update_query <-
  function(previous_query, token) {
    # check arguments
    arguments <- as.list(base::match.call())[-1]

    # add objects to argument names
    for (i in names(arguments)) {
      arguments[[i]] <- get(i)
    }

    # check each arguments
    check_results <- .check_arguments(args = arguments)

    # report errors
    checkmate::reportAssertions(check_results)


    if (previous_query$repository[1] == "GBIF") {
      id_col_df1 <- as.numeric(gsub("\\D", "", previous_query[["catalogNumber"]]))
      id_col_df2 <- previous_query$key
    }

    #Set query term and type for new query search
    query_term <- attr(previous_query, "query_term")
    query_type <- attr(previous_query, "query_type")
    query_all_data <- attr(previous_query, "query_all_data")

    if (previous_query$repository[1] == "GBIF") {
      query_output_new <- query_gbif(term = query_term,
                                     type = query_type,
                                     all_data = query_all_data)

    }
    if (previous_query$repository[1] == "INAT") {
      query_output_new <- query_inaturalist(term = query_term,
                                            type = query_type,
                                            all_data = query_all_data)

    }
    if (previous_query$repository[1] == "Macaulay") {
      query_output_new <- query_macaulay(term = query_term,
                                         type = query_type,
                                         all_data = query_all_data)

    }
    if (previous_query$repository[1] == "Observation") {
      query_output_new <- query_observation(term = query_term,
                                            type = query_type,
                                            token = token)
    }

    if (previous_query$repository[1] == "XC") {
      query_output_new <- query_xenocanto(term = query_term)

    }
    if (previous_query$repository[1] == "wikiaves") {
      query_output_new <- query_wikiaves(term = query_term,
                                         type = query_type,
                                         all_data = query_all_data)

    }

    # Find duplicates
    query_output_df <- detect_duplicates(previous_query, query_output_new, query_duplicate = FALSE)

    # Add a timestamp attribute
    search_time <- Sys.time()
    attr(query_output_df, "search_time") <- search_time

    return(query_output_df)
  }
