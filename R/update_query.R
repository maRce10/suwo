#' Access 'observation' recordings and metadata
#'
#' \code{update_query} detects duplicate data in data frames and updates new query results.
#' @param dataframe_1 dataframe refering to the metadata containing the multimedia information obtained from query functions.
#' @param all_data Logical argument that determines if all data available from database is shown in the results of search. Default is \code{TRUE}.
#' @return If all_data is not provided the function returns a data frame with the following media
#' information: id, scientific_name, name, group, group_name, status, rarity, photo,
#' info_text, permalink, determination_requirements, file_url, repository
#' @export
#' @name update_query
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
  function(previous_query = NULL, token = NULL) {

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

    # dataframe 1 must be supplied
    if (is.null(dataframe_1)) {
      .stop("'dataframe_1' must be supplied")
    }


    if (dataframe_1$repository[1] == "GBIF" ) {
      id_col_df1 <- as.numeric(gsub("\\D", "", dataframe_1[["catalogNumber"]]))
      id_col_df2 <- dataframe_2[["key"]]
    }

    #Set query term and type for new query search
    query_term <- attr(dataframe_1, "query_term")
    query_type <- attr(dataframe_1, "query_type")
    query_all_data <- attr(dataframe_1, "query_add_data")

    if (dataframe_1$repository[1] == "GBIF") {
      query_output_new <- query_gbif(term = query_term, type = query_type, all_data = TRUE)

    }
    if (dataframe_1$repository[1] == "INAT") {
      query_output_new <- query_inaturalist(term = query_term, type = query_type, all_data = TRUE)

    }
    if (dataframe_1$repository[1] == "Macaulay"){
      query_output_new <- query_macaulay(term = query_term, type = query_type, all_data = TRUE)

    }
    if (dataframe_1$repository[1] == "Observation"){
      # Check if token is available
      if (is.null(token)) {
        .stop("Invalid token for observation.org")
      }
      query_output_new <- query_observation(term = query_term, type = query_type, all_data = TRUE, token = Token)

    }
    if (dataframe_1$repository[1] == "XC"){
      query_output_new <- query_xenocanto(term = query_term)

    }
    if (dataframe_1$repository[1] == "wikiaves"){
      query_output_new <- query_wikiaves(term = query_term, type = query_type, all_data = TRUE)

    }

      # Find duplicates
    query_output_df <- detect_duplicates(dataframe_1, query_output_new, query_duplicate = FALSE)

    # Add a timestamp attribute
    search_time <- Sys.time()
    attr(query_output_df, "search_time") <- search_time

    return(query_output_df)
  }
