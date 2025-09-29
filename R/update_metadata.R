#' Update metadata
#'
#' \code{update_metadata} update metadata from previous queries.
#' @inheritParams template_params
#' @param path Directory path where the .csv file will be saved. Only applicable for \code{\link{query_macaulay}} query results. By default it is saved into the current working directory (\code{"."}).
#' @export
#' @name update_metadata
#' @return returns a data frame similar to the input 'metadata' with new data appended.
#' @details This function updates the metadata from a previous query to add entries found in the source repository. All observations must belong to the same repository. The function adds the column `new_entry` which labels those entries that are new (i.e., not present in the input metadata). The input data frame must have been obtained from any of the query functions with the argument `raw_data = FALSE`. The function uses the same query term and format as in the original query. If no new entries are found, the function returns the original metadata and prints a message.
#' @examples
#' \dontrun{
#' # query metadata
#' wa <- query_wikiaves(term = 'Glaucis dohrnii', format =  "sound")
#'
#' # remove last 3 rows to test update_metadata
#' sub_wa <- wa[1:(nrow(wa)- 3), ]
#'
#' # update
#' up_wa <- update_metadata(metadata = sub_wa)
#'
#' # check number of rows is the same
#' nrow(up_wa) == nrow(wa)
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
update_metadata <-
  function(metadata, token = NULL, path = ".",
           cores = getOption("mc.cores", 1),
           pb = getOption("pb", TRUE),
           verbose = getOption("verbose", TRUE)) {
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


    if (length(unique(metadata$repository)) > 1){
      .stop(
        "All observations must belong to the same repository. ",
        "Please provide a single repository query result to update_metadata()."
      )
    }

    if (is.null(attr(metadata, "query_term"))){
      .stop("The input data frame does not have the required attributes. ",
            "Please provide a data frame obtained from any of the query_x() functions setting the argument `raw_data = FALSE`.")
    }

    #Set query term and format for new query search
    query_term <- attr(metadata, "query_term")
    query_format <- attr(metadata, "query_format")
    all_data <- attr(metadata, "all_data")

    if (metadata$repository[1] == "GBIF") {
      query_output_new <- query_gbif(term = query_term,
                                     format = query_format,
                                     all_data = all_data,
                                     cores = cores,
                                     verbose = verbose,
                                     pb = pb)

    }

    if (metadata$repository[1] == "iNaturalist") {
      query_output_new <- query_inaturalist(term = query_term,
                                            format = query_format,
                                            all_data = all_data,
                                            cores = cores,
                                            verbose = verbose,
                                            pb = pb)

    }
    if (metadata$repository[1] == "Macaulay Library") {
      query_output_new <- query_macaulay(
        term = query_term,
        format = query_format,
        all_data = all_data,
        path = path,
        dates = eval(rlang::call_args(attributes(metadata)$query_call)$dates),
        verbose = verbose
      )

    }
    if (metadata$repository[1] == "Observation") {
      query_output_new <- query_observation(term = query_term,
                                            format = query_format,
                                            token = token,
                                            cores = cores,
                                            verbose = verbose,
                                            pb = pb)
    }

    if (metadata$repository[1] == "Xeno-Canto") {
      query_output_new <- query_xenocanto(term = query_term,
                                          cores = cores,
                                          all_data = all_data,
                                          verbose = verbose,
                                          pb = pb)

    }
    if (metadata$repository[1] == "Wikiaves") {
      query_output_new <- query_wikiaves(term = query_term,
                                         format = query_format,
                                         all_data = all_data,
                                         cores = cores,
                                         verbose = verbose,
                                         pb = pb)
    }

    # Find duplicates
    query_output_new <- query_output_new[!query_output_new$key %in% metadata$key,]

    if (nrow(query_output_new) == 0){
      if (verbose) {
        cat(.color_text("No new entries found", "failure"), .add_emoji("sad"), "\n")
      }
      return(metadata)
    }


    query_output_df <- merge_metadata(metadata, query_output_new)

    # remove merge_metadata added column
    query_output_df$source <- NULL

    # tag new entries
    query_output_df$new_entry <- ifelse(query_output_df$key %in% metadata$key, FALSE, TRUE)

    sum_new <- sum(query_output_df$new_entry)

    if (verbose) {
      if (sum_new > 0){
        cat(.color_text(paste("\n", sum_new, "new entries found"
        ), "success"), .add_emoji("happy"), "\n")
      }
    }

    return(query_output_df)
  }
