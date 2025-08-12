#' Access 'observation' recordings and metadata
#'
#' \code{update_query} detects duplicate data in data frames and updates new query results.
#' @inheritParams template_params
#' @param X data frame previously obtained from any query function (i.e. `query_reponame()`).
#' @param path Directory path where the .csv file will be saved. Only applicable for \code{\link{query_macaulay}} query results. By default it is saved into the current working directory (\code{"."}).
#' @export
#' @name update_query
#' @return returns a data frame similar to the input 'X' with new data appended.
#' @details This function updates a previous query to add new information from the corresponding database of the original search
#' @examples
#' \dontrun{
#' # compare
# df3 <- update_query(X = df1)
#' View(df3)
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
update_query <-
  function(X, token = NULL, path = ".",
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


    if (length(unique(X$repository)) > 1)
      .stop(
        "All observations should belong to the same repository. ",
        "Please provide a single repository query result to update_query().",
        call = match.call()
      )

    #Set query term and format for new query search
    query_term <- attr(X, "query_term")
    query_format <- attr(X, "query_format")
    query_all_data <- attr(X, "query_all_data")

    if (X$repository[1] == "GBIF") {
      query_output_new <- query_gbif(term = query_term,
                                     format = query_format,
                                     all_data = query_all_data,
                                     cores = cores,
                                     verbose = verbose,
                                     pb = pb)

    }

    if (X$repository[1] == "iNaturalist") {
      query_output_new <- query_inaturalist(term = query_term,
                                            format = query_format,
                                            all_data = query_all_data,
                                            cores = cores,
                                            verbose = verbose,
                                            pb = pb)

    }
    if (X$repository[1] == "Macaulay Library") {
      query_output_new <- query_macaulay(
        term = query_term,
        format = query_format,
        all_data = query_all_data,
        path = path,
        dates = eval(rlang::call_args(attributes(X)$query_call)$dates),
        verbose = verbose
      )

    }
    if (X$repository[1] == "Observation") {
      query_output_new <- query_observation(term = query_term,
                                            format = query_format,
                                            token = token,
                                            cores = cores,
                                            verbose = verbose,
                                            pb = pb)
    }

    if (X$repository[1] == "Xeno-Canto") {
      query_output_new <- query_xenocanto(term = query_term,
                                          cores = cores,
                                          verbose = verbose,
                                          pb = pb)

    }
    if (X$repository[1] == "Wikiaves") {
      query_output_new <- query_wikiaves(term = query_term,
                                         format = query_format,
                                         all_data = query_all_data,
                                         cores = cores,
                                         verbose = verbose,
                                         pb = pb)
    }

    # Find duplicates
    query_output_df <- merge_query_results(X = X, Y = query_output_new)

    # tag new entries
    names(query_output_df)[names(query_output_df) == "source"] <- "new_entry"
    query_output_df$new_entry <- ifelse(query_output_df$key %in% X$key, FALSE, TRUE)

    sum_new <- sum(query_output_df$new_entry)

    if (verbose) {
      if (sum_new > 0)
      cat(.color_text(paste(sum_new, "new entries found"
      ), "success"), .add_emoji("happy"), "\n")

      if (sum_new == 0)
        cat(.color_text("No new entries found", "failure"), .add_emoji("sad"), "\n")
    }

    return(query_output_df)
  }
