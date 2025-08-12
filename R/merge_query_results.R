#' Detect duplicate observations in suwo query results
#'
#' \code{merge_query_results} detects duplicate data in suwo query results.
#' @inheritParams template_params
#' @param X dataframe referring to the metadata containing the multimedia information obtained from query functions.
#' @param Y a second dataframe referring to the metadata containing the multimedia information obtained from query functions.
#' @return If all_data is not provided the function returns a data frame with the following media
#' information: id, scientific_name, name, group, group_name, status, rarity, photo,
#' info_text, permalink, determination_requirements, file_url, repository
#' @export
#' @name merge_query_results
#' @details This function compares two dataframes to detect repeated data.
#' @examples
#' \dontrun{
#' # compare
# df3 <- merge_query_results(X = df1, Y = df2)
#' View(df3)
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
merge_query_results <-
  function(X,
           Y,
           all_data = getOption("all_data", FALSE)) {
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

    # print(duplicates)
    X$source <- "X"
    Y$source <- "Y"

    Y <- Y[!Y$key %in% X$key,]

    # Merge the dataframes
    merged_data <- rbind(X, Y)

    return(merged_data)

  }
