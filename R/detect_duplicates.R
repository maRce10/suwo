#' Access 'observation' recordings and metadata
#'
#' \code{detect_duplicates} detects duplicate data in data frames.
#' @inheritParams template_params
#' @param X dataframe refering to the metadata containing the multimedia information obtained from query functions.
#' @param Y a second dataframe refering to the metadata containing the multimedia information obtained from query functions.
#' @param query_duplicate Logical argument that determines if user is asked to remove duplicates. Default is \code{TRUE}.
#' @return If all_data is not provided the function returns a data frame with the following media
#' information: id, scientific_name, name, group, group_name, status, rarity, photo,
#' info_text, permalink, determination_requirements, file_url, repository
#' @export
#' @name detect_duplicates
#' @details This function compares two dataframes to detect repeated data.
#' @examples
#' \dontrun{
#' # compare
# df3 <- detect_duplicates(X = df1, Y = df2)
#' View(df3)
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
detect_duplicates <-
  function(X,
           Y,
          all_data = getOption("all_data", TRUE),
           query_duplicate = TRUE) {
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

    if (X$repository[1] == "GBIF" ) {
      id_col_df1 <- as.numeric(gsub("\\D", "", X[["catalogNumber"]]))
      id_col_df2 <- Y[["key"]]
    } else {
      id_col_df1 <- X[["key"]]
      id_col_df2 <- Y[["key"]]
    }
      # Find duplicates
      duplicates <- id_col_df1[id_col_df1 %in% id_col_df2]

    print(duplicates)
    if (query_duplicate == TRUE) {
     user_input <- readline("Would you like to remove duplicates from merged metadata dataframe? (y/n)  ")
      if (user_input == 'y') {
        # Remove duplicates from X
        X <- X[!id_col_df1 %in% duplicates, ]
      } else {
       return(invisible(NULL))
      }
    }

    # Merge the dataframes
    merged_dataframe <- rbind(X, Y)

    return(merged_dataframe)

    }

