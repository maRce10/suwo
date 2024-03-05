#' Access 'observation' recordings and metadata
#'
#' \code{detect_duplicates} detects duplicate data in data frames.
#' @usage detect_duplicates(dataframe_1 = NULL, dataframe_2 = NULL, all_data)
#' @param dataframe dataframe refering to the metadata containing the multimedia information obtained from query     functions.
#' @param all_data Logical argument that determines if all data available from database is shown in the results of search. Default is \code{TRUE}.
#' @return If all_data is not provided the function returns a data frame with the following media
#' information: id, scientific_name, name, group, group_name, status, rarity, photo,
#' info_text, permalink, determination_requirements, file_url, repository
#' @export
#' @name detect_duplicates
#' @details This function compares two dataframes to detect repeated data.
#' @examples
#' \dontrun{
#' # compare
# df3 <- detect_duplicates(dataframe_1 = df1, dataframe_2 = df2)
#' View(df3)
#' }
#'
#' @references {
#'
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
detect_duplicates <-
  function(dataframe_1 = NULL,
           dataframe_2 = NULL,
           all_data = TRUE) {
    # check arguments
    arguments <- as.list(base::match.call())[-1]

    # add objects to argument names
    for (i in names(arguments)) {
      arguments[[i]] <- get(i)
    }

    # check each arguments
    check_results <- check_arguments(args = arguments)

    # report errors
    checkmate::reportAssertions(check_results)

    # dataframe 1 must be supplied
    if (is.null(dataframe_1)) {
      stop2("'dataframe_1' must be supplied")
    }

    # dataframe 2 must be supplied
    if (is.null(dataframe_2)) {
      stop2("'dataframe_2' must be supplied")
    }

    if (dataframe_1$repository[1] == "GBIF" ) {
      id_col_df1 <- as.numeric(gsub("\\D", "", dataframe_1[["catalogNumber"]]))
      id_col_df2 <- dataframe_2[["key"]]

      # Find duplicates
      duplicates <- id_col_df1[id_col_df1 %in% id_col_df2]
    }

    print(duplicates)

    user_input <- readline("Would you like to remove duplicates from merged metadata dataframe? (y/n)  ")
    if(user_input != 'y') {
      #Remove duplicates from merged dataframe
      }


    # id_col_df1 <- df1[[id_column]]
    # id_col_df2 <- df2[[id_column]]
    #
    # # Find duplicates
    # duplicates <- id_col_df1[id_col_df1 %in% id_col_df2]

    return(duplicates)
  }
