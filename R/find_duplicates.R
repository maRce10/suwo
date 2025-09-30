#' Find duplicated entries in metadata
#'
#' \code{find_duplicates} detect possible duplicated entries from merged metadata from several repositories.
#' @param metadata data frame obtained from combining the output metadata of two or more suwo query function using the `merge_metadata()` function. Data frames obtained from a single suwo query function can also be used but duplicates are not really expected within the same repository. The data frame must have the following columns: `user_name`, `locality`, `repository`, `country`, `format`, `time`, and `date`.
#' @param sort Logical argument indicating if the output data frame should be sorted by the `duplicate_group` column. This will group all potential duplicates together in the output data frame. Default is `TRUE`.
#' @return A single data frame with the data from all input data frames combined and with an additional column named `duplicate_group` indicating potential duplicates with a common index. Entries without potential duplicates are labeled as `NA` in this new column.
#' @export
#' @name find_duplicates
#' @details This function compares the information in the entries of a combined metadata data frame and labels those possible duplicates with a common index in a new column named `duplicate_group`. The comparison is based on the similarity of the following fields: `user_name`, `locality`, `time` and `country`. Only rows with no missing data for those fields will be considered. The function uses the `RecordLinkage` package to perform the a fuzzy matching comparison and identify potential duplicates based on predefined similarity thresholds. The function only spots duplicates from different repositories and assumes those duplicates should have the same `format` and `date`. This function is useful for curating the data obtained by merging data from multiple sources, as the same observation might be recorded in different repositories. This is a common issue in citizen science repositories, where users might upload the same observation to different platforms. This can also occur as some repositories automatically share data with other repositories, particularly with GBIF.
#' @examples
#' \dontrun{
#' # get metadata from 2 repos
#' gb <- query_gbif(term = "Turdus rufiventris", format =  "sound")
#' xc <- query_xenocanto(term = "Turdus rufiventris")
#'
#' # combine metadata
#' merged_metadata <- merge_metadata(xc, gb)
#'
#' # find duplicates
#' label_dup_metadata <- find_duplicates(metadata = merged_metadata)
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
find_duplicates <- function(metadata, sort = TRUE) {
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

  if (nrow(metadata) < 2) {
    .stop("The input data frame should have at least two rows to compare.")
  }


  # index to order back
  metadata$..original_order <- seq_len(nrow(metadata))

  # keep only those complete cases for used columns
  non_complete_metadata <- metadata[!stats::complete.cases(metadata[, c("user_name", "locality", "country", "format", "time", "date", "format")]), ]
  metadata <- metadata[stats::complete.cases(metadata[, c("user_name", "locality", "country", "format", "time", "date", "format")]), ]

  # block to force comparison only within same format and date
  # block <- as.numeric(as.factor(paste(metadata$format, metadata$date)))

  # spot duplicates
  similarities <-
    RecordLinkage::compare.dedup(metadata[, c("user_name", "locality", "country", "time", "date", "format")],
                                 # identity = block,
                                 strcmp = TRUE)$pairs

  # remove last column (is_match)
  similarities <- similarities[, -ncol(similarities)]

  # # get repo of each id
  similarities$repo1 <- metadata$repository[similarities$id1]
  similarities$repo2 <- metadata$repository[similarities$id2]


  # theshold to consider a match
  criteria <-
    similarities$format == 1 &
    similarities$country > 0.8 & similarities$locality > 0.5 & similarities$user_name > 0.8 & similarities$time == 1 & similarities$date == 1  & stats::complete.cases(similarities)

  # get duplicates
  possible_duplicates <- similarities[criteria, ]

  # create list with NAS and length nrow(metadata)
  matching_list <- as.list(seq_len(nrow(metadata)))
  repo_list <- as.list(metadata$repository)

  ## create labels to group duplicates
  ## also keep track if their belong to the same repository
  for (i in seq_len(nrow(possible_duplicates))) {
    matching_list[[possible_duplicates$id1[i]]] <- sort(unique(c(matching_list[[possible_duplicates$id1[i]]], possible_duplicates$id2[i])))
    repo_list[[possible_duplicates$id1[i]]] <- sort(unique(c(repo_list[[possible_duplicates$id1[i]]], possible_duplicates$repo2[i])))

    matching_list[[possible_duplicates$id2[i]]] <- sort(unique(c(matching_list[[possible_duplicates$id2[i]]], possible_duplicates$id1[i])))
    repo_list[[possible_duplicates$id2[i]]] <- sort(unique(c(repo_list[[possible_duplicates$id2[i]]], possible_duplicates$repo1[i])))
  }

  # make does with only 1 value a NA
  matching_list <- sapply(matching_list, function(x) if(length(x) == 1) NA else paste(x, collapse = "-"))
  repo_count <- sapply(repo_list, length)

  # convert to unique values and add duplicate index to metadata
  metadata$duplicate_group <- as.numeric(as.factor(matching_list))
  non_complete_metadata$duplicate_group <- NA

  # sort by duplicate_group
  if (sort){
  metadata <- metadata[order(metadata$duplicate_group), ]
}

  metadata <- rbind(metadata, non_complete_metadata)

  # order back
  if (!sort){
  metadata <- metadata[order(metadata$..original_order), ]
  }

  # remove original order column
  metadata$..original_order <- NULL

  return(metadata)
  }
