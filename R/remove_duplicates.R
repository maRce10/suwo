#' Merge metadata data frames
#'
#' \code{remove_duplicates} merges metadata data frames from suwo queries.
#' @inheritParams template_params
#' @param metadata data frame obtained from possible duplicates  with the
#' function \code{\link{find_duplicates}}. The data frame must have the column
#' 'duplicate_group' returned by \code{\link{find_duplicates}}.
#' @param same_repo Logical argument indicating if observations labeled
#' as duplicates that belong to the same repository should be removed. Default
#' is \code{FALSE}. If \code{TRUE}, only one of the duplicated observations
#' from the same repository will be retained in the output data frame. This is
#' useful as it can be expected that observations from the same repository are
#' not true duplicates (e.g. different recordings uploaded to Xeno-Canto with
#' the same date, time and location by the same user), but rather have not been
#' documented with enough precision to be told apart.
#' @param repo_priority Character vector indicating the priority of
#' repositories when selecting which observation to retain when duplicates
#' are found. Default is \code{c("Xeno-Canto", "GBIF", "iNaturalist",
#' "Macaulay Library", "Wikiaves", "Observation")}, which gives priority to
#' repositories in which media downloading is more
#' straightforward (Xeno-Canto and GBIF).
#' @return A single data frame with a subset of the 'metadata' with those
#' observations that were determined not to be duplicates.
#' @export
#' @name remove_duplicates
#' @details This function removes duplicate observations identified with the
#' function \code{\link{find_duplicates}}. When duplicates are found, one
#' observation from each group of duplicates is retained in the output data
#' frame. However, if multiple observations from the same repository are
#' labeled as duplicates, by default (\code{same_repo = FALSE}) all of them
#' are retained in the output data frame. This is useful as it can be
#' expected that observations from the same repository are not true
#' duplicates (e.g. different recordings uploaded to Xeno-Canto with
#' the same date, time and location by the same user), but rather have not
#' been documented with enough precision to be told apart. This behavior can
#' be modified. If \code{same_repo = TRUE}, only one of the duplicated
#' observations from the same repository will be retained in the output data
#' frame. The function will give priority to repositories in which media
#' downloading is more straightforward (Xeno-Canto and GBIF), but this can be
#' modified with the argument 'repo_priority'.
#' @examples
#' \dontrun{
#' # get metadata from 2 repos
#' gb <- query_gbif(species = "Turdus rufiventris", format =  "sound")
#' xc <- query_xenocanto(species = "Turdus rufiventris")
#'
#' # combine metadata
#' merged_metadata <- merge_metadata(xc, gb)
#'
#' # find duplicates
#' label_dup_metadata <- find_duplicates(metadata = merged_metadata)
#'
#' # remove duplicates
#' dedup_metadata <- remove_duplicates(label_dup_metadata)
#' }
#'
#' @seealso \code{\link{find_duplicates}}, \code{\link{merge_metadata}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
remove_duplicates <-
  function(metadata, same_repo = FALSE, cores = getOption("mc.cores", 1), pb = getOption("pb", TRUE), repo_priority = c("Xeno-Canto", "GBIF", "iNaturalist", "Macaulay Library", "Wikiaves", "Observation")) {

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

    # get data with no duplicates
    no_dups <- metadata[is.na(metadata$duplicate_group), ]

    # get data with duplicates
    dups <- metadata[!is.na(metadata$duplicate_group), ]

    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & cores > 1) {
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", cores))
    } else {
      cl <- cores
    }

    # loop over unique duplicate groups
    dedups_list <- pblapply_sw_int(unique(dups$duplicate_group), cl = cl, pbar = pb,  function(x){

      # subset of duplicates
      sub_dups <- dups[dups$duplicate_group == x, ]

      # order according to priority
      sub_dups$.repository_factor <- factor(sub_dups$repository, levels = repo_priority)
      sub_dups <- sub_dups[order(as.numeric(sub_dups$.repository_factor)), ]

      # count possible duplicates by repository
      repo_counts <- table(sub_dups$repository)

      # get repo with more observations
      if (!same_repo){
        # if same_repo is TRUE, keep only one observation per repository
        sub_dups <- sub_dups[!duplicated(sub_dups$repository), ]
      } else {

      if (length(unique(repo_counts)) == 1){

        # select which repo to use
        top_repo <- repo_priority[which.min(which(repo_priority %in% names(repo_counts)))]

        # select XC if available, if not GBIF if not, any
        if ("Xeno-Canto" %in% names(repo_counts)){
          top_repo <- "Xeno-Canto"
        } else if ("GBIF" %in% names(repo_counts)){
          top_repo <- "GBIF"
        } else {
          top_repo <- names(repo_counts)[1]
        }

        } else {
          top_repo <- names(repo_counts)[which.max(repo_counts)]
        }

        sub_dups <- sub_dups[sub_dups$repository == top_repo, ]
      }

      sub_dups$.repository_factor <- NULL

     return(sub_dups)
    })

    # get all deduplicated
    dedups <- do.call(rbind, dedups_list)

    # combine with no duplicates
    dedup_metadata <- rbind(dedups, no_dups)

    return(dedup_metadata)
      }
