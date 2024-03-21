#' Access 'taxon codes'
#'
#' \code{taxon_code_search} searches for metadata from \href{https://www.birds.cornell.edu/clementschecklist/introduction/updateindex/october-2023/download/}{clements}.
#' @usage taxon_code_search(species_name = NULL, taxon_code_df = NULL)
#' @param term species name vector of length one indicating the species, to query 'clements database. For example \emph{Phaethornis longirostris}.
#' @return Returns the taxon code for the given species name if match found in the clements taxon code data base
#' @export
#' @name taxon_code_search
#' @details This function queries for taxon code information from the clements taxon code data base \href{https://www.birds.cornell.edu/clementschecklist/introduction/updateindex/october-2023/download/}{clements}. It can return taxon code data. Clements data base must be downloaded and available in the work directory.
#' @examples
#' \dontrun{
#' # search
# taxon_code_turdus <- taxon_code_search(species_name = 'Turdus iliacus', taxon_code_df = clements_data)
#' }
#'
#' @references {
#'
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
#'

taxon_code_search <-
    function(species_name = NULL) {
  taxon_code_df <- read.csv("Clements-v2023-October-2023.csv")
  taxon_code <- taxon_code_df$species_code[taxon_code_df$scientific.name == species_name]
  if (length(taxon_code) > 0) {
    return(taxon_code[1])
  } else {
    return(NULL)
  }
}
