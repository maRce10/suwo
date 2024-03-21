#' Access 'taxon codes'
#'
#' \code{find_taxon_code} searches for metadata from \href{https://www.birds.cornell.edu/clementschecklist/introduction/updateindex/october-2023/download/}{clements}.
#' @usage find_taxon_code(species_name = NULL, taxon_code_df = NULL)
#' @param term species name vector of length one indicating the
#'  species, to query 'clements database. For example \emph{Phaethornis longirostris}.
#' @param taxon_code_df data frame with clements taxon code information.
#' @return Returns the taxon code for the given species name if match found in the clements taxon code data base
#' @export
#' @name find_taxon_code
#' @details This function queries for taxon code information from the clements taxon code data base \href{https://www.birds.cornell.edu/clementschecklist/introduction/updateindex/october-2023/download/}{clements}. It can return taxon code data.
#' @examples
#' \dontrun{
#' # search
# taxon_code_turdus <- find_taxon_code(species_name = 'Turdus iliacus', taxon_code_df = clements_data)
#' }
#'
#' @references {
#'
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
#'

find_taxon_code <-
    function(species_name = NULL,
             taxon_code_df = NULL) {
  taxon_code <- data_frame$species_code[data_frame$scientific.name == species_name]
  if (length(taxon_code) > 0) {
    return(taxon_code[1])
  } else {
    return(NULL)
  }
}
