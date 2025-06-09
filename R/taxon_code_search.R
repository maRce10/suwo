#' Access 'taxon codes'
#'
#' \code{taxon_code_search} searches for metadata from \href{https://www.birds.cornell.edu/clementschecklist/download/}{clements}.
#' @param term species name vector of length one indicating the species, to query 'clements database. For example \emph{Phaethornis longirostris}.
#' @return Returns the taxon code for the given species name if match found in the clements taxon code data base
#' @export
#' @name taxon_code_search
#' @details This function queries for taxon code information from the clements taxon code data base \href{https://www.birds.cornell.edu/clementschecklist/introduction/updateindex/october-2024/2024-citation-checklist-downloads/}{clements}. It can return taxon code data. Clements data base must be downloaded and available in the work directory.
#' @examples
#' \dontrun{
#' # search
# taxon_code_turdus <- taxon_code_search(term = 'Turdus iliacus')
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'@references {
#' Clements, J. F., P. C. Rasmussen, T. S. Schulenberg, M. J. Iliff, T. A. Fredericks, J. A. Gerbracht, D. Lepage, A. Spencer, S. M. Billerman, B. L. Sullivan, M. Smith, and C. L. Wood. 2024. The eBird/Clements checklist of Birds of the World: v2024.}
#'

taxon_code_search <-
  function(term) {
    taxon_code <- taxon_code_df$species_code[taxon_code_df$scientific.name == term &
                                               !is.na(taxon_code_df$scientific.name)]
    if (length(taxon_code) > 0) {
      return(taxon_code[1])
    } else {
      return(NULL)
    }
  }
