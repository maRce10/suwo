#' Subset data frames while preserving attributes
#'
#' \code{subset_metadata} subsets metadata data frames while preserving attributes.
#' @inheritParams template_params
#' @param condition An expression indicating the rows to keep or the indices of the rows to keep.
#' @export
#' @name subset_metadata
#' @return returns a data frame which is a subset of the input 'metadata'.
#' @details This function subsets a data frame while preserving its attributes. This is important for handling metadata data frames created by query functions in this package, which contain critical attributes that should be retained after subsetting.
#' @examples
#' \dontrun{
#' # Download metadata
#'  pant_xc <- query_xenocanto(term = "Phaethornis anthophilus")
#'
#' # Subset metadata to include only those from Panama
#' subset_metadata(pant_xc, country == "Panama")
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
subset_metadata <- function(metadata, condition) {
  # Capture the condition expression
  condition_expr <- substitute(condition)

  # Evaluate the condition in the context of the data frame
  condition_result <- eval(condition_expr, envir = metadata, enclos = parent.frame())

  # Perform the subset
  result <- metadata[condition_result, ]

  # Get attributes from original and result
  orig_attrs <- attributes(metadata)
  result_attrs <- attributes(result)

  # Only copy attributes that don't exist in the result
  missing_attrs <- setdiff(names(orig_attrs), names(result_attrs))
  for(attr_name in missing_attrs) {
    attr(result, attr_name) <- orig_attrs[[attr_name]]
  }

  # Also handle column-level attributes
  for(col_name in names(metadata)) {
    if(col_name %in% names(result)) {
      col_orig_attrs <- attributes(metadata[[col_name]])
      col_result_attrs <- attributes(result[[col_name]])

      if(!is.null(col_orig_attrs)) {
        for(col_attr_name in names(col_orig_attrs)) {
          if(!col_attr_name %in% names(col_result_attrs)) {
            attr(result[[col_name]], col_attr_name) <- col_orig_attrs[[col_attr_name]]
          }
        }
      }
    }
  }

  return(result)
}
