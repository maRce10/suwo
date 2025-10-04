#' Subset data frames while preserving attributes
#'
#' \code{subset_metadata} subsets metadata data frames while preserving attributes.
#' @inheritParams template_params
#' @param subset logical expression or numeric vector (with row numbers) indicating rows to keep.
#' @param select logical expression, numeric vector (with column numbers) or character vector (with column names) indicating columns to keep.
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
#' subset_metadata(pant_xc, subset = country == "Panama")
#'
#' # get first two rows
#' subset_metadata(pant_xc, subset = 1:2)
#'
#' # select only specific columns
#' subset_metadata(pant_xc, select = c("species", "date", "country", "file_url"))
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
subset_metadata <- function(metadata, subset, select) {

  # check if subset is missing
  if (missing(subset) && is.null(select)) {
    .stop("Either 'subset' or 'select' must be provided.")
  }


  # Get attributes from original
  orig_attrs <- attributes(metadata)

  if(!missing(subset)) {
  # Capture the condition expression
  subset_expr <- substitute(subset)

  # Evaluate the subset in the context of the data frame
  subset_metadata <- eval(subset_expr, envir = metadata, enclos = parent.frame())

  # Perform the subset
  metadata <- metadata[subset_metadata, ]
}
  # select by column
  if (!missing(select)) {
    select_expr <- substitute(select)
    select_metadata <- eval(select_expr, envir = metadata, enclos = parent.frame())
    metadata <- metadata[, select_metadata, drop = FALSE]
  }

  metadata_attrs <- attributes(metadata)

  # Only copy attributes that don't exist in the metadata
  missing_attrs <- setdiff(names(orig_attrs), names(metadata_attrs))
  for(attr_name in missing_attrs) {
    attr(metadata, attr_name) <- orig_attrs[[attr_name]]
  }

  return(metadata)
}
