# load default options on package load
.onLoad <- function(libname, pkgname) {
  # Store original options to restore later
  op <- options()

  # Package-specific default options
  op.yourpackage <- list(
    suwo_cores = 1,
    suwo_pb = TRUE,
    suwo_verbose = TRUE,
    suwo_all_data = FALSE,
    suwo_raw_data = FALSE
  )

  # Only set options that haven't been set by user
  toset <- !(names(op.yourpackage) %in% names(op))
  if (any(toset)) {
    options(op.yourpackage[toset])
  }

  # Store original state for restoration
  assign(".original_options", op, envir = parent.env(environment()))

  invisible()
}

.onUnload <- function(libpath) {
  # Get original options
  original_op <- get(".original_options", envir = parent.env(environment()))

  # Restore only the options that were modified by our package
  package_options <- c("suwo_cores", "suwo_pb", "suwo_verbose", "suwo_all_data", "suwo_raw_data")

  for (opt in package_options) {
    if (opt %in% names(original_op)) {
      # Restore original value
      do.call(options, stats::setNames(list(original_op[[opt]]), opt))
    } else {
      # Option didn't exist before, so remove it
      do.call(options, stats::setNames(list(NULL), opt))
    }
  }

  # Clean up
  if (exists(".original_options", envir = parent.env(environment()))) {
    try(
      rm(".original_options", envir = parent.env(environment())),
      silent = TRUE
    )
  }

  invisible()
}


# message when loading package
.onAttach <- function(libname, pkgname) {
  meta <- utils::packageDescription(pkgname)

  year <- if ("Date" %in% names(meta)) {
    sub("-.*", "", meta$Date)
  } else {
    format(Sys.Date(), "%Y")
  }

  msg <- sprintf(
    "\nPlease cite '%s' as:\n%s (%s). %s: %s. R package version %s.\n",
    meta$Package,
    meta$Author,
    year,
    meta$Package,
    meta$Title,
    meta$Version
  )

  packageStartupMessage(msg)
}
