# load default options on package load
.onLoad <- function(libname, pkgname) {
  # Store original options to restore later
  op <- options()

  # Package-specific default options
  op.yourpackage <- list(
    mc.cores = 1,
    pb = TRUE,
    verbose = TRUE,
    all_data = FALSE,
    raw_data = FALSE
  )

  # Only set options that haven't been set by user
  toset <- !(names(op.yourpackage) %in% names(op))
  if(any(toset)) {
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
  package_options <- c("mc.cores", "pb", "verbose", "all_data", "raw_data")

  for(opt in package_options) {
    if(opt %in% names(original_op)) {
      # Restore original value
      do.call(options, setNames(list(original_op[[opt]]), opt))
    } else {
      # Option didn't exist before, so remove it
      do.call(options, setNames(list(NULL), opt))
    }
  }

  # Clean up
  if(exists(".original_options", envir = parent.env(environment()))) {
    try(rm(".original_options", envir = parent.env(environment())), silent = TRUE)
  }

  invisible()
}


# message when loading package
.onAttach <-
  function(libname, pkgname) {
    packageStartupMessage("\nPlease cite 'suwo' as: \n")
    packageStartupMessage(
      "Araya-Salas, M., & J. Elizondo-Calvo. 2023. suwo: access nature media
      repositories through R. R package version 0.1.0."
    )
  }
