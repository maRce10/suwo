#internal helper suwo functions

# internal suwo function, not to be called by users. It is a modified version of pbapply::pblapply
# that allows to define internally if progress bar would be used (pbapply::pblapply uses pboptions to do this)

#Create internal funciton to update datasets from gbif, launch once before every session

pblapply_sw_int <- function(X, FUN, cl = 1, pbar = TRUE, ...) {

  # conver parallel 1 to null
  if (!inherits(cl, "cluster"))
    if (cl == 1) cl <- NULL

  FUN <- match.fun(FUN)
  if (!is.vector(X) || is.object(X))
    X <- as.list(X)
  if (!length(X))
    return(lapply(X, FUN, ...))
  if (!is.null(cl)) {
    if (.Platform$OS.type == "windows") {
      if (!inherits(cl, "cluster"))
        cl <- NULL
    } else {
      if (inherits(cl, "cluster")) {
        if (length(cl) < 2L)
          cl <- NULL
      } else {
        if (cl < 2)
          cl <- NULL
      }
    }
  }

  if (is.null(cl)) {
    if (!pbar)
      return(lapply(X, FUN, ...))
    Split <- pbapply::splitpb(length(X), 1L, nout = 100)
    B <- length(Split)
    pb <- pbapply::startpb(0, B)
    on.exit(pbapply::closepb(pb), add = TRUE)
    rval <- vector("list", B)
    for (i in seq_len(B)) {
      rval[i] <- list(lapply(X[Split[[i]]], FUN, ...))
      pbapply::setpb(pb, i)
    }
  } else {
    if (inherits(cl, "cluster")) {
      PAR_FUN <- parallel::parLapply
      if (pbar)
        return(PAR_FUN(cl, X, FUN, ...))
      Split <- pbapply::splitpb(length(X), length(cl), nout = 100)
      B <- length(Split)
      pb <- pbapply::startpb(0, B)
      on.exit(pbapply::closepb(pb), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(PAR_FUN(cl, X[Split[[i]]], FUN,
                                ...))
        pbapply::setpb(pb, i)
      }
    } else {
      if (!pbar)
        return(parallel::mclapply(X, FUN, ..., mc.cores = as.integer(cl)))
      Split <- pbapply::splitpb(length(X), as.integer(cl), nout = 100)
      B <- length(Split)
      pb <- pbapply::startpb(0, B)
      on.exit(pbapply::closepb(pb), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(parallel::mclapply(X[Split[[i]]],
                                           FUN, ..., mc.cores = as.integer(cl)))
        pbapply::setpb(pb, i)
      }
    }
  }
  rval <- do.call(c, rval, quote = TRUE)
  names(rval) <- names(X)
  rval
}

# stop function that doesn't print call
stop2 <- function (...)
{
  stop(..., call. = FALSE)
}

# warning function that doesn't print call
warning2 <- function (...)
{
  warning(..., call. = FALSE)
}

# add emojis to messages. based on praise_emoji from testthat

add_emoji <- function(mood) {
  if (!cli::is_utf8_output()) {
    return("")
  }

  happy_emoji <- c(
    "\U0001f600", # smile
    "\U0001f973", # party face
    "\U0001f638", # cat grin
    "\U0001f308", # rainbow
    "\U0001f947", # gold medal
    "\U0001f389", # party popper
    "\U0001f38a" # confetti ball
  )

  sad_emoji <- c(
    "\U0001f62C", # grimacing face
    "\U0001f635", # face with spiral eyes
    "\U0001f62D", # loudly crying face
    "\U0001f613", #  	weary face
    "\U0001f480", # skull
    "\U0001F4A9", # pile of poop
    "\U0001f624", # face with steam from nose
    "\U0001f648" # see-no-evil monkey
  )

  if (mood == "happy")
  sample(happy_emoji, 1) else
    sample(sad_emoji, 1)
}

#####


colortext <- function(text, as = c("success", "skip", "warning", "failure", "error")) {
  if (has_color()) {
    unclass(cli::make_ansi_style(suwo_style(as))(text))
  } else {
    text
  }
}

has_color <- function() {
    cli::num_ansi_colors() > 1
}

suwo_style <- function(type = c("success", "skip", "warning", "failure", "error")) {
  type <- match.arg(type)

  c(
    success = "green",
    skip = "blue",
    warning = "magenta",
    failure = "orange",
    error = "orange"
  )[[type]]
}

## function to check arguments
check_arguments <- function(args){

  # create object to store check results
  check_collection <- checkmate::makeAssertCollection()

  ### check arguments
  if (!is.null(args$term))
    checkmate::assert_multi_class(x = args$term, classes = c("character"), add = check_collection, .var.name = "term")

  if (!is.null(args$type))
    checkmate::assert_multi_class(x = args$type, classes = c("character"), add = check_collection, .var.name = "type")

  if (!is.null(args$cores))
    checkmate::assert_integerish(args$cores, add = check_collection, lower = 1, upper = parallel::detectCores(), .var.name = "cores")

  if (!is.null(args$pb))
    checkmate::assert_logical(x = args$pb, len = 1, add = check_collection, .var.name = "pb")

  if (!is.null(args$identified))
    checkmate::assert_logical(x = args$identified, len = 1, add = check_collection, .var.name = "identified")

  if (!is.null(args$verifiable))
    checkmate::assert_logical(x = args$verifiable, len = 1, add = check_collection, .var.name = "verifiable")

  if (!is.null(args$dataset))
    if (!is.null(args$dataset))
      checkmate::assert_multi_class(x = args$dataset, classes = c("integer"), add = check_collection, .var.name = "dataset")

  if (!is.null(args$all_data))
    checkmate::assert_logical(x = args$all_data, len = 1, add = check_collection, .var.name = "all_data")

  if (!is.null(args$token))
    checkmate::assert_multi_class(x = args$token, classes = c("character"), add = check_collection, .var.name = "token")

  if (!is.null(args$path))
    if (!is.null(args$path))
      checkmate::assert_directory(x = args$path, access = "r", add = check_collection, .var.name = "path")

  if (!is.null(args$metadata))
    checkmate::assert_multi_class(x = args$metadata, classes = c("data.frame"), add = check_collection, .var.name = "metadata")

  return(check_collection)
}
