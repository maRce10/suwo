# internal helper suwo functions

# internal suwo function, not to be called by users. It is a modified version of pbapply::pblapply
# that allows to define internally if progress bar would be used (pbapply::pblapply uses pboptions to do this)

# Create internal funciton to update datasets from gbif, launch once before every session

pblapply_sw_int <- function(X,
                            FUN,
                            cl = 1,
                            pbar = TRUE,
                            ...) {
  # conver parallel 1 to null
  if (!inherits(cl, "cluster")) {
    if (cl == 1)
      cl <- NULL
  }

  FUN <- match.fun(FUN)
  if (!is.vector(X) || is.object(X)) {
    X <- as.list(X)
  }
  if (!length(X)) {
    return(lapply(X, FUN, ...))
  }
  if (!is.null(cl)) {
    if (.Platform$OS.type == "windows") {
      if (!inherits(cl, "cluster")) {
        cl <- NULL
      }
    } else {
      if (inherits(cl, "cluster")) {
        if (length(cl) < 2L) {
          cl <- NULL
        }
      } else {
        if (cl < 2) {
          cl <- NULL
        }
      }
    }
  }

  if (is.null(cl)) {
    if (!pbar) {
      return(lapply(X, FUN, ...))
    }
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
      if (pbar) {
        return(PAR_FUN(cl, X, FUN, ...))
      }
      Split <- pbapply::splitpb(length(X), length(cl), nout = 100)
      B <- length(Split)
      pb <- pbapply::startpb(0, B)
      on.exit(pbapply::closepb(pb), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(PAR_FUN(cl, X[Split[[i]]], FUN, ...))
        pbapply::setpb(pb, i)
      }
    } else {
      if (!pbar) {
        return(parallel::mclapply(X, FUN, ..., mc.cores = as.integer(cl)))
      }
      Split <- pbapply::splitpb(length(X), as.integer(cl), nout = 100)
      B <- length(Split)
      pb <- pbapply::startpb(0, B)
      on.exit(pbapply::closepb(pb), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(parallel::mclapply(X[Split[[i]]], FUN, ..., mc.cores = as.integer(cl)))
        pbapply::setpb(pb, i)
      }
    }
  }
  rval <- do.call(c, rval, quote = TRUE)
  names(rval) <- names(X)
  rval
}

# stop function that doesn't print call
.stop <- function(...) {
  stop(..., call. = FALSE)
}


# warning function that doesn't print call
.warning <- function(x, color = "magenta") {
  warning(.colortext_2(x, as = color), call. = FALSE)
}

# message function that changes colors
.message <- function(x, color = "black") {
  message(.colortext_2(x, as = color))
}

# coloring text
.colortext_2 <-
  function(text,
           as = c("red",
                  "blue",
                  "green",
                  "magenta",
                  "cyan",
                  "orange",
                  "black",
                  "silver")) {
    if (.has_color()) {
      unclass(cli::make_ansi_style(.suwo_message_style(as))(text))
    } else {
      text
    }
  }

.has_color <- function() {
  cli::num_ansi_colors() > 1
}

.suwo_message_style <-
  function(color = c("red",
                     "blue",
                     "green",
                     "magenta",
                     "cyan",
                     "orange",
                     "black",
                     "silver")) {
    color <- match.arg(color)

    c(
      red = "red",
      blue = "blue",
      green = "green",
      magenta = "magenta",
      cyan = "cyan",
      orange = "orange",
      black = "black",
      silver = "silver"
    )[[color]]
  }

# colored message
.colortext <- function(text,
                       as = c("red",
                              "blue",
                              "green",
                              "magenta",
                              "cyan",
                              "orange",
                              "black",
                              "silver")) {
  if (.has_color()) {
    unclass(cli::make_ansi_style(.suwo_style(as))(text))
  } else {
    text
  }
}

# add emojis to messages. based on praise_emoji from testthat
.add_emoji <- function(mood) {
  if (!cli::is_utf8_output()) {
    return("")
  }

  happy_emoji <- c(
    "\U0001f600",
    # smile
    "\U0001f973",
    # party face
    "\U0001f638",
    # cat grin
    "\U0001f308",
    # rainbow
    "\U0001f947",
    # gold medal
    "\U0001f389",
    # party popper
    "\U0001f38a" # confetti ball
  )

  sad_emoji <- c(
    "\U0001f62C",
    # grimacing face
    "\U0001f635",
    # face with spiral eyes
    "\U0001f62D",
    # loudly crying face
    "\U0001f613",
    #  	weary face
    "\U0001f480",
    # skull
    "\U0001F4A9",
    # pile of poop
    "\U0001f624",
    # face with steam from nose
    "\U0001f648" # see-no-evil monkey
  )

  if (mood == "happy") {
    sample(happy_emoji, 1)
  } else {
    sample(sad_emoji, 1)
  }
}

#####

.suwo_style <- function(type = c("success", "skip", "warning", "failure", "error")) {
  type <- match.arg(type)

  c(
    success = "green",
    skip = "blue",
    warning = "magenta",
    failure = "orange",
    error = "orange"
  )[[type]]
}

# Function to download file according to repository
.download <- function(metadata, x, path) {
  dl_result <- try(download.file(
    url = as.character(metadata$file_url[x]),
    destfile = file.path(path, metadata$download_file_name[x]),
    quiet = TRUE,
    mode = "wb",
    cacheOK = TRUE,
    extra = getOption("download.file.extra"),
    Sys.sleep(0.1)
  ),
  silent = TRUE)


  if (is(dl_result, "try-error")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# fix extension so it is homogenuous across functions
.fix_extension <- function(x) {
  # jpg to jpeg
  x <- gsub("jpg$", "jpeg", x, ignore.case = TRUE)

  # tif to tiff
  x <- gsub("tif$", "tiff", x, ignore.case = TRUE)

  # mpeg to mp3
  x <- gsub("mpeg$", "mp3", x, ignore.case = TRUE)

  # x-wav to wav
  x <- gsub("x-wav$", "wav", x, ignore.case = TRUE)

  # x-m4a to m4a
  x <- gsub("x-m4a$", "m4a", x, ignore.case = TRUE)

  # x-m4a to m4a
  x <- gsub("x-m4a$", "m4a", x, ignore.case = TRUE)

  # convert to lower case
  x <- tolower(x)

  # for Macaulay
  x <- ifelse(x == "audio", "mp3", x)
  x <- ifelse(x == "photo", "jpeg", x)
  x <- ifelse(x == "video", "mp4", x)

  # return
  return(x)
}


# format query output dataframe to standardize column names
.format_query_output <- function(X,
                                 column_names,
                                 all_data,
                                 format,
                                 call,
                                 input_file = NA) {
  # lower case
  names(X) <- tolower(names(X))
  names(column_names) <- tolower(names(column_names))

  names_df <- data.frame(old = names(column_names), new = column_names)

  for (i in seq_len(nrow(names_df))) {
    names(X)[names(X) == names_df$old[i]] <- names_df$new[i]
  }


  # replace "." with "_"
  names(X) <- gsub("\\.|\\-", "_", names(X))

  # replace "__" with "_"
  names(X) <- gsub("___", "_", names(X), fixed = TRUE)

  if (is.null(X$latitude))
    X$latitude <- NA

  if (is.null(X$longitude))
    X$longitude <- NA

  # convert lat long to numbers
  X$latitude <- as.numeric(X$latitude)
  X$longitude <- as.numeric(X$longitude)

  # add format
  X$format <- format

  # repo
  X$repository <- .repo_from_call(call)

  # format time column
  if (X$repository[1] == "Xeno-Canto") {
    X$time[X$time == "?"] <- NA
  }

  if (X$repository[1] == "Wikiaves") {
    X$time <- NA
  }

  if (X$repository[1] == "GBIF") {
    X$time <- substr(x = X$time,
                     start = 1,
                     stop = 5)
  }

  if (X$repository[1] == "iNaturalist") {
    X$time <- substr(x = X$time,
                     start = 12,
                     stop = 16)
  }

  if (X$repository[1] == "Macaulay Library") {
    X$time <- ifelse(nchar(X$time) == 3, paste0(0, X$time), X$time)

    X$time <- paste0(substr(X$time, 1, 2), ":", substr(X$time, 3, 4))
  }


  basic_colums <- c(
    "repository",
    "format",
    "key",
    "species",
    "date",
    "time",
    "country",
    "locality",
    "latitude",
    "longitude",
    "file_url",
    "file_extension"
  )

  # order so basic columns go first
  non_basic_colms <- setdiff(names(X), basic_colums)
  X <- X[, c(basic_colums, non_basic_colms)]

  if (!all_data) {
    # remove columns that are not basic
    X <- X[, basic_colums]
  }

  ## add attributes
  X <- .add_attributes(
    X = X,
    term = rlang::call_args(call)$term,
    format = format,
    all_data = all_data,
    call = call,
    input_file = input_file
  )

  # drop additional levels
  X <- droplevels(X)

  return(X)
}

# get repo name from call
.repo_from_call <- function(x) {
  switch(
    strsplit(
      x = as.character(x),
      split = "(",
      fixed = TRUE
    )[[1]][1],
    query_wikiaves = "Wikiaves",
    query_xenocanto = "Xeno-Canto",
    query_gbif = "GBIF",
    query_inaturalist = "iNaturalist",
    query_observation = "Observation",
    query_macaulay = "Macaulay Library"
  )

}


# add attributes to output data frames
.add_attributes <- function(X, term, format, all_data, input_file = NA, call) {
  term <- gsub("%20", " ", term)

  # Add a timestamp attribute
  search_time <- Sys.time()
  attr(X, "query_call") <- call
  attr(X, "repository") <- .repo_from_call(call)
  attr(X, "query_time") <- search_time
  attr(X, "query_term") <- term
  attr(X, "query_format") <- format
  attr(X, "query_all_data") <- all_data
  attr(X, "input_file(s)") <- input_file
  attr(X, "suwo_version") <- utils::packageVersion("suwo")
  return(X)
}

.color_text <- function(text,
                        as = c("success", "skip", "warning", "failure", "error")) {
  if (.has_color()) {
    unclass(cli::make_ansi_style(.suwo_style(as))(text))
  } else {
    text
  }
}

.has_color <- function() {
  cli::num_ansi_colors() > 1
}

.suwo_style <- function(type = c("success", "skip", "warning", "failure", "error")) {
  type <- match.arg(type)

  c(
    success = "green",
    skip = "blue",
    warning = "magenta",
    failure = "orange",
    error = "orange"
  )[[type]]
}

## function to split macaulay queries by year-month
.date_ranges <- function(x) {

  x <- sort(x)
  # current year as year.decimal
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  current_month <- as.numeric(format(Sys.Date(), "%m"))

  unique_years <- floor(min(x)):floor(max(x))

  # determine if has to be split by month
  if (!all(x == floor(x))) {
    # Calculate fraction of the year (0 = Jan, 1/12 ≈ 0.0833 per month)
    current_date <- current_year + (current_month) / 12

    # filter dates in the future (higher than current date)
    x <- x[x <= current_date]

    if (current_year %in% unique_years) {
      x <- c(x, current_date)
    }

    first_year <- min(floor(x))
    last_year <- max(floor(x)) + 1 # add one to include the end of the year

    # create df with all possible month year combinations
    poss_month_year_df <- expand.grid(month = 1:12, year = first_year:last_year)

    poss_month_year_df$year_decimal <-
      poss_month_year_df$year + (poss_month_year_df$month - 1) / 12


    date_list <- lapply(seq_along(x[-1]), function(y){
      time_diff <- poss_month_year_df$year_decimal - x[y]

      start <- poss_month_year_df[poss_month_year_df$year_decimal == poss_month_year_df$year_decimal[time_diff >= 0][1], ]
      time_diff <- poss_month_year_df$year_decimal - x[y + 1]
      end <- poss_month_year_df[poss_month_year_df$year_decimal == rev(poss_month_year_df$year_decimal[time_diff < 0])[1], ]

      out <- cbind(start[, 1:2], end[, 1:2])

      names(out) <- c("start_month", "start_year", "end_month", "end_year")
    return(out)
      }
    )

    dates_df <- do.call(rbind, date_list)

    # expand rows that cross years
    dates_list <- lapply(seq_len(nrow(dates_df)), function(i) {
      Y <- dates_df[i, ]
      if (Y$start_month > Y$end_month | Y$start_year < Y$end_year)
        Y <- data.frame(
          start_month = c(Y$start_month, 1),
          start_year = c(Y$start_year, Y$start_year + 1),
          end_month = c(12, Y$end_month),
          end_year = c(Y$start_year, Y$start_year + 1)
        )

      return(Y)
    })

    dates_df <- do.call(rbind, dates_list)

  } else {
    # remove years above current year
    unique_years <- unique_years[unique_years <= current_year]

    dates_df <- data.frame(start_month = 1, start_year = x[-length(x)], end_month = 12, end_year = c(x[-c(1, length(x))] - 1, x[length(x)]), stringsAsFactors = FALSE)
  }
  return(dates_df)
}


# monitor if a new file is added
.monitor_new_files <- function(path, interval = 1) {
  # Create initial snapshot
  prev_snap <- utils::fileSnapshot(
    path = path,
    full.names = FALSE,
    # Return only file names (not full paths)
    pattern = "\\.csv$",
    recursive = FALSE    # Don't check subfolders
  )

  while (TRUE) {
    # Take new snapshot
    current_snap <- utils::fileSnapshot(
      path = path,
      full.names = FALSE,
      pattern = "\\.csv$",
      recursive = FALSE
    )

    # Compare snapshots
    changes <- utils::changedFiles(prev_snap, current_snap)

    # Return only names of new files
    if (length(changes$added) > 0) {
      return(changes$added)  # Returns character vector of new file names
    }

    # Wait before checking again
    Sys.sleep(interval)

    # Update snapshot for next comparison
    prev_snap <- current_snap
  }
}

## function to check arguments
.check_arguments <- function(args) {
  # create object to store check results
  check_collection <- checkmate::makeAssertCollection()

  ### check arguments
  if (!is.null(args$term)) {
    checkmate::assert_multi_class(
      x = args$term,
      classes = c("character"),
      add = check_collection,
      .var.name = "term"
    )
  }

  if (!is.null(args$format)) {
    checkmate::assert_multi_class(
      x = args$format,
      classes = c("character"),
      add = check_collection,
      .var.name = "format"
    )
  }

  if (!is.null(args$cores)) {
    checkmate::assert_integerish(
      args$cores,
      add = check_collection,
      lower = 1,
      upper = parallel::detectCores(),
      .var.name = "cores"
    )
  }

  if (!is.null(args$pb)) {
    checkmate::assert_logical(
      x = args$pb,
      len = 1,
      add = check_collection,
      .var.name = "pb"
    )
  }

  if (!is.null(args$identified)) {
    checkmate::assert_logical(
      x = args$identified,
      len = 1,
      add = check_collection,
      .var.name = "identified"
    )
  }

  if (!is.null(args$verifiable)) {
    checkmate::assert_logical(
      x = args$verifiable,
      len = 1,
      add = check_collection,
      .var.name = "verifiable"
    )
  }

  if (!is.null(args$dataset)) {
    if (!is.null(args$dataset)) {
      checkmate::assert_multi_class(
        x = args$dataset,
        classes = c("character"),
        add = check_collection,
        .var.name = "dataset"
      )
    }
  }

  if (!is.null(args$all_data)) {
    checkmate::assert_logical(
      x = args$all_data,
      len = 1,
      add = check_collection,
      .var.name = "all_data"
    )
  }

  if (!is.null(args$token)) {
    checkmate::assert_multi_class(
      x = args$token,
      classes = c("character"),
      add = check_collection,
      .var.name = "token"
    )
  }

  if (!is.null(args$path)) {
    if (!is.null(args$path)) {
      checkmate::assert_directory(
        x = args$path,
        access = "r",
        add = check_collection,
        .var.name = "path"
      )
    }
  }

  if (!is.null(args$metadata)) {
    checkmate::assert_multi_class(
      x = args$metadata,
      classes = c("data.frame"),
      add = check_collection,
      .var.name = "metadata"
    )
  }

  return(check_collection)
}

# message when loading package
.onAttach <-
  function(libname, pkgname) {
    packageStartupMessage("\nPlease cite 'suwo' as: \n")
    packageStartupMessage(
      "Araya-Salas, M., & J. Elizondo-Calvo. 2023. suwo: access nature media repositories through R. R package version 0.1.0."
    )
  }

## check internet
# gracefully fail if internet resource is not available
.try_GET <- function(x, ...) {
  tryCatch(
    httr::GET(url = x, httr::timeout(10), agent = "suwo (https://github.com/maRce10/suwo)"),
    error = function(e)
      conditionMessage(e),
    warning = function(w)
      conditionMessage(w)
  )
}

.is_response <- function(x) {
  class(x) == "response"
}

.check_internet_resource <- function(url, skip.error = FALSE) {
  output <- "OK"
  # First check internet connection
  if (!curl::has_internet()) {
    .message(x = "No internet connection.", color = "cyan")
    output <- "not_OK"
  } else {
    # Then try for timeout problems
    resp <- .try_GET(url)
    if (!.is_response(resp)) {
      .message(x = resp, color = "cyan")
      output <- "not_OK"
    } else {
      if (httr::http_error(resp) & !skip.error) {
        httr::message_for_status(resp)
        output <- "not_OK"
      }
    }
  }

  return(output)
}
