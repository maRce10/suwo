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

# detect if string includes color
.has_color <- function() {
  cli::num_ansi_colors() > 1
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

# style text with color
.suwo_style <- function(type = c("success", "skip", "warning", "failure", "error", "message")) {
  type <- match.arg(type)

  c(
    success = "green",
    skip = "blue",
    warning = "magenta",
    failure = "orange",
    error = "red",
    message = "cyan"
  )[[type]]
}

.color_text <- function(text,
                        as = c("success", "skip", "warning", "failure", "error", "message"),
                        n = NULL) {
  if (!is.null(n))
    text <- cli::pluralize(text)

  if (.has_color()) {
    unclass(cli::make_ansi_style(.suwo_style(as))(text))
  } else {
    text
  }
}

# info messages
.message <- function(text, suffix = "\n") {
  cat(.color_text(
    text,
    as = "message"),
    suffix,
    sep = ""
  )
}

# message when files were found
.success_message <- function(text = paste0("Obtaining metadata ({n} matching ", format, " file{?s} found)"), format, n = NULL, suffix = ":\n") {
  cat(.color_text(
    text,
    as = "success",
    n = n),
    .add_emoji("happy"),
    suffix,
    sep = ""
  )
}


.failure_message <- function(text = paste0(text = "No ", format, " files were found"), format) {
  cat(.color_text(
    text,
    as = "failure"),
    .add_emoji("sad"),
    "\n",
    sep = ""
  )
}

# Function to download file according to repository
.download_basic <- function(metadata, x, path, overwrite) {

  # set destination file
  destfile <- file.path(normalizePath(path), metadata$download_file_name[x])

  exists <- file.exists(destfile)
    if (exists & !overwrite){
    (return("already there (not downloaded)"))
    }

  dl_result <- try(download.file(
    url = as.character(metadata$file_url[x]),
    destfile = destfile,
    quiet = TRUE,
    mode = "wb",
    method = "auto",
    cacheOK = TRUE,
    extra = getOption("download.file.extra")),
  silent = TRUE)

  # if failed try again after wating 0.5 seconds
  if (is(dl_result, "try-error")) {
    Sys.sleep(0.5)
    dl_result <- try(download.file(
      url = as.character(metadata$file_url[x]),
      destfile = destfile,
      quiet = TRUE,
      mode = "wb",
      method = "auto",
      cacheOK = TRUE,
      extra = getOption("download.file.extra")
    ),
    silent = TRUE)
    }

  # if still failed then return FALSE
  if (is(dl_result, "try-error")) {
    return("failed")
  } else {
    if (exists)
      (return("overwritten")) else
    return("saved")
  }
}

# suppressing warnings from download.file
.download <- function(...) suppressWarnings(.download_basic(...))

# fix extension so it is homogeneous across functions
.fix_extension <- function(x, url) {

  # if the strings contains "?" extract string before "?"
  if (any(grepl("\\?", x)))
    x <- gsub("\\?.*$", "", x)

  # if is NA then try to get from url
  if (any(is.na(x))) {
    url_ext <- tools::file_ext(url[is.na(x)])
    url_ext[url_ext %in% c("", ".", " ")] <- NA
    x[is.na(x)] <- url_ext
  }

  # jpg to jpeg
  x <- ifelse(grepl("jpg$|jpeg$", x, ignore.case = TRUE), "jpeg", x)

  # png
  x <- ifelse(grepl("png$", x, ignore.case = TRUE), "png", x)

  # gif
  x <- ifelse(grepl("gif$", x, ignore.case = TRUE), "gif", x)

  # tif to tiff
  x <- ifelse(grepl("tif$", x, ignore.case = TRUE), "tiff", x)

  # mpeg to mp3
  # x <- gsub("mpeg$|mpga$", "mp3", x, ignore.case = TRUE)
  x <- ifelse(grepl("mpeg$|mpga$|mpeg3$", x, ignore.case = TRUE), "mp3", x)

  # x-wav to wav
  # x <- gsub("x-wav$|vnd.wave$", "wav", x, ignore.case = TRUE)
  x <- ifelse(grepl("x-wav$|vnd.wave$", x, ignore.case = TRUE), "wav", x)

  # x-m4a to m4a
  # x <- gsub("x-m4a$|mp4$", "m4a", x, ignore.case = TRUE)
  x <- ifelse(grepl("x-m4a$|mp4$", x, ignore.case = TRUE), "m4a", x)

  # convert to lower case
  x <- tolower(x)

  # for Macaulay
  x <- ifelse(x == "audio", "mp3", x)
  x <- ifelse(x == "photo", "jpeg", x)
  x <- ifelse(x == "video", "mp4", x)

  # return
  return(x)
}

# rbind all data frames in a list after making them have the same columns
# X must be a list of data frames
.merge_data_frames <- function(X){
  # get common names to all data frames in X
  common_names <- unique(unlist(lapply(X, names)))

  # add missing columns to all data frames in X
  Y <- lapply(X, function(e) {
    nms <- names(e)
    if (length(nms) != length(common_names)) {
      for (o in common_names[!common_names %in% nms]) {
        e <-
          data.frame(e,
                     NA,
                     stringsAsFactors = FALSE,
                     check.names = FALSE)
        names(e)[ncol(e)] <- o
      }
    }
    return(e)
  })

  # all results in a single data frame
  Z <- do.call(rbind, Y)

  return(Z)
}

# format query output dataframe to standardize column names
.format_query_output <- function(X,
                                 column_names,
                                 all_data,
                                 format,
                                 raw_data = FALSE,
                                 call,
                                 input_file = NA,
                                 only_basic_columns = FALSE) {

  if (raw_data)
    return(X)

  basic_colums <- c(
    "repository",
    "format",
    "key",
    "species",
    "date",
    "time",
    "user_name",
    "country",
    "locality",
    "latitude",
    "longitude",
    "file_url",
    "file_extension"
  )

  if (only_basic_columns)
    return(basic_colums)

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

  # fix time
  ## for Macaulay first
  if (X$repository[1] == "Macaulay Library") {
    X$time <- ifelse(nchar(X$time) == 3, paste0(0, X$time), X$time)

    X$time <- paste0(substr(X$time, 1, 2), ":", substr(X$time, 3, 4))
  }

  # homogenize time
  X$time <- .convert_times(X$time)

  # homogenize date
  X$date <- .homogenize_dates(X$date)

  # fix extension
  X$file_extension <- .fix_extension(X$file_extension, url = X$file_url)

  # replace "" with NA
  X$country[X$country == ""] <- NA
  X$locality[X$locality == ""] <- NA

  # order so basic columns go first
  non_basic_colms <- setdiff(names(X), basic_colums)
  X <- X[, c(basic_colums, non_basic_colms)]

  # remove rows with NAs in URL
  if (anyNA(X$file_url)){

    option_df_name <- tolower(paste0(X$repository[1], "_excluded_results"))

    # save at options
    options(stats::setNames(list(X[is.na(X$file_url), ]), option_df_name))


    # let users know some observations were excluded
    cat(.color_text(paste0("{n} observation{?s} d{?oes/o} not have a download link and w{?as/ere} removed from the results (saved at `options('",option_df_name,"')`)."),
                    as = "warning",
                    n  = sum(is.na(X$file_url))
    )
    )

    # remove those observations
    X <- X[!is.na(X$file_url), ]
  }



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

# homogenize dates
.homogenize_dates <- function(date_strings) {
    vapply(date_strings, function(date_str) {
      # If input is NA, return NA
      if (is.na(date_str)) {
        return(NA_character_)
      }

      # Try to parse the date
      parsed <- lubridate::parse_date_time(date_str,
                                orders = c("dmy", "ymd", "ymd HMS", "ymd HM", "ymd"),
                                truncated = 2,
                                quiet = TRUE)

      # If parsing succeeded, convert to Date format
      if (!is.na(parsed)) {
        as.character(as.Date(parsed))
      } else {
        # If parsing failed but input is not NA, return the input value
        date_str
      }
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  }

# Function to check if a string is a valid time
.is_valid_time <- function(time_str) {
  if (is.na(time_str)) return(FALSE)

  # Remove spaces and convert to lowercase for easier matching
  clean_str <- tolower(trimws(time_str))

  # Check for invalid patterns first
  if (grepl("^\\?|xx|morning|^[a-z]+$", clean_str) && !grepl("am|pm", clean_str)) {
    return(FALSE)
  }

  # Check for various valid time patterns
  # HH:MM, H:MM, HH.MM, H.MM patterns with optional AM/PM
  if (grepl("^\\d{1,2}[:.]\\d{2}\\s*(am|pm)?$", clean_str) ||
      grepl("^\\d{1,2}\\s*(am|pm)$", clean_str)) {
    return(TRUE)
  }

  return(FALSE)
}

# Function to convert time strings to standardized format
.convert_times <- function(time_strings) {
  vapply(time_strings, function(time_str) {
    if (is.na(time_str) || !.is_valid_time(time_str)) {
      return(NA_character_)
    }

    # Clean the string
    clean_str <- tolower(trimws(time_str))

    # Handle simple am/pm without time (like "6am")
    if (grepl("^\\d{1,2}\\s*(am|pm)$", clean_str)) {
      time_num <- as.numeric(sub("\\s*(am|pm)$", "", clean_str))
      period <- ifelse(grepl("pm$", clean_str), "pm", "am")

      # Convert to 24-hour format
      if (period == "pm" && time_num < 12) {
        hours <- time_num + 12
      } else if (period == "am" && time_num == 12) {
        hours <- 0
      } else {
        hours <- time_num
      }

      return(sprintf("%02d:00", hours))
    }

    # Handle times with AM/PM
    if (grepl("am|pm", clean_str)) {
      time_part <- sub("\\s*(am|pm)$", "", clean_str)
      period <- ifelse(grepl("pm$", clean_str), "pm", "am")

      # Handle both colon and dot separators
      if (grepl("[:.]", time_part)) {
        parts <- strsplit(time_part, "[:.]")[[1]]
      } else {
        # If no separator, assume it's just hours
        parts <- c(time_part, "00")
      }

      hours <- as.numeric(parts[1])
      minutes <- ifelse(length(parts) > 1, as.numeric(parts[2]), 0)

      # Convert to 24-hour format
      if (period == "pm" && hours < 12) {
        hours <- hours + 12
      } else if (period == "am" && hours == 12) {
        hours <- 0
      }

      return(sprintf("%02d:%02d", hours, minutes))
    }

    # Handle 24-hour times without AM/PM
    if (grepl("[:.]", clean_str)) {
      parts <- strsplit(clean_str, "[:.]")[[1]]
    } else {
      # If no separator, assume it's just hours
      parts <- c(clean_str, "00")
    }

    hours <- as.numeric(parts[1])
    minutes <- ifelse(length(parts) > 1, as.numeric(parts[2]), 0)

    # Standardize to HH:MM format
    sprintf("%02d:%02d", hours, minutes)
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

# fix image size in INAT
.replace_image_size <- function(file_url) {
  gsub("square", "original", file_url)
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

.checkconnection <- function(service = c("gbif", "inat", "macaulay", "wikiaves", "xenocanto", "observation")) {

  # set user agent option globally
  options(HTTPUserAgent = "suwo (https://github.com/maRce10/suwo)")

  service <- match.arg(service)

  urls <- list(
    gbif     = "https://api.gbif.org/",
    inat     = "https://www.inaturalist.org/",
    macaulay = "https://www.macaulaylibrary.org/",
    wikiaves = "https://www.wikiaves.com.br",
    xenocanto = "https://www.xeno-canto.org",
    observation = "https://observation.org"
  )

  messages <- list(
    gbif     = "GBIF API",
    inat     = "INaturalist",
    macaulay = "macaulaylibrary.org",
    wikiaves = "wikiaves.com.br",
    xenocanto = "xeno-canto.org",
    observation = "https://observation.org"
  )

  url <- urls[[service]]
  name <- messages[[service]]

  # Attempt request
  response <- try(httr::GET(url,  httr::user_agent("suwo (https://github.com/maRce10/suwo)")), silent = TRUE)

  if (inherits(response, "try-error") || httr::http_error(response)) {
    .failure_message(paste("No connection to", name, "(check your internet connection!)"))
    return(FALSE)
    }

  content <- httr::content(response, as = "text", encoding = "UTF-8")
  if (grepl("Could not connect to the database", content)) {
    .failure_message(paste(name, "website is apparently down"))
    return(FALSE)
  }

  return(TRUE)
}

# look up species taxon code for Macaulay queries
.taxon_code_search <-
  function(term, ml_taxon_code = ml_taxon_code) {
    taxon_code <- ml_taxon_code$species_code[ml_taxon_code$scientific.name == term &
                                               !is.na(ml_taxon_code$scientific.name)]
    if (length(taxon_code) > 0) {
      return(taxon_code[1])
    } else {
      return(NULL)
    }
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
    rm(".original_options", envir = parent.env(environment()))
  }

  invisible()
}
