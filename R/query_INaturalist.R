query_Macaulay <-
  function(term = NULL,
           file.name = "sciName",
           type = "a",
           cores = 1,
           pb = TRUE,
           verbose = TRUE) {

    # type must be supplied
    if (is.null(type))
      stop("'type' must be supplied")

    # type must be supplied
    if (is.null(term))
      stop("'term' must be supplied")

    #check internet connection
    a <- try(RCurl::getURL("https://www.inaturalist.org/"), silent = TRUE)
    if (is(a, "try-error"))
      stop("No connection to INaturalist (check your internet connection!)")

    if (a == "Could not connect to the database")
      stop("INaturalist website website is apparently down")

    # If cores is not numeric
    if (!is.numeric(cores))
      stop("'cores' must be a numeric vector of length 1")
    if (any(!(cores %% 1 == 0), cores < 1))
      stop("'cores' should be a positive integer")

    #format JSON
    term <- gsub(" ", "%20", term)


    #Check file.name
    file.name <- gsub(" ", "", file.name)

    if (is.null(X) & !is.null(file.name))
    {

      if (any(!(file.name %in%
                c("catalogId", "age", "behaviors", "sex", "locationLine2", "location", "licenseType", "thumbnailUrl", "previewUrl", "largeUrl", "mediaUrl", "userId", "mediaType", "rating", "userDisplayName", "assetId", "sciName", "width", "height","speciesCode", "eBirdChecklistId", "valid","comments", "obsComments", "specimenUrl", "userProfileUrl", "locationLine1", "obsDttm", "collected", "eBirdChecklistUrl", "ratingCount", "recorder", "microphone", "accessories", "specimenIds", "homeArchive", "stimulus", "source","commonName" )))) stop("File name tags don't match column names in the output of this function (see documentation)")
    }

    if (is.null(X))
    {

      if (media.type == "a") {
        fls <- "recording(s)"
        fl.xtn <- ".mp3"} else
          if (media.type == "p") {
            fls <- "photo(s)"
            fl.xtn <- ".jpg"} else
              if (media.type == "v") {
                fls <- "video(s)"
                fl.xtn <- ".mp4"}



      term = "Hirundo rustica"
      #format JSON
      term <- gsub(" ", "%20", term)

      base.srch.pth <- paste0("https://api.inaturalist.org/v1/Search?", "q=", term)

      #initialize search
      q <- rjson::fromJSON(file = paste0(base.srch.pth, term))

      count <- q$results$count

      if (q$results$count == 0) {cat(paste("No", fls, "were found"))
        download <- FALSE
      }else {

        # no more than 100 at the time currently
        if (q$results$count > 100) q$results$count <- 100

        if (q$results$count > 30)   q <- rjson::fromJSON(file = paste0(base.srch.pth, qword, "&count=", q$results$count))

        # no more than 100 at the time currently
        if (q$results$count > 100) q$results$count <- 100

        ### loop over pages
        # set clusters for windows OS
        if (Sys.info()[1] == "Windows" & parallel > 1)
          cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel

          f <- pbapply::pblapply(X = 1:q$results$count, cl = cl, FUN = function(y)
          {
            itm <- q$results$content[[y]]
            itm <- itm[which(names(itm) != "subjectData")]

            itm <- lapply(itm, function(x) if(!is.atomic(x) | is.null(x)) return(NA) else
              return(x))

            itm <- as.data.frame(itm)

            return(itm)
          }
          )




  }
