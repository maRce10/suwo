term = "Hirundo rustica"
#format JSON
term <- gsub(" ", "%20", term)

base.srch.pth <- paste0("https://api.inaturalist.org/v1/Search?", "q=", term)

#initialize search
q <- rjson::fromJSON(file = paste0(base.srch.pth, term))

count <- q$results$count
