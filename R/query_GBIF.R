term = "Sound"
#format JSON
term <- gsub(" ", "%20", term)

base.srch.pth <- paste0("https://api.gbif.org/v1/occurrence/search?&", "media_type=", term)

#initialize search
q <- rjson::fromJSON(file = paste0(base.srch.pth, term))

count <- q$results$count
