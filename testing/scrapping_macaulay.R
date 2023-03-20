library(rvest)
library(httr)
library(stringr)

## get ids
url <- "https://search.macaulaylibrary.org/catalog?taxonCode=lobher&view=list&mediaType=photo"

url_parse(url)

# Send GET request to the URL
response <- GET(url)

# Extract content from response
content <- content(response, as = "text")

# Parse content as HTML
page <- read_html(content)

# Extract all <a> elements with the href attribute
links <- page %>% html_nodes("a[href]") %>% html_attr("href")


links <- grep("asset", links, value = T)
# Print the list of links
links

setwd(tempdir())

df <- data.frame(links = links, image_id = gsub("https://macaulaylibrary.org/asset/", "", links))
df$image_url <-
paste0("https://cdn.download.ams.birds.cornell.edu/api/v1/asset/", df$image_id, "/1200")

df$image_name <-
  paste0(df$image_id, ".jpeg")

pbapply::pblapply(1:nrow(df), cl = 6, function(x){
  download.file(df$image_url[x], df$image_name[x], quiet = TRUE)
  # print(x)
  }
  )

csv <- read.csv("~/Downloads/ML__2023-03-19T18-19_lobher_photo.csv")

View(csv)


csv$image_url <-
  paste0("https://cdn.download.ams.birds.cornell.edu/api/v1/asset/", csv$ML.Catalog.Number, "/1200")

csv$image_name <-
  paste0(csv$ML.Catalog.Number, ".jpeg")

pbapply::pblapply(1:nrow(csv), cl = 6, function(x){
  download.file(csv$image_url[x], csv$image_name[x], quiet = TRUE)
  # print(x)
}
)


#
https://search.macaulaylibrary.org/api/v1/search?mediaType=photo&taxonCode=mallar3
