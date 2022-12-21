library(testthat)
test_that("search Glaucis dohrnii audio", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type =  "audio")


# system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) == 25)

})


test_that("search Spatula discors audio (no audios)", {

  df1 <- query_wikiaves(term = 'Spatula discors', type =  "audio")


  # system(paste("firefox", df1$link[1]))

  expect_true(is.null(df1))

})

test_that("search Glaucis dohrnii photos", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type =  "photo")


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >=  257)

})



test_that("no result", {

  df1 <- query_wikiaves(term = 'asdasdasd', type =  "photo")


  # system(paste("firefox", df1$link[1]))

  expect_true(is.null(df1))

})


test_that("search Glaucis photos (2 species)", {

  df1 <- query_wikiaves(term = 'Glaucis', type =  "photos")


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >=  77)

})


test_that("search Glaucis photos (2 species) in parallel", {

  df1 <- query_wikiaves(term = 'Glaucis', type =  "photos", cores = 3)


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >=  77)

})


test_that("test verbose FALSE", {

  df1 <- capture_output(query_wikiaves(term = 'a3', type =  "audio", verbose = FALSE, pb = FALSE))


  # system(paste("firefox", df1$link[1]))

  expect_true(df1 == "")

})
