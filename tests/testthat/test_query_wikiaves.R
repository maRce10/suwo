library(testthat)
test_that("search Glaucis dohrnii sound", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type =  "sound")


# system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >= 25)

})


test_that("search Spatula discors sound (no sounds)", {

  df1 <- query_wikiaves(term = 'Spatula discors', type =  "sound")


  # system(paste("firefox", df1$link[1]))

  expect_true(is.null(df1))

})

test_that("search Glaucis dohrnii photos", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type =  "still image")


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >=  257)

})



test_that("no result", {

  df1 <- query_wikiaves(term = 'asdasdasd', type =  "still image")


  # system(paste("firefox", df1$link[1]))

  expect_true(is.null(df1))

})


test_that("search Glaucis photos (2 species) in parallel", {

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', type =  "still image", cores = 2)


  # system(paste("firefox", df1$link[1]))

  expect_true(nrow(df1) >=  77)

})


test_that("test verbose FALSE", {

  df1 <- capture_output(query_wikiaves(term = 'a3', type =  "sound", verbose = FALSE, pb = FALSE))


  # system(paste("firefox", df1$link[1]))

  expect_true(df1 == "")

})
