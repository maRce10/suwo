library(testthat)
test_that("search Glaucis dohrnii sound", {

  df1 <- query_inaturalist(term = "Helicobacter pylori", type =  "still image")

  expect_true(nrow(df1) >= 1)

})


test_that("search Spatula discors sound (no sounds)", {

  df1 <- query_inaturalist(term = 'Spatula discors', type =  "sound")

  expect_true(nrow(df1) >= 20)

})

test_that("search Glaucis dohrnii photos", {

  df1 <- query_inaturalist(term = 'Glaucis dohrnii', type =  "still image")

  expect_true(nrow(df1) >=  10)

})

test_that("no result", {

  df1 <- query_inaturalist(term = 'asdasdasd', type =  "still image")

  expect_true(is.null(df1))

})


test_that("search Glaucis photos (2 species) in parallel", {

  df1 <- query_inaturalist(term = '', type =  "sound", cores = 2)

  expect_true(nrow(df1) >=  725)

})


test_that("test verbose FALSE", {

  df1 <- capture_output(query_inaturalist(term = 'a3', type =  "sound", verbose = FALSE, pb = FALSE))

  expect_true(df1 == "")

})
