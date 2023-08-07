library(testthat)
test_that("search Glaucis dohrnii audio", {

  df1 <- query_gbif(term = 'Glaucis dohrnii', type =  "sound")

  expect_true(nrow(df1) >= 22)

})

test_that("search Aristolochia baetica images", {

  df1 <- query_gbif(term = 'Aristolochia baetica', type =  "still image")

        expect_true(nrow(df1) >= 1456)

})


test_that("search Floractus heimi audio (no audios)", {

  df1 <- query_gbif(term = 'Floractus heimi', type =  "sound")

  expect_true(is.null(df1))

})

test_that("search Glaucis dohrnii photos", {

  df1 <- query_gbif(term = 'Glaucis dohrnii', type =  "still image")

  expect_true(nrow(df1) >=  29)

})



test_that("no result", {

  df1 <- query_gbif(term = 'asdasdasd', type =  "still image")

  expect_true(is.null(df1))

})


test_that("search Aristolochia baetica photos in parallel", {

  cores <- parallel::detectCores()
  cores <- if(cores > 2) 2
  df1 <- query_gbif(term = 'Aristolochia baetica', type =  "still image", cores = cores)

  expect_true(nrow(df1) >= 2003)

})


test_that("test verbose FALSE", {

  df1 <- capture_output(query_gbif(term = 'a3', type =  "sound", verbose = FALSE, pb = FALSE))

  expect_true(df1 == "")

})
