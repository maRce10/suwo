library(testthat)
test_that("search Glaucis dohrnii audio", {

  df1 <- query_gbif(term = 'Glaucis dohrnii', type =  "sound")

  expect_true(nrow(df1) >= 22)

})

test_that("search Aristolochia baetica images", {

  df1 <- query_gbif(term = 'Glaucis dohrnii', type =  "still image")

        expect_true(nrow(df1) >= 30)

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
  cores <- if(!is.numeric(cores)) 1
  df1 <- query_gbif(term = 'Aristolochia baetica', type =  "still image", cores = cores)

  expect_true(nrow(df1) >= 2003)

})


test_that("test verbose FALSE", {

  msg <- capture_output(query_gbif(term = '000', verbose = FALSE))

  expect_true(msg == "")

})