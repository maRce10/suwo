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
  if(!is.numeric(cores)) cores <- 1
  df1 <- query_gbif(term = 'Aristolochia baetica', type = "still image", cores = cores)

  expect_true(nrow(df1) >= 2003)

})


test_that("test verbose FALSE", {

  msg <- capture.output(a <- query_gbif(term = 'Glaucis dohrnii', type =  "still image", verbose = FALSE, pb = FALSE))

  expect_true(length(msg) == 0)

})

test_that("test all_data FALSE", {

  df1 <- query_gbif(term = 'Glaucis dohrnii', type =  "still image", all_data = FALSE)

  expected_col_names <- c("key", "species", "date", "country", "location", "latitude", "longitude", "file_url", "repository")
  query_col_names <- colnames(df1)
  expect_true(identical(query_col_names, expected_col_names), info = "Column names do not match the expected names")

})
