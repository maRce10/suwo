test_that("search Glaucis dohrnii audio", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(term = 'Glaucis dohrnii', format =  "sound")

  expect_true(nrow(df1) >= 22)

})

test_that("search Aristolochia baetica images", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(term = 'Glaucis dohrnii', format =  "image")

  expect_true(nrow(df1) >= 30)

})


test_that("search Floractus heimi audio (no audios)", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(term = 'Floractus heimi', format =  "sound")

  expect_true(is.null(df1))

})

test_that("search Glaucis dohrnii photos", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(term = 'Glaucis dohrnii', format =  "image")

  expect_true(nrow(df1) >=  29)

})

test_that("search Glaucis dohrnii photos", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(term = 'Glaucis dohrnii', format =  "image", dataset = "b1047888-ae52-4179-9dd5-5448ea342a24")

  expect_true(nrow(df1) >=  22)

})



test_that("no result", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(term = 'asdasdasd', format =  "image")

  expect_true(is.null(df1))

})


test_that("search Aristolochia baetica photos in parallel", {

  skip_on_cran()
  skip_if_offline()

  cores <- parallel::detectCores()
  cores <- if(cores > 2) 2
  if(!is.numeric(cores)) cores <- 1
  df1 <- query_gbif(term = 'Aristolochia baetica', format = "image", cores = cores)

  expect_true(nrow(df1) >= 2003)

})


test_that("test verbose FALSE", {

  skip_on_cran()
  skip_if_offline()

  msg <- capture.output(a <- query_gbif(term = 'Glaucis dohrnii', format =  "image", verbose = FALSE, pb = FALSE))

  expect_true(length(msg) == 0)

})

test_that("test all_data FALSE", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(term = 'Glaucis dohrnii', format =  "image", all_data = FALSE)

  expected_col_names <- c("key", "species", "date", "country", "locality", "latitude", "longitude", "file_url", "repository", "file_extension", "time", "format")

  query_col_names <- colnames(df1)
  expect_true(all(expected_col_names %in% query_col_names) && all(query_col_names %in% expected_col_names), info = "Column names do not match the expected names")

})
