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

  expect_true(nrow(df1) == 0 && ncol(df1) == 0)

})


test_that("search Glaucis photos (2 species) in parallel", {

  df1 <- query_inaturalist(term = 'bolitoglossa striatula', type =  "still image", cores = 2)

  expect_true(nrow(df1) >=  52)

})


test_that("test verbose FALSE", {

  df1 <- capture_output(query_inaturalist(term = 'Glaucis dohrnii', type =  "sound", verbose = FALSE))

  expect_true(df1 == "")

})

test_that("test all_data FALSE", {

  df1 <- query_inaturalist(term = 'bolitoglossa striatula', type =  "still image", all_data = FALSE)

  expected_col_names <- c("key", "species", "date", "country", "location", "latitude", "longitude", "file_url", "repository")
  query_col_names <- colnames(df1)
  expect_true(all(expected_col_names %in% query_col_names) && all(query_col_names %in% expected_col_names), info = "Column names do not match the expected names")

})
