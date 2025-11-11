test_that("search Glaucis dohrnii sound", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(species = 'Glaucis dohrnii', format =  "sound")

  skip_if(is.null(df1))

  expect_true(nrow(df1) >= 30)

})


test_that("search Glaucis dohrnii photos", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(species = 'Glaucis dohrnii', format = "image")

  skip_if(is.null(df1))

  expect_true(nrow(df1) >=  420)

})



test_that("no result", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(species = 'asdasdasd', format =  "image")

  expect_true(is.null(df1))

})


test_that("test verbose FALSE", {

  skip_on_cran()
  skip_if_offline()

  df1 <- capture_output(query_wikiaves(species = 'a3', format =  "sound",
                                       verbose = FALSE, pb = FALSE))

  skip_if(is.null(df1))

  expect_true(df1 == "")

})

test_that("test all_data FALSE", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(species = 'Glaucis dohrnii', format =  "image",
                        all_data = FALSE)

  skip_if(is.null(df1))

    expected_col_names <- .format_query_output(only_basic_columns = T)

  query_col_names <- colnames(df1)

  expect_true(all(query_col_names %in% expected_col_names) &
                all(expected_col_names %in% query_col_names),
              info = "Column names do not match the expected names")

})
