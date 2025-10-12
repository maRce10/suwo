test_that("update query_wikiaves", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(term = 'Glaucis dohrnii', format =  "sound")

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1)- 3), ]

  up_df <- update_metadata(metadata = sub_df)

  expect_true(nrow(up_df) == nrow(df1))

})


test_that("update query_inaturalist", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(term = "Helicobacter pylori", format =  "image")

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1)- 3), ]

  up_df <- update_metadata(metadata = sub_df)

  expect_true(nrow(up_df) == nrow(df1))
})


test_that("update query_gbif", {

  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(term = 'Glaucis dohrnii', format =  "sound")

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1)- 3), ]

  up_df <- update_metadata(metadata = sub_df)

  expect_true(nrow(up_df) == nrow(df1))

})

test_that("update query_xenocanto", {

  skip_on_cran()
  skip_if_offline()
  skip_if_not(interactive())

  df1 <- query_xenocanto(term = 'Phaethornis anthophilus', all_data = FALSE, key = Sys.getenv("XENO_CANTO_API_KEY"))

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1)- 3), ]

  up_df <- update_metadata(metadata = sub_df, key = Sys.getenv("XENO_CANTO_API_KEY"))

  expect_true(nrow(up_df) == nrow(df1))

})

test_that("update query_macaulay", {

  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(term = 'Glaucis dohrnii', format =  "sound", path = tempdir())

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1)- 3), ]

  up_df <- update_metadata(metadata = sub_df, path = tempdir())

  expect_true(nrow(up_df) == nrow(df1))

})


test_that("update query_macaulay with paging", {

  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(term = 'Glaucis dohrnii', format =  "sound", path = tempdir(), dates = c(1979, 2022, 2026))

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1)- 3), ]

  up_df <- update_metadata(metadata = sub_df, path = tempdir())

  expect_true(nrow(up_df) == nrow(df1))

})
