test_that("update query_wikiaves", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(species = 'Glaucis dohrnii', format =  "sound")

  skip_if(is.null(df1))

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1) - 3), ]

  up_df <- update_metadata(metadata = sub_df)

  expect_true(nrow(up_df) == nrow(df1))

})


test_that("update query_inaturalist", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(species = "Helicobacter pylori", format =  "image")

  skip_if(is.null(df1))

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1) - 3), ]

  up_df <- update_metadata(metadata = sub_df)

  expect_true(nrow(up_df) == nrow(df1))
})


test_that("update query_gbif", {
  skip_on_cran()
  skip_if_offline()
  df1 <- query_gbif(species = 'Glaucis dohrnii', format =  "sound")

  skip_if(is.null(df1))
  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1) - 3), ]

  up_df <- update_metadata(metadata = sub_df)

  expect_true(nrow(up_df) == nrow(df1))

})

test_that("update query_xenocanto", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("XENO_CANTO_API_KEY")),
          "Xeno-Canto API key not set")

  df1 <- query_xenocanto(
    species = 'Phaethornis anthophilus',
    all_data = FALSE,
    api_key = Sys.getenv("XENO_CANTO_API_KEY")
  )

  skip_if(is.null(df1))

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1) - 3), ]

  up_df <- update_metadata(metadata = sub_df,
                           api_key = Sys.getenv("XENO_CANTO_API_KEY"))

  expect_true(nrow(up_df) == nrow(df1))

})

test_that("update query_macaulay", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(species = 'Glaucis dohrnii',
                        format =  "sound",
                        path = tempdir())

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1) - 3), ]

  up_df <- update_metadata(metadata = sub_df, path = tempdir())

  expect_true(nrow(up_df) == nrow(df1))

})


test_that("update query_macaulay with paging", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(
    species = 'Glaucis dohrnii',
    format =  "sound",
    path = tempdir(),
    dates = c(1979, 2022, 2026)
  )

  # remove last 3 rows to test update_metadata
  sub_df <- df1[1:(nrow(df1) - 3), ]

  up_df <- update_metadata(metadata = sub_df, path = tempdir(),
                           dates = c(1979, 2022, 2026))

  expect_true(nrow(up_df) == nrow(df1))

})
