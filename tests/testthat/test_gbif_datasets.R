test_that("Test gbif dataset csv download", {

  df1 <- query_gbif(term = 'Glaucis dohrnii', type = "sound")

  test_keys <- c("3863342525", "3863345521")

  df1 <- subset(df1, key %in% test_keys)

  download_media(metadata = df1, path = tempdir())

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))


  expect_equal(fls, c("Glaucis_dohrnii-GBIF3863342525.mp3",
                      "Glaucis_dohrnii-GBIF3863345521.mp3"))
})
